app_path<-function(app){
	found_path <- NULL
	lapply(getOption('raconteur.app_path'),
		function(p){
			if (is.null(found_path) && file.exists(file.path(p,app)))
				found_path <<- file.path(p,app)
		}
	)

	# Maybe app is not found on the raconteur.app_path search path
	# So we test if app exists on the file system and presume it's
	# a valid app path.
	if (is.null(found_path) && file.exists(app))
		app
	else
		found_path
}

is_raconteur_app <- function(app){

	# First see if app is a dir under getOption(raconteur.app_path)
	#  If not, see if app is a path in it's own right.
	path <- app_path(app)
	if (is.null(path)) {
		path <- app
	}

	if (!file.exists(file.path(path,'routes.R')))
		return(FALSE)

	# more sanity checks
	if (!file.exists(file.path(path,'views')))
		return(FALSE)

	TRUE
}

confirm_rhttpd_response <- function(router,path,query){
	# empty any previous rapache response
	unlockBinding('.Rhttpd_response',asNamespace('rapache'))
	assignInNamespace(".Rhttpd_response", NULL, 'rapache')
	lockBinding('.Rhttpd_response',asNamespace('rapache'))

	# Execute route and capture all leaky output to a textConnection
	con <- textConnection('.captured_output',open='w')
	sink(con)
	ret <- router$route(path,query)
	sink()
	close(con)

	# Classic httpd response is a list, just return it for now
	if (is.list(ret)) return(ret)

	# otherwise we need to peek into rapache:::.Rhttpd_response

	# Pathological respone or route blew up!
	if (is.null(rapache:::.Rhttpd_response)) {
		return(list(payload='error','text/plain',c(),500))
	} else {
		# Presume it was embellished at this point but we need
		# to embellish more.
		response <- rapache:::.Rhttpd_response
		response['status code'] <- ret
		if (is.null(response[[1]]))
			response[[1]] <- paste(paste(.captured_output,collapse="\n"),"\n",sep="")
		response
	}
}

rhttpd_dispatch <- function(app=NULL,path=getOption('raconteur.app_path'),port=8181){

	if (is.null(app) && is.null(path))
		stop("One of app or path must be specified.")


	if (!is.null(app)){
		if (!is_raconteur_app(app)) stop(app,"not a raconteur app!")

		router_env = new.env(hash=TRUE,parent=globalenv())
		router_env$router_mtime <- 0
		router_env$router <- Router$clone()

		dispatch_raconteur <- function(path, query, ...){
			oldwd <- setwd(app_path(app))
			on.exit(setwd(oldwd))
			mtime = as.integer(file.info('routes.R')$mtime)
			if (mtime > router_env$router_mtime){
				sys.source('routes.R',router_env)
				router_env$router_mtime <- mtime
			}
			confirm_rhttpd_response(router_env$router,path,query)
		}
	} else {
		# dispatch a collection of apps from path
		appcache <- list()
		dispatch_raconteur <- function(path, query, ...){
			cat("Request: uri is ",path,"\n")
			# First split the uri by "/", first element will always be "".
			uri <- strsplit(path,"/")[[1]][-1]

			# bad uri
			if (length(uri) == 0){
				# TODO: return JSON object of all known apps
				return(list(payload='error','text/plain',c(),500))
			}

			# First element of uri is app
			app <- NULL
			app_name <- uri[1]
			if (app_name %in% names(appcache)){
				app <- appcache[[app_name]]
			} else if (is_raconteur_app(app_name)){
				appcache[[app_name]] <<- cached_app(app_name)
				app <- appcache[[app_name]]
			} else {
				return(list(payload='error','text/plain',c(),500))
			}

			oldwd <- setwd(app$path)
			on.exit(setwd(oldwd))

			mtime = as.integer(file.info(app$routefile)$mtime)
			if (mtime > app$routefile_mtime){
				cat("refreshing routes.R from ",app_name,"\n",file=stderr())
				sys.source(app$routefile,app)
				app$routefile_mtime <- mtime
			}

			# Trim /app_name from path_info
			sinartra_route <- sub(paste('^/',app_name,sep=''),'',path)
			confirm_rhttpd_response(app$router,sinartra_route,query)
		}
	}

	# Now start the R help web server. NOTE that
	# this merges with the R console's REPL loop, so if you shut
	# down R, you're shutting down the web server too.
	# Already running when httpdPort non zero. Stop/Start if our
	# port is different than what we want.
	assignInNamespace("httpd", dispatch_raconteur, "tools")
	options(help.ports=port)
	if (tools:::httpdPort != port && tools:::httpdPort != 0L)
		tools:::startDynamicHelp(start=FALSE)
	tools:::startDynamicHelp()

	if (!is.null(app))
		invisible(router_env$router)
	else
		invisible(appcache)
}

cached_app <- function(app_name){
	app <- new.env(hash=TRUE)
	app$path <- app_path(app_name)
	app$routefile <- file.path(app$path,'routes.R');
	app$routefile_mtime <- as.integer(file.info(app$routefile)$mtime)
	app$router <- Router$clone()
	sys.source(app$routefile,app)
	app
}

rApache_root_url <- function(){
	if (SERVER$path_info == '')
		SERVER$uri
	else 
		sub(paste(SERVER$path_info,'$',sep=''),'',SERVER$uri)
}

rApache_app_url <- function(app) paste(rApache_root_url(),app,sep='/')

rApache_splash_screen <- function(msg=NULL){
	setContentType('text/html')
	if (!is.null(msg)) cat(msg)

	any_paths <- NULL
	lapply(getOption('raconteur.app_path'),
		function(p){
			if (is.null(any_paths)){
				cat("<h2>Try one of these apps</h2>")
				any_paths <<- TRUE
			}
			for (i in dir(p)){
				cat(sprintf('<a href="%s/">%s</a><br>\n',rApache_app_url(i),i))
			}
		}
	)
	return(OK)
}

rApache_appcache <- list()
rApache_handler <- function(){
	# lame that we have to do this
	rapache::reset_magic_vars()

	# First split the uri by "/", first element will always be "".
	uri <- strsplit(SERVER$path_info,"/")[[1]][-1]

	# bad uri
	if (length(uri) == 0) return(rApache_splash_screen())

	# First element of uri is app
	app <- NULL
	app_name <- ifelse(length(uri)>0,uri[1],NULL)
	if (app_name %in% names(rApache_appcache)){
		app <- rApache_appcache[[app_name]]
	} else if (is_raconteur_app(app_name)){
		rApache_appcache[[app_name]] <<- cached_app(app_name)
		app <- rApache_appcache[[app_name]]
	} else {
		msg <- sprintf("<h2>Error! No app named %s.</h2>",app_name)
		return(rApache_splash_screen(msg))
	}

	oldwd <- setwd(app$path)
	on.exit(setwd(oldwd))

	mtime = as.integer(file.info(app$routefile)$mtime)
	if (mtime > app$routefile_mtime){
		cat("refreshing routes.R from ",app_name,"\n",file=stderr())
		sys.source(app$routefile,app)
		app$routefile_mtime <- mtime
	}

	# Trim /app_name from path_info
	sinartra_route <- sub(paste('^/',app_name,sep=''),'',SERVER$path_info)

	# Execute route and capture all leaky output to a textConnection
	con <- textConnection('.captured_output',open='w')
	sink(con)
	ret <- app$router$route(sinartra_route, GET)
	sink()
	close(con)

	if (is.list(ret)){
		# Translate Rhttpd response object to rApache response
		setContentType(ret[['content-type']])
		if (!is.null(ret$headers)){
			lapply(strsplit(ret$headers,': '),function(i) setHeader(i[1],i[2]))
		}
		if ('file' %in% names(ret)){
			# payload is a file
			sendBin(readBin(ret$file,'raw',n=file.info(ret$file)$size))
		} else{
			capture.output(str(ret),file=stderr())
			if (is.character(ret[[1]]))
				cat(ret[[1]])
			else if (is.raw(ret[[1]]))
				sendBin(ret[[1]])
		}
		return(ifelse(ret[['status code']]==200,OK,ret[['status code']]))
	} else {
		# rApache response
		if (length(.captured_output)>0)
			cat(paste(paste(.captured_output,collapse="\n"),"\n",sep=""))
		return(ret)
	}
}
