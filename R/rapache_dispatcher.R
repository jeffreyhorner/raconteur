app_path<-function(app){
	lapply(getOption('raconteur.app_path')
		function(p){
			if (file.exists(file.path(p,app)))
				return(file.path(p,app))
		}
	)
	NULL
}

is_raconteur_app <- function(app){
	path <- app_path(app)
	if (is.null(path)) return(FALSE)

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
	rapache:::.Rhttpd_response <- NULL
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
			response[[1]] <- .captured_output # if any
		response
	}
}

rhttpd_dispatch <- function(app=NULL,path=getOption('raconteur.app_path'),port=8181){

	if (is.null(app) && is.null(path))
		stop("One of app or path must be specified.")

	if (is.null(path)){
		old_app_path <- getOption('raconteur.app_path')
		on.exit(options('raconteur.app_path'=old_app_path))
		options('raconteur.app_path'='.')
	}

	options(help.ports=port)

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
			confirm_rttpd_response(router_env$router,path,query)
		}
	} else {
		# dispatch a collection of apps from path
		appcache <- list()
		dispatch_raconteur <- function(path, query ...){
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
			confirm_rttpd_response(app$router,sinartra_route,query)
		}
	}
	assignInNamespace("httpd", dispatch_raconteur, "tools")

	# Now start the R help web server with help.start. NOTE that
	# this merges with the R console's REPL loop, so if you shut
	# down R, you're shutting down the web server too.
	if (tools:::httpdPort == 0L) {
		help.start()
			#options("help_type" = "html")
	}

	cat('port is ',tools:::httpdPort,'\n')

	return(invisible(router_env$router))

}

rApache_appcache <- list()
cached_app <- function(app_name){
	app <- new.env(hash=TRUE)
	app$path <- app_path(app_name)
	app$routefile <- file.path(app$path,'routes.R');
	app$routefile_mtime <- as.integer(file.info(app$routefile)$mtime)
	app$router <- Router$clone()
	sys.source(app$routefile,app)
	app
}

rApache_handler <- function(){

	# First split the uri by "/", first element will always be "".
	uri <- strsplit(SERVER$path_info,"/")[[1]][-1]

	# bad uri
	if (length(uri) == 0){
		# TODO: return JSON object of all known apps
		setContentType("text/plain")
		cat(paste("URI is",SERVER$path_info))
		return(OK)
	}

	# First element of uri is app
	app <- NULL
	app_name <- uri[1]
	if (app_name %in% names(rApache_appcache)){
		app <- rApache_appcache[[app_name]]
	} else if (is_raconteur_app(app_name)){
		rApache_appcache[[app_name]] <<- cached_app(app_name)
		app <- appcache[[app_name]]
	} else {
		setContentType('text/html')
		cat("<h3>No app named",app_name,"<h3>\n")
		return(OK)
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
	ret <- app$router$route(sinartra_route, GET)
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
			sendBin(ret[[1]])
		}
		return(ifelse(ret[['status code']]==200,OK,ret[['status code']]))
	} else {
		# rApache response
		return(ret)
	}
}
