confirm_rhttpd_response <- function(router,path,query){

	# empty any previous rapache response
	rapache::reset_magic_vars()

	# need a better interface than this
	unlockBinding('GET',asNamespace('rapache'))
	assignInNamespace('GET', query, 'rapache')
	lockBinding('GET',asNamespace('rapache'))

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
				appcache[[app_name]] <<- create_cached_app(app_name)
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
