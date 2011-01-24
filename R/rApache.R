rApache_root_url <- function(){
    if (!is.null(getOption('raconteur.root_url')))
	getOption('raconteur.root_url')
	else if (SERVER$path_info == '')
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
	appname <- ifelse(length(uri)>0,uri[1],NULL)
	if (appname %in% names(rApache_appcache)){
		app <- rApache_appcache[[appname]]
	} else if (is_raconteur_app(appname)){
		rApache_appcache[[appname]] <<- create_cached_app(appname)
		app <- rApache_appcache[[appname]]
	} else {
		msg <- sprintf("<h2>Error! No app named %s.</h2>",appname)
		return(rApache_splash_screen(msg))
	}

	oldwd <- setwd(app$path)
	on.exit(setwd(oldwd))

	mtime = as.integer(file.info(app$routefile)$mtime)
	if (mtime > app$routefile_mtime){
		sys.source(app$routefile,app)
		app$routefile_mtime <- mtime
		rApache_appcache[[appname]] <<- app
	}

	# Trim /appname from path_info
	sinartra_route <- sub(paste('^/',app$appname,sep=''),'',SERVER$path_info)

	# Execute route and capture all leaky output to a textConnection
	con <- textConnection('.captured_output',open='w')
	sink(con)
	ret <- try(app$router$route(sinartra_route, GET))
	sink()
	close(con)

	if (inherits(ret,'try-error')){
		cat('rApache_handler returned a "try-error"\n',file=stderr())
		cat(ret,"\n",file=stderr())
	} else if (is.list(ret)){
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
	} else if (is.character(ret) && grepl('^ERROR:',ret[1])) {
		cat('sinartra returned an ERROR:\n',file=stderr())
		cat(ret,"\n",file=stderr())
	} else {
		# rApache response
		if (length(.captured_output)>0)
			cat(paste(paste(.captured_output,collapse="\n"),"\n",sep=""))
		return(ret)
	}
}

rApache_admin_app <- NULL
rApache_admin_handler <- function(){
    app <- getOption('raconteur.admin_app')
    if (!is_raconteur_app(app))
	return(HTTP_BAD_REQUEST)

    # lame that we have to do this
    rapache::reset_magic_vars()

    if (is.null(rApache_admin_app))
	rApache_admin_app <<- create_cached_app(app)
    app <- rApache_admin_app

    oldwd <- setwd(app$path)
    on.exit(setwd(oldwd))

    mtime = as.integer(file.info(app$routefile)$mtime)
    if (mtime > app$routefile_mtime){
	sys.source(app$routefile,app)
	app$routefile_mtime <- mtime
	rApache_admin_app <<- app
    }

    # Trim /appname from path_info
    cat("appname",app$appname,file=stderr())
    cat('path_info',SERVER$path_info,'\n',file=stderr())
    sinartra_route <- sub(paste('/',app$appname,sep=''),'',SERVER$path_info)
    cat('route',sinartra_route,'\n',file=stderr())

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
	if (ret[['status code']] == 200)
	    return(OK)
	else 
	    return(ret[['status code']])
    } else {
	# rApache response
	if (length(.captured_output)>0)
	    cat(paste(paste(.captured_output,collapse="\n"),"\n",sep=""))
	    return(ret)
    }
}
