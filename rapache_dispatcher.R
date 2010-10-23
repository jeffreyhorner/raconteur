library(RJSONIO)
library(evaluate) # sinartra broken dependency
library(sinartra)

setwd(RACONTEUR_ROOT)

appcache <- list()

is_raconteur_app <- function(app){
	app_path <- file.path(RACONTEUR_ROOT,app)

	if (!file.exists(app_path)) return(FALSE)

	if (!file.exists(file.path(app_path,'routes.R')))
		return(FALSE)

	# more sanity checks
	if (!file.exists(file.path(app_path,'views')))
		return(FALSE)

	TRUE
}

new_raconteur_app <- function(app_name){
	app <- new.env(hash=TRUE)
	app$path <- file.path(RACONTEUR_ROOT,app_name)
	app$routefile <- file.path(app$path,'routes.R');
	app$routefile_mtime <- as.integer(file.info(app$routefile)$mtime)
	app$router <- Router$clone()
	sys.source(app$routefile,app)
	app
}

dispatch <- function(){

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
	if (app_name %in% names(appcache)){
		app <- appcache[[app_name]]
	} else if (is_raconteur_app(app_name)){
		appcache[[app_name]] <<- new_raconteur_app(app_name)
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
	app$router$route(sinartra_route, GET)
}
