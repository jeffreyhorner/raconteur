app_name<-function(app){
	path <- app_path(app)
	if (file.exists(path)){
		x <- strsplit(path,.Platform$file.sep,fixed=TRUE)
		if (length(x) <= 0) return(NULL)
		x <- x[[1]]
		if (is.null(x) || length(x) <= 0) return(NULL)

		rev(x)[1]
	} else {
		NULL
	}
}
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

create_cached_app <- function(appname){
	app <- new.env(hash=TRUE)
	app$appname <- app_name(appname)
	app$path <- app_path(appname)
	app$routefile <- file.path(app$path,'routes.R');
	app$routefile_mtime <- as.integer(file.info(app$routefile)$mtime)
	app$router <- Router$clone()
	sys.source(app$routefile,app)
	app
}

upload_app <- function(url=NULL,app){
	path <- app_path(app)
}
