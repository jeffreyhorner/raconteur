library(sinartra)
library(evaluate) # sinartra dependency

# This might  be a useful pattern to add to sinartra
# will talk to HW.
start_sinartra <- function(app_path=getwd(),port=8181) {

	if (app_path!=getwd()){
		oldwd <- setwd(app_path)
		on.exit(setwd(oldwd))
	}

	# The sinartra package proper has essentially three
	# components:
	#
	# 1. Router object
	#   
	#    The user is expected to clone it like so:
	#
	#    router <- Router$clone()
	#
	#    and then add routes like so
	#
	#    router$get('/index.html',function(...){
	#         render_brew('index')
	#    })
	#
	#    Scope out ?Router or help(package='sinartra') for
	#    a brief overview.
	#
	# 2. route function
	#
	#    Sinartra doesn't define how this gets dispatched but
	#    it should be called by the web server to dispatch to
	#    the appropriate route.
	#
	#  3. the views directory
	#
	#    This is the only file system convention so far. Basically
	#    all views are stored there and with 

	# This is my convention for sinartra. Store all the routes,
	# e.g., route$get('/*',function(...){}) in own file.
	if (!file.exists('routes.R')){
		stop(paste("routes.R does not exist in",app_path))
	}

	# more sanity checks
	if (!file.exists('views')){
		stop(paste("views does not exist in",app_path))
		on.exit(setwd(oldwd))
	}

	# Recommend to R which port(s) to try. It could be more than one.
	options(help.ports=port)

	# My solution to managing the routes.R file
	router_env = new.env(hash=TRUE,parent=globalenv())
	router_env$router_mtime <- 0

	# From 1. above
	router_env$router <- Router$clone()

	# This is the function called by the R help server when a new
	# request comes in. As you can see belowthe route function
	# is called as the last expression, thus it must return the payload
	render_path <- function(path, query, ...){
		oldwd <- setwd(app_path)
		on.exit(setwd(oldwd))
		mtime = as.integer(file.info('routes.R')$mtime)
		if (mtime > router_env$router_mtime){
			sys.source('routes.R',router_env)
			router_env$router_mtime <- mtime
		}
		router_env$router$route(path, query)
	}

	# This is how you get the R help web server to call your
	# function. Once you do this, I'm not sure the HTML help system
	# will work again...
	assignInNamespace("httpd", render_path, "tools")

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
