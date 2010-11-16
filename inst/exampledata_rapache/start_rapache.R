library(sinartra)
library(evaluate) # sinartra dependency

app_path <- '/home/hornerj/metamx/sinartra_rapache'

setwd(app_path)

if (!file.exists('routes.R')){
	stop(paste("routes.R does not exist in",app_path))
}

# more sanity checks
if (!file.exists('views')){
	stop(paste("views does not exist in",app_path))
	on.exit(setwd(oldwd))
}

router_env <- new.env(hash=TRUE)
router_env$router <- Router$clone()
router_env$router_mtime <- 0

handler <- function(){

	if (app_path!=getwd()){
		oldwd <- setwd(app_path)
		on.exit(setwd(oldwd))
	}


	mtime = as.integer(file.info('routes.R')$mtime)
	if (mtime > router_env$router_mtime){
		sys.source('routes.R',router_env)
		router_env$router_mtime <- mtime
	}

	router_env$router$route(SERVER$path_info, GET)
}
