#' Make directory or notify
#'
#' @param name name of folder to be created
make_app_dir <- function(name) {
	if(! identical(file.info(name)$isdir, TRUE))
		dir.create(name)
	else if(identical(file.info(name)$isdir, TRUE))
		message("Overwriting directory ", name)
}


#' Application Skeleton
#'
#' @param func function to be made into a skeleton
#' @examples app.skeleton(rnorm)
#' 		app.skeleton(rnorm) # telling you it's overwriting it
#' 		app.skeleton(rnorm, overwrite = FALSE)
#' 		p <- function(...) plot(...)
#' 		u <- function(...) runif(...)
#' 		n <- function(...) rnorm(...)
#' 		app.skeleton(c(p, u, n), dir

app.skeleton <- function(func, dir_name, overwrite = TRUE) {
	
	# Set up parameters
	func_c <- as.character(substitute(func))
	if(length(func_c) > 1) {
		if(func_c[1] == "c"){
			func_c <- func_c[-1]
			# func <- func[-1]
		}
	}
	
	if(missing(dir_name)) {
		# only one func supplied, making sure dir_name is supplied
		dir_name <- func_c[[1]]
	}
	first_func <- func_c[[1]]
	
	base_dir <- valid_directory(dir_name, overwrite)
	
	# Create the dir
	make_app_dir(base_dir)
		
	# Create/Clear Route File
	routes_path <- file.path(dir_name, "routes.R")
	print_line("", file_path = routes_path, append = FALSE)
	
	# Create route to main function
	make_routes_r(routes_path, first_func)

	# Save functions to file	
	for(i in seq_along(func_c)) {
		print(func_c[i])
		add_functions_to_routes_r(routes_path, func_c[[i]], func[[i]])
	}

	
	# Create the views dir
	make_app_dir(file.path(base_dir, "views"))
	
	# Create README
	make_index_html(file.path(base_dir, "views"), first_func)
	
	# Create output view
	make_view_output_html(file.path(base_dir, "views"))
	# make_view_interaction_html(file.path(base_dir, "views"))
	
	invisible(func)
}

