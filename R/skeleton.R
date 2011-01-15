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
app.skeleton <- function(func, overwrite = TRUE) {
	
	# Set up parameters
	func_c <- as.character(substitute(func))
	base_dir <- valid_directory(func_c, overwrite)
	
	# Create the dir
	make_app_dir(base_dir)
		
	# Create README
	make_index_html(base_dir)
	
	# Save functions to file
	make_function_file(base_dir, func_c, func)
	
	# Create route to main function
	make_routes_r(base_dir, func_c)
	
	# Create the views dir
	make_app_dir(file.path(base_dir, "views"))
	
	# Create output view
	make_view_output_html(file.path(base_dir, "views"))
	# make_view_interaction_html(file.path(base_dir, "views"))
	
	invisible(func)
}

