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
		
	# Create/Clear Route File
	routes_path <- file.path(func_c, "routes.R")
	print_line("", file_path = routes_path, append = FALSE)
	
	# Create route to main function
	make_routes_r(routes_path, func_c)

	# Save functions to file	
	add_functions_to_routes_r(routes_path, func_c, func)
	
	# Create the views dir
	make_app_dir(file.path(base_dir, "views"))
	
	# Create README
	make_index_html(file.path(base_dir, "views"), func_c)
	
	# Create output view
	make_view_output_html(file.path(base_dir, "views"))
	# make_view_interaction_html(file.path(base_dir, "views"))
	
	invisible(func)
}

