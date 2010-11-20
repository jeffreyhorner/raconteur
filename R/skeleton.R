

#' Make directory or notify
#'
#' @param name name of folder to be created
make_app_dir <- function(name) {
	if(! identical(file.info(name)$isdir, TRUE))
		dir.create(name)
	else if(identical(file.info(name)$isdir, TRUE))
		message("Overwriting directory ", name)
}


#' Make Function File
#' Make a file for the functions to be housed in.
make_function_file <- function(base_dir, func_c, fun) {

	# Make file
	file_path <- file.path(base_dir, str_c(func_c, ".R"))

	# Save original function
	func_body <- body_text(func_c)
	cat(func_body, file = file_path)
	
	# Add all inner functions to the file
	innerFunctions <- user_functions(fun)
	for(innerFunc in innerFunctions) {
		cat("\n\n", file = file_path, append = TRUE)
		cat(body_text(innerFunc), file = file_path, append = TRUE)
	}
	
}

#' Make a readme file
#' readme file that explains how the routes and views work together.  Also tells what needs to be filled in.
#' 
#' @param path path to folder where the readme should be placed
make_readme <- function(path) {
	content <- "hi!\n this is a readme file.\n\n\nThis is so cool!"
	
	cat(content, file = file.path(path, "Read-and-delete-me"))
}

#' All user defined inner functions
#' Find all user defined functions that are used within the given function (recursively)
#'
#' @param fun function in question
#' @examples user_functions(app.skeleton)
user_functions <- function(fun) {
	parsed <- c()
	if(is.character(fun)) {
		if(!is.null(get_function(fun))) 
			parsed <- eval(parse(text = str_c("parser:::parser(",fun,")")))
		else 
			return(NULL)
	} else {
		parsed <- parser:::parser(fun)
	}
	
	funcs <- subset(attr(parsed, "data"), token.desc == "SYMBOL_FUNCTION_CALL", select = "text")
	funcs$user_func <- sapply(funcs$text, is_user_function)
	
	user_funcs <- subset(funcs, user_func == TRUE, select = "text")$text
	
	inner_funcs <- c(sapply(user_funcs, user_functions, simplify = FALSE, USE.NAMES = FALSE))
	
	temp_all_funcs <- as.vector(c(user_funcs, inner_funcs))
	
	if(length(temp_all_funcs) < 1)
		return(NULL)
	
	all_funcs <- c()
	all_funcs_pos <- 1
	for(i in seq_along(temp_all_funcs)) {
		for(j in seq_along(temp_all_funcs[[i]])) {
			all_funcs[all_funcs_pos] <- temp_all_funcs[[i]][j]
			all_funcs_pos <- all_funcs_pos + 1
		}
	}
	
	all_funcs <- all_funcs[!is.null(all_funcs)]
	all_funcs <- all_funcs[!is.na(all_funcs)]
	all_funcs[!duplicated(all_funcs)]
}

#' Is user function
#' Determines if the function is user made or located in an environment
#' 
#' @param func_c function in question
is_user_function <- function(func_c) {
	
	bod <- capture.output(get_function(func_c))
	if(identical(bod,"NULL"))
		return(FALSE)

	lastLine <- bod[length(bod)] # get the line that talks about the envir
	
	!any(str_detect(bod, ".Primitive")) && !(str_detect(lastLine, "environment") && str_detect(lastLine, "namespace"))
}



#' Make routes.R
#' Make a routes.R file that contains the main function to route and source for every function
#'
#' @param path path of folder were the file is to be saved
#' @param func_c character form of the function to be in main route
make_routes <- function(path, func_c) {
	file_path <- file.path(path, "routes.R")
	
	print_line("# routes.R - contains two default routes", file_path = file_path, append = FALSE)
	print_line("# 1. execute function - htmlpath/myFunc?arg1=\"A\"&arg2=5", file_path = file_path)
	print_line("# 2. view source of function - htmlpath/myFunc/source", file_path = file_path)
	
	print_line("", file_path = file_path);print_line("", file_path = file_path)
	
	print_line("# load_html(\"/\",func_c,\"?arg1=foo&arg2=bar\")", file_path = file_path)
	print_line("# Execute Function", file_path = file_path)
	print_line("router$get(\"/:func\", function(func, query, ...) {", file_path = file_path)
	print_line("	call <- str_c(func, \"(\",str_c(names(query), str_c(\"\\\"\", query, \"\\\"\"), sep = \" = \", collapse = \", \") ,\")\")", file_path = file_path)
	print_line("	output <- eval(parse(text = call))", file_path = file_path)
	print_line("	", file_path = file_path)
	print_line("	# brews the file output.html in the /views dir", file_path = file_path)
	print_line("	render_brew(\"output\",list(header = \"Output\", output = output))", file_path = file_path)
	print_line("})", file_path = file_path)
	
	print_line("", file_path = file_path);print_line("", file_path = file_path)
	
	print_line("# load_html(\"/\",func_c,\"/source\")", file_path = file_path)
	print_line("# View Source", file_path = file_path)
	print_line("router$get(\"/:func/source\", function(func, query, ...) {", file_path = file_path)
	print_line("	output <- body_text(func)", file_path = file_path)
	print_line("	", file_path = file_path)
	print_line("	# brews the file output.html in the /views dir", file_path = file_path)
	print_line("	render_brew(\"output\",list(header = \"Source\", output = output))", file_path = file_path)
	print_line("})", file_path = file_path)
	print_line("", file_path = file_path)
}


make_view <- function(path) {
	
	file_path <- file.path(path, "output.html")
	
	print_line("", file_path = file_path, append = FALSE)
	
	
	print_line("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">", file_path = file_path)
	print_line("<html xmlns=\"http://www.w3.org/1999/xhtml\">", file_path = file_path)
	print_line("<head>", file_path = file_path)
	print_line("  <title><%= header %></title>", file_path = file_path)
	print_line("</head>", file_path = file_path)
	print_line("<body>", file_path = file_path)
	print_line("	<%= output %>", file_path = file_path)
	print_line("</body>", file_path = file_path)
	print_line("</html>", file_path = file_path)
	
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
	make_readme(base_dir)
	
	# Save functions to file
	make_function_file(base_dir, func_c, func)
	
	# Create route to main function
	make_routes(base_dir, func_c)
	
	# Create the views dir
	make_app_dir(file.path(base_dir, "views"))
	
	# Create output view
	make_view(file.path(base_dir, "views"))
	
	invisible(func)
}

