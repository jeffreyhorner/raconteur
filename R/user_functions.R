#' Make Function File
#' Make a file for the functions to be housed in.
add_functions_to_routes_r <- function(path, func_c, fun) {
	
	# Make file path
	file_path <- file.path(path, "routes.R")
	
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
	
	file.remove(attr(parsed, "file"))
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
