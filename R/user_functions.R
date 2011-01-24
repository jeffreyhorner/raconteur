#' Make Function File
#' Make a file for the functions to be housed in.
add_functions_to_routes_r <- function(file_path, func_c, fun) {
	
	# Save original function
	inners <- find_user_functions(func_c, fun)
cat_e(str_c(inners, collapse =", "), "\n")
	# Add all inner functions to the file
	for(innerFunc in inners) {
		print_line("\n\n", file_path = file_path)
		print_line(body_text(innerFunc), file_path = file_path)
	}
	
}


find_user_functions <- function(func_c, fun) {
	
	if(length(func_c) < 2) {
		c(func_c, user_functions(fun))
	} else {
		d <- func_c
		inners <- sapply(fun, "user_functions")

		for(i in seq_along(inners)) {
			items <- inners[[i]]
			for(j in seq_along(items))
				d <- append_vector(d, inners[[i]][j])
		}
		d[!duplicated(d)]
	}	
}


#' All user defined inner functions
#' Find all user defined functions that are used within the given function (recursively)
#'
#' @param fun function in question
#' @examples user_functions(app.skeleton)
user_functions <- function(fun) {
	is.local <- function(x) exists(x, globalenv(), inherits = FALSE)
	user_funcs <- Filter(is.local, findGlobals(match.fun(fun), merge = FALSE)$functions)
	
	user_funcs <- user_funcs[!duplicated(user_funcs)]
	if(typeof(fun) != "closure") {
		user_funcs <- user_funcs[! user_funcs %in% fun]
	}
	user_funcs <- user_funcs[!is.null(user_funcs)]
	user_funcs <- user_funcs[!is.na(user_funcs)]

	if(length(user_funcs) < 1) return(NULL)

	inner_funcs <- c(sapply(user_funcs, user_functions, simplify = FALSE, USE.NAMES = FALSE))

	temp_all_funcs <- as.vector(c(user_funcs, inner_funcs))

	all_funcs <- c()
	for(i in seq_along(temp_all_funcs)) {
		for(j in seq_along(temp_all_funcs[[i]])) {
			all_funcs <- append_vector(all_funcs, temp_all_funcs[[i]][j])
		}
	}
	if(length(all_funcs) < 1) return(NULL)

	all_funcs <- all_funcs[!is.null(all_funcs)]
	all_funcs <- all_funcs[!is.na(all_funcs)]
	all_funcs[! duplicated(all_funcs)]
	
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
