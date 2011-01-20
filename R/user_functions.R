find_user_functions <- function(func_c, fun) {
	
	if(length(func_c) < 2) {
		c(func_c, user_functions(fun))
	} else {
		d <- func_c
		inners <- sapply(fun, "user_functions")
		for(i in seq_along(inners)) {
			item <- inners[[i]][1]
			if(length(item) > 0)
				d <- append_vector(d, inners[[i]][1])
		}
		d[!duplicated(d)]
	}	
}


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
	
	# 112       SYMBOL_PACKAGE     TRUE                   stringr
	# 113               NS_GET     TRUE                        ::
	# 114 SYMBOL_FUNCTION_CALL     TRUE               str_replace

	# 1       SYMBOL_PACKAGE     TRUE lubridate
	# 2           NS_GET_INT     TRUE       :::
	# 3 SYMBOL_FUNCTION_CALL     TRUE      days
	
	tmp_data <- attr(parsed, "data")[,c("token.desc", "text")]
	tkdc <- tmp_data$token.desc
	
	tkdc_len <- length(tkdc)
	tmp_data$token.desc_prev <- c(tkdc[tkdc_len], tkdc[-tkdc_len])
	tmp_data$token.desc_prev_prev <- c(tkdc[(tkdc_len-1):tkdc_len], tkdc[-((tkdc_len-1):tkdc_len)])
	# print(tmp_data)
	
	funcs <- subset(tmp_data, (token.desc == "SYMBOL_FUNCTION_CALL") & ((token.desc_prev != "NS_GET_INT") | (token.desc_prev != "NS_GET")) & (token.desc_prev_prev != "SYMBOL_PACKAGE"), select = "text")
	funcs$user_func <- sapply(funcs$text, is_user_function)
	
	user_funcs <- subset(funcs, user_func == TRUE, select = "text")$text

	inner_funcs <- c(sapply(user_funcs, user_functions, simplify = FALSE, USE.NAMES = FALSE))
	
	temp_all_funcs <- as.vector(c(user_funcs, inner_funcs))
	
	if(length(temp_all_funcs) < 1)
		return(NULL)
	
	all_funcs <- c()
	for(i in seq_along(temp_all_funcs)) {
		for(j in seq_along(temp_all_funcs[[i]])) {
			all_funcs <- append_vector(all_funcs, temp_all_funcs[[i]][j])
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
