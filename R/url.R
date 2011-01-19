#' Cut and trim a string
#'
#' @param string string in question
#' @param start please see \link[string]{\code{str_sub}}
#' @param end please see \link[string]{\code{str_sub}}
#' @exmaples cut_and_trim_string("1 2 3 4 5 6 7 8 9", 2, 5) # "2 3"
cut_and_trim_string <- function(string, start, end) {
	str_trim(str_sub(string, start = start, end = end))
}

#' As character dot dots
#'
#' @param ... text to subsitute
as_char <- function(...) {
	as.character(substitute(...))
}


func_args <- function(...) {
	func_c <- as_char(...)
	func_body <- body_text(func_c)
	

	# find all commas, equals, and the start and end positions of the parenthesis
	start_paren <- end_paren <- counter_paren <- 0L
	pos_comma <- pos_equal <- c()
	chars <- str_split(func_body, "")[[1]]

	for (i in seq_len(str_length(func_body)) ) {
		char <- chars[i]
		
		if (counter_paren == 1){
			if (identical(char, "=")) {
				pos_equal <- append_vector(pos_equal, i)
			} else if(identical(char, ",")) {
				pos_comma <- append_vector(pos_comma, i)
			} 
		} 

		if (identical(char, "(")) {
			if (counter_paren == 0L) {
				start_paren <- i
			}
			counter_paren <- counter_paren + 1L
		} else if (identical(char, ")")) {
			counter_paren <- counter_paren - 1L
			if (counter_paren == 0L) {
				end_paren <- i
				break;
			}
		}
	}

	# if((end_paren - start_paren) == 1){
	# 	return(list(name = func_c, args = c()))
	# }
	
	args <- list(name = c(), default = c())
	# go through each argument and append url string
	# print(list(s = start_paren, c = pos_comma, e = pos_equal, e = end_paren, t = func_body))
	if ((end_paren - start_paren) > 1) {
		arg_boundries <- c(start_paren, pos_comma, end_paren)
		for (i in seq_len(length(arg_boundries) - 1) ) {
			start <- arg_boundries[i]
			end <- arg_boundries[i + 1]
				
			eq <- pos_equal[pos_equal < end & pos_equal > start]
			if(length(eq)) {
				args$name[i] <- cut_and_trim_string(func_body, start, eq - 2)
				args$default[i] <- cut_and_trim_string(func_body, eq+1, end - 2)
			} else {
				args$name[i] <- cut_and_trim_string(func_body, start, end - 2)
				args$default[i] <- NULL
			}
		}
	}
		
	as.data.frame(args, stringsAsFactors = FALSE)
}


#' Convert a function to a url string
#'
#' @param ... function name to be used.  Can be typed without quotes
func_to_url <- function(..., url="", unknown = "<obj>") {
	name <- as_char(...)
	args <- func_args(...)
	# print(args)
	args <- subset(args, name != "...")
	
	url_string <- name
	if(NROW(args) < 1){
		return(url_string)
	}
	
	# go through each argument and append url string
	url_string <- str_c(url_string, "?")
	for (i in seq_len(NROW(args)) ) {
		
		if(i > 1) {
			url_string <- str_c(url_string, "&")
		}
		
		n <- args$name[i]
		d <- args$default[i]
		if(length(d) < 1) d <- unknown
		if(is.na(d) | is.null(d)) d <- unknown
		
		url_string <- str_c(url_string, n, "=", d)
	}
	
	if(str_length(url) < 1){
		url_string
	} else {
		if(str_sub(url, start = -1) != "/") {
			url <- str_c(url, "/")
		}
		
		str_c(url, url_string)
	}
}

