#' Load an html page from the console.
#' 
#' @param ... site to be loaded
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @export
load_html <- function(...) {
  url_path <- str_c(as.character(substitute(...)), collapse = "/")

  if (str_sub(url_path, end = 1) != "/") {
    url_path <- str_c("/", url_path, collapse = "")
  }

  if (str_sub(url_path, end = 2) == "//") {
    url_path <- str_sub(url_path, start = 2)
  }
  
  browseURL(paste(base_html_path(), url_path, sep = ""), getOption("browser"))
}


#' Find good directory name
#'
#' @param name name of wanted directory
#' @param overwrite boolean determining if the name should be appended
valid_directory <- function(name, overwrite) {
	base_dir <- name
	if(!overwrite) {
		folder_items <- dir()
		count <- 1
		while(base_dir %in% folder_items) {
			base_dir <- str_c(name, "-", count)
			count <- count + 1
		}
	}
	base_dir
}


#' Extract source code of a function.
#'
#' @param fun name of function to get the source code from
#' @return NULL or source code of of the function
#' @author Barret Schloerke \email{schloerke@@gmail.con} and Hadley Wickham
#' @keywords internal
body_text <- function(func_c) {
	print(func_c)
	text <- get_function(func_c)
	
	if (is.null(text)) {
		NULL
	} else {
		str_c(func_c, " <- ", str_c(deparse(text), collapse = "\n"))
	}
}




#' Get Function Body
#' 
#' @param func_c character form of the function
get_function <- function(func_c) {
	val <- tryCatch(
		get(func_c, mode = "function"),
		error = function(e) {
			"error"
		}
	)

	if(!identical(val, "error"))
		return(val)
	
	tryCatch(
		get(func_c, envir = globalenv()),
		error = function(e) {
			NULL
		}
	)
}


#' Print line to file using cat
#'
#' @param ... args sent to cat
#' @param file_path path to file being used
#' @param append boolen to append file or overwrite file
print_line <- function(..., file_path, append = TRUE) {
	cat(..., "\n", file = file_path, append = append)
}