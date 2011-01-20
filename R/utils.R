#' Base html path needed to load a website.
#' 
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
base_html_path <- function() {
  str_c("http://127.0.0.1:", tools:::httpdPort, collapse = "")
}

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
	rac_path <- options()$raconteur.app_path
	if(!is.null(rac_path)) {
		base_dir <- file.path(rac_path, name)
	}
	
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
body_text <- function(func_c, html_view=FALSE) {
	# print(func_c)
	if(missing(func_c)){
		stop("Please supply a function name")
	}
	
	text <- get_function(func_c)
	
	# output <- NULL
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
	tryCatch(
		get(func_c, mode = "function"),
		error = function(e) {
			tryCatch(
				get(func_c, envir = globalenv()),
				error = function(e) {
					tryCatch(
						get(func_c),
						error = function(e) {
							NULL
						}
					)
				}
			)
		}
	)
}


#' Print line to file using cat
#'
#' @param ... args sent to cat
#' @param file_path path to file being used
#' @param append boolen to append file or overwrite file
print_line <- function(..., file_path, append = TRUE) {
	cat(..., "\n", sep = "", file = file_path, append = append)
}


#' Temporary File with an Extension
#'
#' @param ext extension to be used
tempfileWithExtension <- function(ext = "png") { 
	str_c(tempfile(), ".", ext)
}


#' Make a Drop Down Menu
#'
#' @param l list with one item and a collection inside
#' @examples html_drop_down(list(listTitle = c("A", "B", "C")))
html_drop_down <- function(l, displayName = TRUE) {
	options <- stringr::str_c("<option value=\"", l[[1]], "\">", l[[1]], "</option>", collapse = "\n")
	dd <- stringr::str_c("<select id=\"", names(l)[1],"\" name=\"", names(l)[1],"\">", options, "</select>", collapse = "")
	
	if(identical(displayName, TRUE)) {
		stringr::str_c(names(l)[1], " ", dd)
	} else {
		dd
	}
}


append_vector <- function(vect, val){
	vect[length(vect) + 1] <- val
	vect
}


cat_e <- function(...) {
	cat(..., file=stderr())
}


check_pkg_and_load <- function(...) {
	pkg <- as_char(...)
	if (!base::require(pkg, character.only = TRUE)) {
		utils::install.packages(pkg)
		base::library(pkg, character.only = TRUE)
	}
}


func_string_with_query <- function(func_c, query) {
	txt <- NULL
	if (missing(query)) {
		txt <- str_c(func_c, "()")
	} else {
		q <-  c()
		for (i in names(query))
			q <- append_vector(q, query[[i]])

		txt <- str_c(func_c, "(",str_c(names(query), str_c(q), sep = " = ", collapse = ", ") ,")")
	}
	
	cat_e("calling: ", txt, "\n")
	txt
}

# eval_text <- function(txt, ...) {
# 	eval(parse(text = txt, ...))
# }

eval_func_with_query <- function(func_c, query = c(), ...) {
	eval_text(txt = func_string_with_query(func_c = func_c, query = query), ...)
}

