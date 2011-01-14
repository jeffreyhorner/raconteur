#' Make a readme file
#' readme file that explains how the routes and views work together.  Also tells what needs to be filled in.
#' 
#' @param path path to folder where the readme should be placed
make_readme <- function(path) {
	content <- "hi!\n this is a readme file.\n\n\nThis is so cool!"
	
	cat(content, file = file.path(path, "Read-and-delete-me"))
}