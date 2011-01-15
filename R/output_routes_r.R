#' Make routes.R
#' Make a routes.R file that contains the main function to route and source for every function
#'
#' @param path path of folder were the file is to be saved
#' @param func_c character form of the function to be in main route
make_routes_r <- function(file_path, func_c) {
	
	print_line("# routes.R - contains two default routes
# 1. execute function - htmlpath/myFunc?arg1=\"A\"&arg2=5
# 2. view source of function - htmlpath/myFunc/source


# load_html(\"/\",func_c,\"?arg1=foo&arg2=bar\")
# Execute Function
router$get(\"/\", function() {", file_path = file_path)
		temp_path <- str_c("/",func_c,"?arg1=foo&arg2=bar")
	print_line("	redirect(\"",temp_path,"\")", sep="", file_path = file_path)
	print_line("})
	

# load_html(\"/\",func_c,\"?arg1=foo&arg2=bar\")
# Execute Function
router$get(\"/:func\", function(func, query, ...) {
	if(!is.missing(arg1) && !is.missing(arg2))
		if(identical(arg1, \"foo\") && identical(arg2, \"bar\"))
			redirect(\"tutorial.html\")

	call <- str_c(func, \"(\",str_c(names(query), str_c(\"\\\"\", query, \"\\\"\"), sep = \" = \", collapse = \", \") ,\")\")
	output <- eval(parse(text = call))
	
	# brews the file output.html in the /views dir
	render_brew(\"output\",list(header = \"Output\", output = output))
})


# load_html(\"/\",func_c,\"/source\")
# View Source
router$get(\"/:func/source\", function(func, query, ...) {
	output <- body_text(func)
	
	# brews the file output.html in the /views dir
	render_brew(\"output\",list(header = \"Source\", output = output))
})

# load_html(\"/\",func_c,\"/interaction\")
# PLEASE EDIT THE FILE BELOW!!!!
# View Interactions
router$get(\"/:func/interaction\", function(func, query, ...) {
	# brews the file interaction.html in the /views dir
	render_brew(\"interaction\",list(header = \"Interaction\", output = output))
})
", file_path = file_path)

}