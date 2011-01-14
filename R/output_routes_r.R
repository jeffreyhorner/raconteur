#' Make routes.R
#' Make a routes.R file that contains the main function to route and source for every function
#'
#' @param path path of folder were the file is to be saved
#' @param func_c character form of the function to be in main route
make_routes_r <- function(path, func_c) {
	file_path <- file.path(path, "routes.R")
	
	app_path <- options()$raconteur.app_path
	if(!is.null(app_path)) { #installed in box
		print_line("source(\"", app_path, "/", func_c, "/", func_c,".R\")", sep = "", file_path = file_path, append = FALSE)
	} else { #personal computer
		print_line("source(\"", func_c,".R\")", sep = "", file_path = file_path, append = FALSE)
	}
	
	print_line("# routes.R - contains two default routes", file_path = file_path)
	print_line("# 1. execute function - htmlpath/myFunc?arg1=\"A\"&arg2=5", file_path = file_path)
	print_line("# 2. view source of function - htmlpath/myFunc/source", file_path = file_path)
	
	print_line("", file_path = file_path);print_line("", file_path = file_path)
	
 	print_line("# load_html(\"/\",func_c,\"?arg1=foo&arg2=bar\")", file_path = file_path)
	print_line("# Execute Function", file_path = file_path)
	print_line("router$get(\"/\", function() {", file_path = file_path)
		temp_path <- str_c("/",func_c,"?arg1=foo&arg2=bar")
	print_line("	redirect(\"",temp_path,"\")", sep="", file_path = file_path)
	print_line("})", file_path = file_path)

	print_line("", file_path = file_path);print_line("", file_path = file_path)	
	
	print_line("# load_html(\"/\",func_c,\"?arg1=foo&arg2=bar\")", file_path = file_path)
	print_line("# Execute Function", file_path = file_path)
	print_line("router$get(\"/:func\", function(func, query, ...) {", file_path = file_path)
	print_line("	if(!is.missing(arg1) && !is.missing(arg2))", file_path = file_path)
	print_line("		if(identical(arg1, \"foo\") && identical(arg2, \"bar\"))", file_path = file_path)
	print_line("			redirect(\"tutorial.html\")", file_path = file_path)
	
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

	print_line("", file_path = file_path);print_line("", file_path = file_path)
	
	print_line("# load_html(\"/\",func_c,\"/interaction\")", file_path = file_path)
	print_line("# PLEASE EDIT THE FILE BELOW!!!!", file_path = file_path)
	print_line("# View Interactions", file_path = file_path)
	print_line("router$get(\"/:func/interaction\", function(func, query, ...) {", file_path = file_path)
	print_line("	# brews the file interaction.html in the /views dir", file_path = file_path)
	print_line("	render_brew(\"interaction\",list(header = \"Interaction\", output = output))", file_path = file_path)
	print_line("})", file_path = file_path)
	print_line("", file_path = file_path)
}