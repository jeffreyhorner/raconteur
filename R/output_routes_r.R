#' Make routes.R
#' Make a routes.R file that contains the main function to route and source for every function
#'
#' @param path path of folder were the file is to be saved
#' @param func_c character form of the function to be in main route
make_routes_r <- function(file_path, func_c) {
	
	print_line("# routes.R - contains two default routes
# 1. execute function - htmlpath/myFunc?arg1=\"A\"&arg2=5
# 2. view source of function - htmlpath/myFunc/source

router$get(\"/doc/html/index.html\", function() {redirect(\"/\")})
router$get(\"/\", function() {redirect(\"index.html\")})

# View the interaction \"index.html\" page
router$get(\"/index.html\", function(...) {
        # brews the file index.html in the /views dir
        render_brew(\"index\",list(...))
})


# load_html(fun_to_url(",func_c,", url = \"/eval/\")
# Execute Function
router$get(\"/eval/:func\", function(func, query, ...) {
	
	if(missing(query))
		call <- str_c(func, \"()\")
	else
		call <- str_c(func, \"(\",str_c(names(query), str_c(\"\\\"\", query, \"\\\"\"), sep = \" = \", collapse = \", \") ,\")\")
	output <- eval(parse(text = call))
	
	cat(\"making call: \", call, file = stderr())
	cat(\"output: \", output, file = stderr())
	# brews the file output.html in the /views dir
	render_brew(\"output\",list(header = \"Output\", output = output))
})


# load_html(str_c(\"/source/\", ", func_c,"))
# View Source
router$get(\"/source/:func\", function(func, query, ...) {
	output <- body_text(func)
	# brews the file output.html in the /views dir
	render_brew(\"output\",list(header = \"Source\", output = output))
})



# load_html(fun_to_url(",func_c,", url = \"/pic/\")
# View Picture
router$get(\"/pic/:func\", function(func, query, ...) {
	output <- body_text(func)
	
	if(missing(query))
		call <- str_c(func, \"()\")
	else
		call <- str_c(func, \"(\",str_c(names(query), str_c(\"\\\"\", query, \"\\\"\"), sep = \" = \", collapse = \", \") ,\")\")
	
	f <- tempfileWithExtension(\"png\")
	png(f)
	  eval(parse(text = call))
	dev.off()
	
	# brews the file output.html in the /views dir
	static_file(f, remove = TRUE)
})


# load_html(\"/interaction/\",", func_c, ")
# PLEASE EDIT THE FILE BELOW!!!!
# View Interactions
router$get(\"/:func/interaction\", function(func, query, ...) {
	# brews the file interaction.html in the /views dir
	render_brew(\"interaction\",list(header = \"Interaction\", output = output))
})

", file_path = file_path)

}