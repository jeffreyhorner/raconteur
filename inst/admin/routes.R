router$get("/index.html", function(...) {
	
	# brews the file index.html in the /views dir
	render_brew("index",list(...))
})

router$get("/upload", function(...) {
	setContentType("text/plain")
	if (!is.null(POST)){
		cat("POST variable is... \n")
		str(POST)
	}
	if (!is.null(FILES)){
		str(FILES)
	} else {
		cat("No App Uploaded!\n")
	}
	OK	
})
