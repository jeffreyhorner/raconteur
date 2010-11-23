router$get("/index.html", function(...) {
	
	# brews the file index.html in the /views dir
	render_brew("index",list(...))
})

router$get("/upload", function(...) {
	
	# brews the file index.html in the /views dir
	render_brew("upload",list(...))
})
