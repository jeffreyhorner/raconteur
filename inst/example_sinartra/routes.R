###########################################
# Load any libraries you need here
###########################################
library(datasets)

###########################################
# Top level HTML pages
###########################################

# Returned when no dataset's example renders a plot
rando_plot <- function(dataset){
	t <- tempfileWithExtension()
	png(t)
	#par(mar=rep(0,4))
	plot( rnorm(100), main = paste('Cool Random Plot!'), col = rainbow(100, alpha = runif(100, 0, 1)), pch = '.', cex = c(2, 3, 4, 5, 10, 50, 100))
	dev.off()
	# print(t)
	t
}

router$get("/doc/html/index.html", function() {
	redirect("/")
})
router$get("/", function() {
	redirect("index.html")
})
router$get("/index.html", function(...) {
	# brews the file index.html in the /views dir
	render_brew("index",list(...))
})


# This is the RESTful part of the application. Each 
# dataset found in the datasets package has its own
# URL
router$get("/dataset/:dataset.html", function(dataset) {
	if (any(ls('package:datasets') == dataset)) {
		example_code <- "no example code"
		pkgpaths <- .find.package('datasets')
		file <- utils:::index.search(dataset, pkgpaths, TRUE)
		if (length(file) > 0) {
			con <- textConnection('tf',open='w')
			on.exit(close(con))
			tools::Rd2ex(utils:::.getHelpFile(file), con)
			example_code <- paste(tf,collapse='\n')
		}
		
		has_image <- TRUE
		fileName <- dataset_example_pic(dataset)
		if(file.exists(fileName)) {
			file.remove(fileName)
		} else {
			cat("No Plot in Example... making a Rando Plot\n", file = stderr())
			has_image <- FALSE
		}
		
		render_brew("dataset",list(dataset=dataset, example_code = example_code, has_image = has_image))

	} else {
		render_brew("error",list(dataset=dataset))
	}
})


router$get("/dataset/:dataset.csv", function(dataset, query, ...) {
	# write to a textConnection
	con <- textConnection('payday',open='w')
	eval(parse(text=paste('write.csv(',dataset,',file=con)')))
	on.exit(close(con))
	
	if(!missing(query)) {
		if(identical(query["isText"], "TRUE"))
			return(sinartra:::render(paste(payday,collapse='\n'), mime_type = "text/html"))
	}
	
	sinartra:::render(paste(payday,collapse='\n'), mime_type = "text/csv")
})


# Convenience function to render an R object to JSON
# Supposed to be part of sinartra, but not yet.
render_json <- function(object) {

	# Some objects are instances of a sub-class of data.frame
	# and RJSONIO doesn't know what to do with them, so we just
	# use trickery.
	if (inherits(object,'data.frame',which = TRUE) > 0){

		class(object) <- 'data.frame'

		# Even these may have ts objects as columns so lets 
		# just punt for right now and assign it an NA column.
		for (i in names(object)){
			if (inherits(object[[i]],'ts')){
				object[[i]] <- NA
			}
		}
	}

	# Unclassing here is unclassy. Would be nice to use as.data.frame
	# but it seems that the original ts object is stuffed into the result
	# somehow.
	if (inherits(object,'ts')){
		object <- unclass(object)
	}

	if (inherits(object,'table') || inherits(object,'array')){
		object <- as.data.frame(object)
	}

	json <- RJSONIO::toJSON(object)
  sinartra:::render(json, mime_type = "application/json")
  
}

###########################################
# Web services
###########################################

router$get('/dataset/:dataset.json', function(dataset){
	if (!any(ls('package:datasets') == dataset))
		render_brew("error",list(dataset=dataset))

	render_json(get(dataset))
})


dataset_example_pic <- function(dataset) {
	if (!any(ls('package:datasets') == dataset))
		return(render_brew("error",list(dataset=dataset)))

	t <- tempfileWithExtension()
	# cat('tempfile is',t,'\n',file=stderr())
	png(t)
		
		# This is a bit of magic. R has an example function
		# which runs example code located at the end of a
		# particular help topic. Fortunately, there's a help
		# topic for all datasets exported from the 'datasets'
		# package. Unfortunately, not all of them produce a plot,
		# and they can be noisy.
		#
		# This is where you would place your own data and plot routines, FYI
		#
		capture.output(
			suppressWarnings(
				eval(
					substitute(
						example(dataset,package='datasets',ask=FALSE),
						list(dataset=dataset)
					)
				)
			)
		)
		
	dev.off()
	
	t
}


dataset_example_pic_or_rando <- function(dataset) {
	fileName <- dataset_example_pic(dataset)
	if(file.exists(fileName)) {
		fileName
	} else {
		cat("No Plot in Example... making a Rando Plot\n", file = stderr())
		rando_plot(dataset)
	}
}

router$get('/dataset/:dataset.png', function(dataset){
	fileName <- dataset_example_pic_or_rando(dataset)
	
	# print(fileName)
	static_file(fileName, remove = TRUE)
})

