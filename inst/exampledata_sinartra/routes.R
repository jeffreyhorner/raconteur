###########################################
# Load any libraries you need here
###########################################
library(datasets)

###########################################
# Top level HTML pages
###########################################

router$get("/", function(...) {
	
	# brews the file index.html in the /views dir
	render_brew("index",list(...))
})

# This is the RESTful part of the application. Each 
# dataset found in the datasets package has its own
# URL
router$get("/dataset/:dataset.html", function(dataset) {
	if (any(ls('package:datasets') == dataset))
		render_brew("dataset",list(dataset=dataset))
	else
		render_brew("error",list(dataset=dataset))
})
router$get("/dataset/:dataset.csv", function(dataset) {
	# write to a textConnection
	con <- textConnection('payday',open='w')
	eval(parse(text=paste('write.csv(',dataset,',file=con)')))
	close(con)
	list(
		payload = paste(payday,collapse='\n'),
		"content-type" = 'text/csv',
		"headers" = c(),
		"status code" = 200
	)
})


# Convenience function to render an R object to JSON
# Supposed to be part of sinartra, but not yet.
render_json <- function(object) {

	# Some objects are instances of a sub-class of data.frame
	# and RJSONIO doesn't know what to do with them, so we just
	# use trickery.
	if (inherits(object,'data.frame',which=TRUE) > 0){

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

	json <- toJSON(object)

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

# Returned when no dataset's example renders a plot
bad_plot <- function(dataset){
    t <- tempfile('png')
    png(t)
    #par(mar=rep(0,4))
    plot(rnorm(100),main=paste('Fail for',dataset,' but Check it!'),col=rainbow(100,alpha=runif(100,0,1)),pch='.',cex=c(2,3,4,5,10,50,100))
    dev.off()
	payday <- readBin(t,'raw',file.info(t)$size)
	unlink(t)
	payday
}

router$get('/dataset/:dataset.png', function(dataset){

	if (!any(ls('package:datasets') == dataset))
		render_brew("error",list(dataset=dataset))

	t <- tempfile('png')
	cat('tempfile is',t,'\n',file=stderr())
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
	suppressWarnings(
		eval(
			substitute(
				example(dataset,package='datasets',ask=FALSE),
				list(dataset=dataset)
			)
		)
	)

	dev.off()
	payday <- try(readBin(t,'raw',file.info(t)$size))
	unlink(t)

	if (inherits(payday,'try-error') || length(payday) <=1 ){
		cat("oops! bad plot\n",file=stderr())
		payday <- bad_plot(dataset)
	}

	# This is the low level return value that the R help web server
	# expects. A list with four named elements. The payload must contain
	# the entire response, regardless of size.
	list(
		payload = payday,
		"content-type" = 'image/png',
		"headers" = c(),
		"status code" = 200
	)
})

