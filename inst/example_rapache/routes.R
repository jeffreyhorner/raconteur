###########################################
# Load any libraries you need here
###########################################
library(datasets)

###########################################
# Top level HTML pages
###########################################

render_brew <- function(template, params = list(), path = getwd()) {
	if (is.list(params)) {
		env <- new.env(TRUE)
			for(name in names(params)) {
				env[[name]] <- params[[name]]
			}
		params <- env
	}

	path <- file.path(path, "views", stringr::str_c(template, ".html"))
	if (!file.exists(path)) stop("Can not find ", template, " template ",
				call. = FALSE)
	setContentType('text/html')
	brew::brew(path, envir = params)
	OK
}

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
	setContentType('text/csv')
	eval(parse(text=paste('write.csv(',dataset,',file=stdout())')))
	OK
})



###########################################
# Web services
###########################################

router$get('/dataset/:dataset.json', function(dataset){
	if (!any(ls('package:datasets') == dataset))
		render_brew("error",list(dataset=dataset))
	else 
		render_json(get(dataset))
})

# Returned when no dataset's example renders a plot
bad_plot <- function(dataset){
    t <- tempfileWithExtension()
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

	setContentType('image/png')

	t <- tempfileWithExtension()
	#cat('tempfile is',t,'\n')
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
	))


	dev.off()
	payday <- try(readBin(t,'raw',file.info(t)$size))
	unlink(t)

	if (inherits(payday,'try-error') || length(payday) <=1 ){
		payday <- bad_plot(dataset)
	}
	
	sendBin(payday)

	OK
})

