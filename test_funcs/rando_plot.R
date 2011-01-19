# setwd("inst")
# source("../load.r")
# source("../test_funcs/index_try.R")
# app.skeleton(rando_plot)


rando_plot <- function(count = 100){
	plot( 
		rnorm(count), 
		main = paste('Cool Random Plot!'), 
		col = rainbow(count, alpha = runif(100, 0, 1)), 
		pch = '.', cex = c(2, 3, 4, 5, 10, 50, 100)
	)
}

