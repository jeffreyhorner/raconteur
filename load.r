library(sinartra)
library(RJSONIO)
library(RCurl)
library(tools)
library(parser)
library(utils)
library(evaluate)




FILE <- (function() {
  attr(body(sys.function()), "srcfile")
})()$filename
PATH <- dirname(FILE)

# load data
# if (!exists("flea")) load(file.path(PATH, "data", "flea.rda"))

lapply(dir(file.path(PATH, "R"), full.name=TRUE), source)
