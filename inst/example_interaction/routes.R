###########################################

###########################################
# Top level HTML pages
###########################################

# Returned when no dataset's example renders a plot
rando_plot <- function(count = 100){
        cat("starting rando plot\n", file = stderr())
        t <- paste(tempfile(), ".png", sep = "", collapse = "")

        cat("starting png dev on file: ", t, "\n", file = stderr())
        png(t)

        cat("starting plot with count: ", count, "\n", file = stderr())
        plot( rnorm(count), main = paste('Cool Random Plot!'), col = rainbow(count, alpha = runif(100, 0, 1)), pch = '.', cex = c(2, 3, 4, 5, 10, 50, 100))
        cat("done plot", file = stderr())

        dev.off()
        cat("dev off: file path: ", t, "\n", file = stderr())
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


router$get("/rando.png", function(query, ...) {
        cat("Making a rando plot!!!!!!!!!\n", file = stderr())
        count <- 100
        if(!missing(query)) {
                # print(query["count"])
                if(length(query[["count"]]))
                        count <- query[["count"]]
        }
        
        # brews the file index.html in the /views dir
        static_file(rando_plot(count), remove = TRUE)
})


