#' Make a readme file
#' readme file that explains how the routes and views work together.  Also tells what needs to be filled in.
#' 
#' @param path path to folder where the readme should be placed
make_index_html <- function(path) {
	content <- "<h1>Welcome to raconteur!</h1>

<p>The orientation of the function has been switched around to accomodate html requests.</p>
<p>The new format involves setting the arguments as url query variables such as:</p>

<pre>example_seq(to = 10)
to
http://127.0.0.1:8181/<b>example_seq?to=10</b>
http://bigbear-raconteur.metamx.com/raconteur/example_seq/<b>example_seq?to=10</b>

example_seq(from = 4, to = 7)
to
http://bigbear-raconteur.metamx.com/raconteur/example_seq/<b>example_seq?from=4&to=7</b></pre>"

	cat(content, file = file.path(path, "views", "index.html"))
}
