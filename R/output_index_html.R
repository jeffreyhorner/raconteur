#' Make a readme file
#' readme file that explains how the routes and views work together.  Also tells what needs to be filled in.
#' 
#' @param path path to folder where the readme should be placed
make_index_html <- function(path) {
	content <- "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
<head>
  <title>Quick Tutorial</title>
</head>
<body>
	<h1>Welcome to raconteur!</h1>

<p>The orientation of the function has been switched around to accomodate html requests.</p>
<p>The new format involves setting the arguments as url query variables such as:</p>

<pre>body_text(\"body_text\")
to
http://127.0.0.1:8181/<b>body_text?func_c=body_text</b>
http://bigbear-raconteur.metamx.com/raconteur/example_skeleton/<b>body_text?func_c=body_text</b></pre>

<p>A function to help you generate your url is \"func_to_url().\"</p>
<pre>> func_to_url(body_text) #func_to_url(\"body_text\")
[1] body_text?func_c=<obj>

> func_to_url(body_text, url = \"http://bigbear-raconteur.metamx.com/raconteur/example_skeleton\")
[1] \"http://bigbear-raconteur.metamx.com/raconteur/example_skeleton/body_text?func_c=<obj>\"
</body>
</html>"

	cat(content, file = file.path(path, "views", "index.html"))
}
