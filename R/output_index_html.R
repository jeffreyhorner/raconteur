#' Make a readme file
#' readme file that explains how the routes and views work together.  Also tells what needs to be filled in.
#' 
#' @param path path to folder where the readme should be placed
make_index_html <- function(path, func_c) {


	file_path <- file.path(path, "index.html")
	print_line("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
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
\"body_text?func_c=body_text\"
</pre>

<p>A function to help you generate your url is \"func_to_url().\"</p>
<pre>> func_to_url(body_text) #func_to_url(\"body_text\")
[1] \"body_text?func_c=<obj>\"

> func_to_url(body_text, url = \"http://bigbear-raconteur.metamx.com/raconteur/example_skeleton\")
[1] \"http://bigbear-raconteur.metamx.com/raconteur/example_skeleton/body_text?func_c=<obj>\"", file_path = file_path, append = FALSE)

print_line("

> func_to_url(\"", func_c, "\", url = \"http://127.0.0.1:8181/\")", file_path = file_path)
	txt <- eval( parse( text = str_c( "func_to_url(\"", func_c,"\", url = \"http://127.0.0.1:8181/eval/\", unknown = \"&lt;obj&gt;\")") ) )
print_line("[1] \"<a href=\"", txt,"\" target=\"_blank\">",txt, "</a>\"", file_path = file_path)

print_line("
</body>
</html>
", file_path = file_path)

}


