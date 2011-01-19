make_view_output_html <- function(path) {
	
	file_path <- file.path(path, "output.html")
	
	print_line("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
	<head>
	  <title><%= header %></title>
	</head>
	<body>
		<%= output %>
	</body>
</html>
", file_path = file_path, append = FALSE)
	
}