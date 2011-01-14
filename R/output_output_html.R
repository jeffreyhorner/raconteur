make_view_output_html <- function(path) {
	
	file_path <- file.path(path, "output.html")
	
	print_line("", file_path = file_path, append = FALSE)
	
	
	print_line("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">", file_path = file_path)
	print_line("<html xmlns=\"http://www.w3.org/1999/xhtml\">", file_path = file_path)
	print_line("<head>", file_path = file_path)
	print_line("  <title><%= header %></title>", file_path = file_path)
	print_line("</head>", file_path = file_path)
	print_line("<body>", file_path = file_path)
	print_line("	<%= output %>", file_path = file_path)
	print_line("</body>", file_path = file_path)
	print_line("</html>", file_path = file_path)
	
}