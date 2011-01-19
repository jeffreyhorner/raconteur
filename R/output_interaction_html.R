make_view_interaction <- function(path, func_c) {
	
	file_path <- file.path(path, "interaction.html")
	
	print_line("", file_path = file_path, append = FALSE)
	
	
	print_line("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">", file_path = file_path)
	print_line("<html xmlns=\"http://www.w3.org/1999/xhtml\">", file_path = file_path)
	print_line("<head>", file_path = file_path)
	print_line("  <title><%= header %></title>", file_path = file_path)
	print_line("</head>", file_path = file_path)
	print_line("<script type=\"text/javascript\" src=\"http://code.jquery.com/jquery-latest.min.js\">", file_path = file_path)
	# <script type="text/javascript">
	# 
	# 	$(document).ready(function() {
	# 		
	# 	});
	# 	
	# 	execute_, func_c,(){
	# 		
	# 		$.ajax({
	# 		    url: 'document.xml',
	# 		    type: 'GET',
	# 		    dataType: 'xml',
	# 		    timeout: 1000,
	# 		    error: function(){
	# 		        alert('Error loading XML document');
	# 		    },
	# 		    success: function(xml){
	# 		        // do something with xml
	# 		    }
	# 		});
	# 	}
	# 
	# </script>
	
	
	print_line("<body>", file_path = file_path)
	print_line("	<%= output %>", file_path = file_path)
	print_line("</body>", file_path = file_path)
	print_line("</html>
	", file_path = file_path)
	
}