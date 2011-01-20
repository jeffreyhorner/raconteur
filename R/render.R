raconteur_toJSON <- function(object) {
	# Some objects are instances of a sub-class of data.frame
	# and RJSONIO doesn't know what to do with them, so we just
	# use trickery.
	if (inherits(object,'data.frame',which=TRUE) > 0){

		class(object) <- 'data.frame'

		# Even these may have ts objects as columns so lets 
		# just punt for right now and assign it an NA column.
		for (i in names(object)){
			if (inherits(object[[i]],'ts')){
				object[[i]] <- NA
			}
		}
	}

	# Unclassing here is unclassy. Would be nice to use as.data.frame
	# but it seems that the original ts object is stuffed into the result
	# somehow.
	if (inherits(object,'ts')){
		object <- unclass(object)
	}

	if (inherits(object,'table') || inherits(object,'array')){
		object <- as.data.frame(object)
	}

	toJSON(object)
}

# Convenience function to render an R object to JSON
# Supposed to be part of sinartra, but not yet.
raconteur_render_json <- function(object) {
	sinartra:::render(raconteur_toJSON(object), mime_type = "application/json")
}

raconteur_render_javascript <- function(var_name, json_object) {
	# Yes, 'text/javascript is "obsolete" according to wikipedia, 
	# but it has more support than 'application/javascrip'
	# .... "Defined in RFC 4329 but not accepted in IE 8 or earlier"
	
	payday <- str_c("var ", var_name, " = ", json_object, ";")
	
	sinartra:::render(payday, mime_type = "text/javascript")
}




