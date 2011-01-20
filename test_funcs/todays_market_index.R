# setwd("inst")
# source("../load.r")
# source("../test_funcs/todays_market_index.R")
# app.skeleton(todays_market_index)

# "http://gp2.metamx.com:8080/squrl/v1.1/openx/query/2011-01-01T00:00:00Z_2011-01-01T23:59:59Z;;[[%22sold-impressions%22%20%28hourly%20+%20sold-impressions%29]%20[%22price%22%20%28hourly%20+%20price%29]%20[%22impressions%22%20%28hourly%20+%20impressions%29]]?callback=var%20timeseries_data="
# 
# # then today's (hack the URL string)
# 
# "http://gp2.metamx.com:8080/squrl/v1/openx/query/2010-10-10T00:00:00Z_2010-10-10T23:59:59Z;;[[%22sold-impressions%22%20%28hourly%20+%20sold-impressions%29]%20[%22revenue%22%20%28hourly%20+%20revenue%29]%20[%22impressions%22%20%28hourly%20+%20impressions%29]]?callback=var%20timeseries_data="
# 
# [hour volume * hourly price] = [hourly revenue]
# 
# index =  100 * total_revenue_today / total_revenue_010101
# 
# 
# 
# 



twentyFourHourDataURL <- function(date_string = "2011-01-15") {
	# stringr::str_c( "http://gp2.metamx.com:8080/squrl/v1.1/openx/query/", startDateString, "T", pad_beg_w_zero(startHour), ":00:00Z_",endDateString, "T", pad_beg_w_zero(endHour), ":59:59Z;;[[%22sold-impressions%22%20%28hourly%20+%20sold-impressions%29]%20[%22price%22%20%28hourly%20+%20price%29]%20[%22impressions%22%20%28hourly%20+%20impressions%29]]?callback=var%20timeseries_data=", collapse = "")

	stringr::str_c( "http://gp2.metamx.com:8080/squrl/v1.1/openx/query/P7d_", date_string, ";;[[revenue%20(sma%207%20(daily%20+%20price))]]", collapse = "")
	
	
}

total_revenue_for_dates <- function(...) {
	
	cat_e("getting URL\n")
	index_date_url <- twentyFourHourDataURL(...) 

	cat_e("making URL\n")
	con <- url(index_date_url)
	on.exit(close(con))
	
	cat_e("getting data:", index_date_url, "\n")
	index_date_value <- stringr::str_c(suppressWarnings(readLines(con)), collapse = "")
	# print(index_date_value)
	cat_e("done getting data\n")
	
	# cat_e("trimming data string\n")
	index_date_value <- stringr::str_replace(index_date_value, "var [a-zA-Z_ =]*\\(", "")
	# print(index_date_value)
	# index_date_value <- stringr::str_sub(index_date_value, end = -2L)
	# print(index_date_value)


	cat_e("convert to json\n")
	tmp <- rjson::fromJSON(index_date_value)
	# print(tmp)
	cat_e("making usable\n")
	index_date_matrix <- as.data.frame(t(sapply(tmp, c)))
	# print(index_date_matrix)
	index_date_matrix$revenue[[1]]
}

pad_beg_w_zero <- function(x) {
	while(stringr::str_length(x) < 2)
		x <- stringr::str_c("0", x, collapse = "")
		
	x
}

make_date <- function(obj) {
	stringr::str_c(year(obj), month(obj), day(obj), sep = "-", collapse = "")
}

# 1999-12-31
todays_market_index <- function(date_string = "2011-01-15") {
	check_pkg_and_load(stringr)
	check_pkg_and_load(rjson)
	check_pkg_and_load(plyr)
	check_pkg_and_load(lubridate)
	
	start_sum <- total_revenue_for_dates(date_string = date_string)
	
	t <- lubridate::now() - lubridate::days(1)
	end_sum <- total_revenue_for_dates(date_string = make_date(t))
		
	ratio <- end_sum / start_sum
	payday <- 100 * signif(ratio, 5)
	
	cat_e("start val: ", start_sum, "\n")
	cat_e("end val: ", end_sum, "\n")
	cat_e("ratio: ", ratio, "\n")
	cat_e("payday: ", payday, "\n")
	
	payday
}
