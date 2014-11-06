## Coach Metadata Scraper
packages = c('rvest','dplyr','pipeR','data.table')
lapply(packages,library, character.only = T)
url = "http://www.basketball-reference.com/coaches/"

## Get the Table

url %>>% html() %>>%
	{html_table(x = .,trim = T,header = F)} %>>%
	data.frame() %>>%
	(coaches = tbl_df(.[3:nrow(.),]))

#Clean the Table

names(coaches) <- c('coach','from','to','birthday','university')
row.names(coaches) = NULL
coaches %>%
	filter(!is.na(from)) %>%
	filter(!coach == 'Coach') -> coaches

#Extract HOF
coaches$coach %>>% {grepl(pattern = '\\*',x = .)} -> coaches$hall_of_fame
coaches$coach %>>% {gsub(pattern = '\\*',replacement = '',x = .)} %>>% Trim() -> coaches$coach

url %>>% html() %>>%
	html_nodes(css = 'strong a') %>>%
	html_text() %>>%(
		active = .
	)
apply(coaches[2:3],2, function(x) as.numeric(x)) -> coaches[,2:3]
coaches$coach %in% active -> coaches$active_coach
coaches[coaches$coach %in% c("Lionel Hollins",'Byron Scott','Brian Shaw'),'to'] = 2015
ifelse(coaches$to == 2015,T,F) -> coaches$active_coach

url %>>% html() %>>%
	html_nodes(css ='td:nth-child(1) a') %>>%
	html_attrs() %>>% {paste0('http://www.basketball-reference.com',.)} -> coaches$coach_url

coaches$birthday %>>% as.Date('%B %d , %Y') -> coaches$birthday

coaches %>%
	filter(is.na(birthday)) %>%
	select(coach,birthday) -> missing_coaches #get DOB for missing coaches

coaches$birthday %>>% year() -> coaches$birth_year



##Play with the data

coaches %>%
	filter(!is.na(birth_year)) %>>%
	mutate(age = from - birth_year) %>%
	filter(age == min(age)|age == max(age)) -> oldest_youngest_coaches

coaches %>%
	filter(!is.na(birth_year)) %>>%
	mutate(age = from - birth_year) %>>%{
		.[,'age']
	} %>>%
	describe()

