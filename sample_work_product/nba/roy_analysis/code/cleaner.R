
roy = tbl_df(read.csv('nba_roy_data.csv'))
roy[,c('fg_pct','X3pt_pct','ft_pct','ws_per48')] = 
	apply(roy[,c('fg_pct','X3pt_pct','ft_pct','ws_per48')],2,function(x) 100* x)
write.csv(roy,'roy_data.csv', row.names = F)
tp = read.csv('players_yeams_performances.csv')
roy$team = 
	substr(tp$team_season,start = 9,stop = nchar(tp$team_season))
ds = merge(roy,tp)
ds = 
	cbind(ds,colsplit(ds$record,'\\-',c('team_wins','team_losses')))
ds$team_win_pct = 
	round(100 * (ds$team_wins / (ds$team_wins + ds$team_losses)),digits = 3)
years = colsplit(ds$season,'\\-',c('start_year','v'))
years$v = NULL
years$end_year = years$start_year + 1
ds = cbind(years,ds)
ds$team_season = NULL
write.csv(ds, 'roy_data.csv', row.names = F)

iframe_base <- "http://nflcombineresults.com/testembed2.php#advanced/search-advanced-query="
ds$iframe_url <- 
	paste0(iframe_base,
											 gsub('\\ ','+', gsub("\\'|\\&",'\\',ds$player_name)),"+",
				 gsub('\\ ','\\_', ds$team)
				 )

