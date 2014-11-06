#setwd("~/Desktop/Github/asb_shiny_apps/nba/star_chart_ws")
#'data/advanced_player_stat_table_names.csv' %>>% read.csv() %>>% (data.frame(.,stringsAsFactors = F)) %>>% tbl_df()-> table_names_advanced 
#'data/per_game_player_stat_table_names.csv' %>>% read.csv() %>>% (data.frame(.,stringsAsFactors = F)) %>>% tbl_df()-> table_names_pg 
#rbind(table_names_advanced[,c(1:3)],table_names_pg[,c(1:3)]) -> all_names
#all_names[!duplicated(all_names$column_name),] -> all_names
#c("PG","SG","SF","PF","C") -> positions
#all_names %>>% 
#	filter(!name %in% c("Player","Games Played","Games Started","Team ID","Position")) %>>%
#	arrange(name) %>>%
#	select(name) %>>% (.$name) -> rank_options
#rank_options %>>% as.character() -> rank_options
#("Winshare Total","Turnovers Per Game","Blocks Per Game","Steals Per Game","Assists Per Game","Total Rebounds Per Game", "Usage Rate","Player Efficency Rating") -> selected_plot

## Year Selector

#50 -> players
#2011 -> season_name
#(season_name - 1) %>>% ((paste0(.,'-',season_name))) -> s
#10 -> min_games
#500 -> min_minutes
#"Winshare Total" %>>% as.character() -> sort_var_name
#selected_plot %>>% as.character() -> selected_names

#Per Game
getPGYearTable = function(season_end = 2014, min_games = 5 ){
	season_end %>>% as.numeric() -> season_name
	(season_name - 1) %>>% ((paste0(.,'-',season_name))) -> s	
	!season_end == (year(Sys.Date())+1) -> not_current_year
	
	if (not_current_year){
	'data/per_game_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
		filter(season == s) %>>%
		filter(g >= min_games) %>>% 
		select(-table_name,-data_source,-scrape_time) -> pg
	
	pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% 
		(.$pos_1 -> pg$pos)
} else{
	source('src/brefSeasonStatFunctions.R')
	getBREFSeasonLeaders(year_end_season = season_name,stat_type = "Per Game") -> pg
	pg[2] %>>% data.frame() %>>% tbl_df() %>>% tbl_df() %>>%
		filter(season == s) %>>%
		filter(g >= min_games) %>>% 
		select(-table_name,-data_source,-scrape_time) -> pg
	pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% 
		(.$pos_1 -> pg$pos)
}
return(pg)
}
#advanced
getADVYearTable = function(season_end = 2014, min_minutes_season = 500, min_games = 5 ){
	season_end %>>% as.numeric() -> season_name
	(season_name - 1) %>>% ((paste0(.,'-',season_name))) -> s	
	!season_end == (year(Sys.Date())+1) -> not_current_year
	min_minutes_season -> min_minutes
	if (not_current_year){
	'data/advanced_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
		filter(season == s) %>>%
		filter(g >= min_games) %>>% 
		filter(mp >= min_minutes) %>>%
		select(-table_name,-data_source,-scrape_time,-mp) -> adv
	
	adv$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% 
		(.$pos_1 -> adv$pos)
} else{
	source('src/brefSeasonStatFunctions.R')
	getBREFSeasonLeaders(year_end_season = season_name,stat_type = "Advanced") -> adv
	adv[2] %>>% data.frame() %>>% tbl_df() %>>% tbl_df() %>>%
		filter(season == s) %>>%
		filter(g >= min_games) %>>% 
		filter(mp >= min_minutes) %>>%
		select(-table_name,-data_source,-scrape_time,-mp) -> adv
	adv$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% 
		(.$pos_1 -> adv$pos)
}
return(adv)
}
getYearMergedTable = function(season_end = 2014, min_minutes_season = 500, min_games = 5){
	getPGYearTable(season_end = season_end, min_games = min_games) -> pg
	getADVYearTable(season_end = season_end,min_minutes_season = min_minutes_season,min_games = min_games) -> adv
	merge(pg,adv,by = names(pg)[names(pg) %in% names(adv)], all.x = T) -> plot_data
	return(plot_data)
}
