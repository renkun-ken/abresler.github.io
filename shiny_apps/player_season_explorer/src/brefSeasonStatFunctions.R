packages = c('rvest','pipeR','dplyr','XML','RCurl')
lapply(packages,library,character.only = T)
library(rvest);library(pipeR);library(dplyr);library(XML);library(RCurl)
bref_team_base = 'http://www.basketball-reference.com/leagues/'
bref_base = 'http://www.basketball-reference.com'
getBREFSeasonLeaders = function(year_end_season, stat_type = c("Advanced","Totals","Per Minute","Per Game"),  league = 'NBA'){
	bref_team_base = 'http://www.basketball-reference.com/leagues/'
	bref_base = 'http://www.basketball-reference.com'
	match.arg(stat_type,choices = c("Advanced",'Totals','Per Minute',"Per Game")) -> stat_type
	stat_type %>>% tolower() %>>% (gsub("\\ ",'_',.)) -> stat_type
	paste0(bref_team_base,league,'_',year_end_season,'_',stat_type,'.html') -> url
	paste0('#',stat_type) -> css_page
	"td:nth-child(2) a" -> css_player
	url %>>%
		html() %>>%
		html_nodes(css_page) %>>%
		html_table() %>>% data.frame() %>>% tbl_df() -> table
	names(table) %>>% tolower() -> names(table)
	table %>>%
		filter(!g == 'G') -> table
	apply(table[,!names(table) %in% c('player','pos','age','tm')],2,function(x) as.numeric(x)) -> table[,!names(table) %in% c('player','pos','age','tm')]
	table$player %>>% (grepl('\\*',.) -> table$allstar)
	table$player %>>% (gsub(pattern = '\\*','',.) -> table$player)
	url %>>%
		html() %>>%
		html_nodes(css_player) %>>%
		html_attrs() %>>%
		(paste0(bref_base,.) -> table$bref_player_url)
	table$rk = NULL
	names(table)[names(table) %in% 'tm'] <- 'bref_team_id'
	table %>>%
		select(player:bref_team_id,bref_player_url) %>>%
		filter(!bref_team_id == 'TOT') -> players_seasons_teams
	paste0((year_end_season-1),"-",year_end_season) -> players_seasons_teams$season
	'players_teams_season' -> players_seasons_teams$table_name
	'Basketball-Reference' -> players_seasons_teams$data_source
	Sys.time() -> players_seasons_teams$scrape_time
	table %>>%
		group_by(player) %>>%
		filter(g == max(g)) %>>%
		select(-bref_player_url,-allstar)-> player_total_stats_pg
	paste0('player_',stat_type,"_total") -> player_total_stats_pg$table_name
	paste0((year_end_season-1),"-",year_end_season) -> player_total_stats_pg$season	
	'Basketball-Reference' -> player_total_stats_pg$data_source
	Sys.time() -> player_total_stats_pg$scrape_time
	table %>>%
		filter(!bref_team_id == 'TOT') %>>%
		select(-bref_player_url,-allstar) -> player_team_stats_pg
	paste0('player_',stat_type,"per_team_total") -> player_team_stats_pg$table_name
	paste0((year_end_season-1),"-",year_end_season) -> player_team_stats_pg$season	
	'Basketball-Reference' -> player_team_stats_pg$data_source
	Sys.time() -> player_team_stats_pg$scrape_time
	list(players_seasons_teams,player_total_stats_pg,player_team_stats_pg) -> all_tables
	return(all_tables)
}
getBREFSeasonLeaders(year_end_season = 2015,'Advanced')
