library(RPostgreSQL)
source('src/brefSeasonStatFunctions.R')

getBREFSeasonLeaders(year_end_season = 2014,stat_type = 'Advanced') -> a
a[2] %>>% data.frame() %>>% tbl_df() -> a
data.frame(column_name = names(a)[1:25]) -> names_advanced
names_advanced$name = c('Player','Position','Age','Team ID','Games Played','Minutes Played','Played Efficency Rating',
												'True Shooting %','Effective Field Goal %','Free Throw Attempt Rate','3PT Attempt Rate','Offensive Rebound Percentage',
												'Defensive Rebound %','Total Rebound %','Assist Percentage','Steal %','BLK %','Turnover %','Usage Rate','Offensive Rating','Defensive Rating','Offensive Winshare','Defensive Winshare','Total Winshare','Winshare Per 48 Minutes')
names_advanced$table_name = 'Advanced Player Stats'
write.csv(names_advanced,'data/advanced_player_stat_table_names.csv',row.names = F)

getBREFSeasonLeaders(year_end_season = 2014,stat_type = 'Per Game') -> pg
pg[2] %>>% data.frame() %>>% tbl_df() -> pg
data.frame(column_name = names(pg)[1:28]) -> names_pg
names_pg$name = c('Player','Position','Age','Team ID','Games Played','Games Started','Minutes','Field Goals Made','Field Goal Attempts','Field Goal %','3PT Shots Made','3PT Shot Attempts','3PT %','2PT Shots Made','2PT Shot Attemps','2PT Shot %','Free Throws Made','Free Throw Attempts',
									'Free Throws %','Offensive Rebounds','Defensive Rebounds','Total Rebounds','Assists','Steals','Blocks','Turnovers','Personal Fouls','Points')
names_pg$name[c(7:9,11:12,14:15,17:18,20:28)] %>>% (paste(.,'Per Game')) -> names_pg$name[c(7:9,11:12,14:15,17:18,20:28)]
'Per Game Player Stats' -> names_pg$table_name
write.csv(names_pg,'data/per_game_player_stat_table_names.csv',row.names = F)


dbConnect(PostgreSQL(), user = "postgres", dbname = "nba") -> nba_con

rbind(names_advanced,names_pg) -> all_names 
all_names$data_source = "Basketball-Reference"
all_names %>>% data.frame() %>>%
	(dbWriteTable(conn = nba_con, name = 'table_column_descriptions',.,overwrite = T
	))
"ALTER TABLE table_column_descriptions ADD PRIMARY KEY (column_name,table_name,data_source);" %>>%
	(dbGetQuery(nba_con ,.))

