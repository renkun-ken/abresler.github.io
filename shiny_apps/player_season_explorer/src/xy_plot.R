packages = c('dplyr','pipeR','reshape2','data.table','rCharts','ggplot2')
lapply(packages,library,character.only = T)
s = "2013-2014"
min_games = 10
min_minutes = 0
setwd("~/Desktop/Github/asb_shiny_apps/nba/explorer/v3")
'data/advanced_player_stat_table_names.csv' %>>% read.csv() %>>% (data.frame(.,stringsAsFactors = F)) %>>% tbl_df()-> table_names_advanced 
'data/per_game_player_stat_table_names.csv' %>>% read.csv() %>>% (data.frame(.,stringsAsFactors = F)) %>>% tbl_df()-> table_names_pg 

rbind(table_names_advanced[,c(1:3)],table_names_pg[,c(1:3)]) -> all_names
all_names %>>%
	arrange((column_name)) -> all_names
all_names[!duplicated(all_names$column_name),] -> all_names



'data/per_game_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
	filter(season == s) %>>%
	filter(g >= min_games) %>>% 
	select(-table_name,-data_source,-scrape_time) -> pg
pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% (.$pos_1 -> pg$pos)
'data/'
'data/advanced_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
	filter(season == s) %>>%
	filter(g >= min_games) %>>% 
	filter(mp >= min_minutes) %>>%
	select(-table_name,-data_source,-scrape_time,-mp) -> adv
adv$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% (.$pos_1 -> adv$pos)
merge(pg,adv, by = names(pg)[names(pg) %in% names(adv)], all.y = T) -> all
c('x3p.1','x2p.1','ts.','efg.','fg.','ft.','ftr','x3par') -> adjust
all$pos  %>>% factor(levels = c("PG","SG","SF","PF","C")) -> all$pos
all$bref_team_id = NULL
all[is.na(all)] <- 0

apply(
	all[,adjust],2,function(x) as.numeric(x) * 100
) -> all[,adjust]

bin = 'age'
all[,bin]  %>>% cut_interval(n = 5) -> all$bin
xvar = 'fga'
all_names[all_names$column_name == xvar,'name'] %>>% (.$name) -> xname
yvar = 'pts'
all_names[all_names$column_name == yvar,'name'] %>>% (.$name) -> yname
group_var = 'bin'
facet = 'pos'


'x' -> names(all)[names(all) %in% xvar]
'y' -> names(all)[names(all) %in% yvar]

paste0("#!function(item){ return item.player + ' at ' +'age ' + item.age + ","' '",'+',
			 item.y," + ", "' ",yvar %>>% toupper,' on ',"'",' + ',
			 item.x," + ","' ",xvar %>>% toupper,"'"," }!#") -> tooltip
plot = rPlot(
	height=600,
	width=800, 
	x = xvar,
	y = yvar,
	data = all,
	color = bin,
	type = 'point',
	size = list( const = 2),
	tooltip = tooltip)
plot$facet(
	type = "wrap",
	var = list( var = facet, levels = all$pos %>>% unique %>>% levels),
	cols = 5
)
plot
#give name for x and y axis to match example
plot$guides(
	x = list( title = xname ),
	y = list( title = yname )
)
plot$guides(
	color = list(
	labels = all$bin %>>% levels %>>% as.character,
	position = "top"
),

)
plot$set(dom = 'chart1')
plot$params$height = 600
paste0(s,' ',yname,' by ',xname,' ,',min_games,' Game Minimum ',", ",min_minutes,' Minute Minimum') -> title
plot$addParams(title = title)
plot
