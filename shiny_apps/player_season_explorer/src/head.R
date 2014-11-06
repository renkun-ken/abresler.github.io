options(stringsAsFactors = F)
library(ggplot2);library(dplyr);library(pipeR);library(shiny);library(lubridate);library(corrplot);library(ggthemr);library(reshape2);library(data.table)
'data/advanced_player_stat_table_names.csv' %>>% read.csv() %>>% (data.frame(.,stringsAsFactors = F)) %>>% tbl_df()-> table_names_advanced 
'data/per_game_player_stat_table_names.csv' %>>% read.csv() %>>% (data.frame(.,stringsAsFactors = F)) %>>% tbl_df()-> table_names_pg 

rbind(table_names_advanced[,c(1:3)],table_names_pg[,c(1:3)]) -> all_names
all_names %>>% arrange(desc(name)) %>>%
	filter(!name %in% c('Player','Team ID')) %>>%
	select(name) %>>% (.$name) -> bin_options
all_names[!duplicated(all_names$column_name),] -> all_names
all_names[c(3,47:5),'name']  %>>% (.$name) -> bin_options
c("PG","SG","SF","PF","C") -> positions
all_names %>>% 
	filter(!name %in% c("Player","Games Played","Games Started","Team ID","Position")) %>>%
	arrange(name) %>>%
	select(name) %>>% (.$name) -> rank_options
rank_options %>>% as.character() -> rank_options
c("Winshare Total","Points Per Game","Turnovers Per Game","Blocks Per Game","Steals Per Game","Assists Per Game","Total Rebounds Per Game", "Usage Rate","Player Efficency Rating") -> selected_plot
Sys.Date() >= "2014-10-28" -> in_season
ifelse(in_season,
			 years_possible <- 2015:1951,
			 years_possible <- 2014:1951)
