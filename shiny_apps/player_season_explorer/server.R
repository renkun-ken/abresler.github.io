options(stringsAsFactors = F)
library(ggplot2);library(dplyr);library(pipeR);library(shiny);library(lubridate);library(corrplot);library(ggthemr);library(reshape2);library(data.table);library(class);library(MASS);library(kohonen);library(rCharts);library(plotrix);library(rvest)
detach("package:dplyr", unload=TRUE)
source('src/brefSeasonStatFunctions.R')
library("dplyr")
'data/advanced_player_stat_table_names.csv' %>>% read.csv() %>>% (data.frame(.,stringsAsFactors = F)) %>>% tbl_df()-> table_names_advanced 
'data/per_game_player_stat_table_names.csv' %>>% read.csv() %>>% (data.frame(.,stringsAsFactors = F)) %>>% tbl_df()-> table_names_pg 
rbind(table_names_advanced[,c(1:3)],table_names_pg[,c(1:3)]) -> all_names
all_names[!duplicated(all_names$column_name),] -> all_names
c("PG","SG","SF","PF","C","F","G") -> positions
all_names %>>% 
	filter(!name %in% c("Player","Games Played","Games Started","Team ID","Position")) %>>%
	arrange(name) %>>%
	select(name) %>>% (.$name) -> rank_options
rank_options %>>% as.character() -> rank_options
source('src/brefSeasonStatFunctions.R')
shinyServer(function(input, output) {
	source('src/brefSeasonStatFunctions.R')
output$mds_plot <- renderPlot({
	
	input$min_player_rank -> players
	input$season_end %>>% as.numeric() -> season_name
	!season_name == 2015 -> not_current_year
	(season_name - 1) %>>% ((paste0(.,'-',season_name))) -> s
	input$min_minutes -> min_minutes
	input$min_games -> min_games
	input$sort_mds %>>% as.character() -> sort_var_name
	input$plot_var %>>% as.character() -> selected_names
	input$position_selection -> pos_selected
	input$mds_scaled_per_minute -> scaled_per_minute
	input$max_quantile -> max_quantile
	all_names[all_names$name %in% sort_var_name,'column_name'] -> sort_var
	sort_var$column_name -> sort_var
	if (not_current_year){
		'data/per_game_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
			filter(season == s) %>>%
			filter(g >= min_games) %>>% 
			select(-table_name,-data_source,-scrape_time) -> pg
		pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% (.$pos_1 -> pg$pos)
		
		
	} else{
		getBREFSeasonLeaders(year_end_season = season_name,stat_type = "Per Game") -> pg
		pg[2] %>>% data.frame() %>>% tbl_df() %>>% tbl_df() %>>%
			filter(season == s) %>>%
			filter(g >= min_games) %>>% 
			select(-table_name,-data_source,-scrape_time) -> pg
		pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% 
			(.$pos_1 -> pg$pos)
	}
	
	if (not_current_year){
		'data/advanced_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
			filter(season == s) %>>%
			filter(g >= min_games) %>>% 
			filter(mp >= min_minutes) %>>%
			select(-table_name,-data_source,-scrape_time,-mp) -> adv
		
		adv$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% 
			(.$pos_1 -> adv$pos)
	} else{
		getBREFSeasonLeaders(year_end_season = season_name,stat_type = "Advanced") -> adv
		adv[2] %>>% data.frame() %>>% tbl_df() %>>% tbl_df() %>>%
			filter(season == s) %>>%
			filter(g >= min_games) %>>% 
			filter(mp >= min_minutes) %>>%
			select(-table_name,-data_source,-scrape_time,-mp) -> adv
		adv$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% 
			(.$pos_1 -> adv$pos)
	}
	merge(pg,adv,by = names(pg)[names(pg) %in% names(adv)], all.x = T, all.y = T) -> plot_data
	0 -> plot_data[is.na(plot_data)]
	all_names[all_names$name %in% sort_var_name,'column_name'] -> sort_var
	sort_var$column_name -> sort_var
	call <- substitute(arrange(plot_data, desc(sort_var)), list(sort_var = as.name(sort_var)))
	eval(call) -> plot_data
	plot_data[1:players,] -> mds
	if(season_name == 2015){
		mds$pos %>>% factor(levels = c('G','F','C',''),ordered = T) -> mds$pos
		c('G','F','C') -> plot_labels
		c('#ff0000','#0000bf','#008000','#000000') -> plot_colors
	}else{
		mds$pos %>>% factor(levels = c('PG','SG','SF','PF','C'),ordered = T) -> mds$pos
		c('PG','SG','SF','PF','C') -> plot_labels
		c('#000000','#800080','#008000','#ff0000','#0000bf') -> plot_colors
	}
	
	if(scaled_per_minute) {
		0 -> mds[is.na(mds)]
		mds %>>%
		select(-age,-player,-bref_team_id,-pos,-g,-season,-gs) -> mds_scale
	apply(mds_scale[,names(mds_scale) %in% c('pts','ast.','blk','fg','fga','tov','stl','fga','x3p','x3pa','drb','orb','x2p','x2pa','ft','fta','pf')],2, 
				function(x) x / mds_scale$mp) -> mds_scale[,names(mds_scale) %in% c('pts','ast.','blk','fg','fga','tov','stl','fga','x3p','x3pa','drb','orb','x2p','x2pa','ft','fta')]
	#select(-mp,fg:pts,orb.:tov.,-usg.:-ws.48)
	mds_scale  %>% select(-mp,fg:pts,orb.:tov.,-usg.:-ws.48) %>>% 
		scale() %>%  dist() %>>% 
		cmdscale(eig = T, 2) -> fit_scale
	x_scale <- fit_scale$points[, 1]
	y_scale <- fit_scale$points[, 2]
	data.frame(x = x_scale,y = y_scale, position = mds$pos, player = mds$player, age = mds$age) -> df_scale
	
	title = paste0(s,' Season Player MDS Clusters, Scaled Per Minute\n',min_games,' Game Minimum, ',min_minutes,' Total Minute Minimum\n','Top ',players,' Ranked on ',sort_var_name)
	df_scale %>>% ggplot(aes(x = x,y = y,label = player, colour = position)) + 
		geom_text() +
		scale_colour_manual(values = plot_colors, 
												labels = plot_labels, name = 'Position') + 
		ggtitle(label = title) -> gg
	gg}else{
		0 -> mds[is.na(mds)]
		mds %>>%
			select(-age,-player,-bref_team_id,-pos,-g,-season,-gs,fg:x2p.,orb.:tov.,ts.,drb.,-trb.,x3p.) %>>%
			scale() %>%
			dist() -> d
		
		d %>>% cmdscale(eig = T, 2) -> fit
		x <- fit$points[, 1]
		y <- fit$points[, 2]
		data.frame(x,y, position = mds$pos, player = mds$player) -> df
		
		title = paste0(s,' Season Player MDS Clusters, Scaled Per Game\n',min_games,' Game Minimum, ',min_minutes,' Total Minute Minimum\n','Top ',players,' Ranked on ',sort_var_name)
		df %>>% ggplot(aes(x = x,y = y,label = player, colour = position)) + 
			geom_text() +
			scale_colour_manual(values = plot_colors, 
													labels = plot_labels, name = 'Position') + 
			ggtitle(label = title) -> gg
		gg
	}
	
},width = 800, height = 520)	

output$xy_chart <- renderChart2({
	input$season_end %>>% as.numeric() -> season_name
	!input$season_end == (year(Sys.Date())+1) -> not_current_year
	(season_name - 1) %>>% ((paste0(.,'-',season_name))) -> s
	input$min_minutes -> min_minutes
	input$min_games -> min_games
	input$rc_xvar -> xname
	all_names[all_names$name == xname,'column_name'] %>>% (.$column_name) -> xvar
	if(input$log_x){
		paste("Log", xname) -> xname
	}else{
		xname -> xname
	}
	input$rc_yvar -> yname 
	all_names[all_names$name == yname,'column_name'] %>>% (.$column_name) -> yvar
	if(input$log_x){
		paste("Log", yname) -> yname
	}else{
		yname -> yname
	}
	input$rc_facet -> facet_on
	facet = 'pos'
	input$rc_bin -> bin 
	bin %>>% as.character() -> bin_var_name
	all_names[all_names$name %in% bin_var_name,'column_name'] -> bin_var
	bin_var$column_name -> bin
	
	
	if (not_current_year){
		'data/per_game_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
			filter(season == s) %>>%
			filter(g >= min_games) %>>% 
			select(-table_name,-data_source,-scrape_time) -> pg
		pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% (.$pos_1 -> pg$pos)
		
		
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
	names(adv)[!names(adv) %in% names(pg)] -> adv_names
	c('player','pos',adv_names) -> adv_names
	merge(pg,adv[,adv_names],by = c('player','pos'),all.x = T, all.y = T) -> all
	all[is.na(all)] <- 0
	if(all$pos  %>>% length >= 3){
		all$pos  %>>% factor(levels = c("PG","SG","SF","PF","C")) -> all$pos} else{
			all$pos  %>>% factor(levels = c("G","F","C")) -> all$pos
		}
	all$bref_team_id = NULL
	all$age %>>% as.numeric -> all$age
	if(input$log_x){
		all[,xvar] %>>% log1p -> all$x_plot
	} else{
		all[,xvar] -> all$x_plot
	}
	
	if(input$log_y){
		all[,yvar] %>>% log1p -> all$y_plot
	} else{
		all[,yvar] -> all$y_plot
	}
	all %>>%
		filter(x_plot > 0 & y_plot > 0) -> all
	all[,yvar] %>>% rescale(newrange = c(.25,2)) -> all$size
	paste('item',xvar,sep = ".") -> item.x
	paste('item',yvar,sep = ".") -> item.y
	paste0("#!function(item){ return item.player + ' at ' +'age ' + item.age + ","' '",'+',
				 item.y," + ", "' ",yvar %>>% toupper,' on ',"'",' + ',
				 item.x," + ","' ",xvar %>>% toupper,"'"," }!#") -> tooltip	
	all %>>%
		filter(x_plot > 0 & y_plot > 0) -> all
	
	if(facet_on) {
		substitute(arrange(all,desc(bin)), list(bin = as.name(bin))) %>>% 
			eval  -> all
		all[,bin]  %>>% cut_interval(n = 5) -> all$bin_group
		all$bin_group %>>% (gsub("\\[","\\(",.)) %>>% factor(ordered = T) -> all$bin_group
		plot = rPlot(
		height=500,
		width=800, 
		x = 'x_plot',
		y = 'y_plot',
		data = all,
		color = 'bin_group',
		type = 'point',
		tooltip = tooltip,
		size ='size')
	plot$facet(
		type = "wrap",
		var = list( var = facet, levels = all$pos %>>% unique %>>% levels),
		cols = 5
	)
	#give name for x and y axis to match example
	plot$guides(
		x = list( title = xname ),
		y = list( title = yname ),
		numticks = length(10)
	)
	plot$guides(
		color = list(
			title = input$rc_bin,
			labels = all$bin_group,
			position = "top"
		)
	)
	plot$params$height = 525
	paste0(s,' ',yname,' by ',xname,', ',min_games,' Game Minimum, ',
				 min_minutes,' Minute Minimum') -> title
	plot$addParams(title = title)} else{
	substitute(summarise(all,mean_yvar = mean(yvar)), list(yvar = as.name(yvar))) %>>% 
			eval  -> mean_yvar
		all %>>%
			group_by(pos) -> as
		as %>>%
			summarise(mean_yvar = mean(pts))
		plot = rPlot(
			height=500,
			width=800, 
			x = 'x_plot',
			y = 'y_plot',
			data = all,
			color = 'pos',
			type = 'point',
			tooltip = tooltip,
			size ='size')
		#give name for x and y axis to match example
		plot$guides(
			x = list( title = xname ),
			y = list( title = yname ),
			numticks = length(10)
		)
		plot$guides(
			color = list(
				labels = all$pos %>>% levels() %>>% factor(ordered=T),
				position = "top"
			)
		)
		plot$params$height = 525
		paste0(s,' ',yname,' by ',xname,', ',min_games,' Game Minimum, ',
					 min_minutes,' Minute Minimum') -> title
		plot$addParams(title = title)
		loess.smooth(
			x = all[,xvar],
			y = all[,yvar]
		) %>>% data.frame -> smoothing
	#add loess smoothing line as layer
	#plot$layer(
	#	x = "x",
	#	y = "y",
	#	data = smoothing,
	#	type = "line"
	#)
	}
	plot	
})	

output$cluster <- renderChart2({
	input$season_end %>>% as.numeric() -> season_name
	!input$season_end == (year(Sys.Date())+1) -> not_current_year
	(season_name - 1) %>>% ((paste0(.,'-',season_name))) -> s
	input$min_minutes -> min_minutes
	input$min_games -> min_games
	input$position_selection -> pos_selected
	input$cluster_xvar -> xname
	all_names[all_names$name == xname,'column_name'] %>>% (.$column_name) -> xvar
	if(input$log_x){
		paste("Log", xname) -> xname
	}else{
		xname -> xname
	}
	input$cluster_yvar -> yname 
	all_names[all_names$name == yname,'column_name'] %>>% (.$column_name) -> yvar
	if(input$log_x){
		paste("Log", yname) -> yname
	}else{
		yname -> yname
	}
	input$kmeans_cluster_facet -> facet_on
	facet = 'pos'
	input$cluster_bin -> bin 
	bin %>>% as.character() -> bin_var_name
	all_names[all_names$name %in% bin_var_name,'column_name'] -> bin_var
	bin_var$column_name -> bin
	input$clusters_kmeans -> kmeans_clusters
	
	if (not_current_year){
		'data/per_game_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
			filter(season == s) %>>%
			filter(g >= min_games) %>>% 
			select(-table_name,-data_source,-scrape_time) -> pg
		pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% (.$pos_1 -> pg$pos)
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
	names(adv)[!names(adv) %in% names(pg)] -> adv_names
	c('player','pos',adv_names) -> adv_names
	merge(pg,adv[,adv_names],by = c('player','pos'), all.x = T, all.y = T) -> all
	all[is.na(all)] <- 0
	if(all$pos  %>>% length >= 3){
		all$pos  %>>% factor(levels = c("PG","SG","SF","PF","C")) -> all$pos} else{
			all$pos  %>>% factor(levels = c("G","F","C")) -> all$pos
		}
	all$bref_team_id = NULL
	all[,xvar] -> all$x_plot
	paste('item',xvar,sep = ".") -> item.x
	paste('item',yvar,sep = ".") -> item.y
	paste0("#!function(item){ return item.player + ' at ' +'age ' + item.age + ","' '",'+',
				 item.y," + ", "' ",yvar %>>% toupper,' on ',"'",' + ',
				 item.x," + ","' ",xvar %>>% toupper,"'"," }!#") -> tooltip	
	if(facet_on) {
		all[,c(xvar,yvar)] -> plot
		plot  %>>% kmeans(centers = kmeans_clusters) -> results
		all$age -> plot$age
		all$player -> plot$player
		all$pos -> plot$pos
		results$cluster %>>% as.numeric -> plot$cluster
		plot %>>% data.frame %>>% tbl_df -> plot
		plot %>>%
			arrange(cluster) -> plot
		paste0("#!function(item){ return 'Cluster ' + item.cluster + ' '+ item.player + ' at ' +'age ' + item.age + ","' '",'+',
					 item.y," + ", "' ",yvar %>>% toupper,' on ',"'",' + ',
					 item.x," + ","' ",xvar %>>% toupper,"'"," }!#") -> tooltip	
		plot$cluster %>>% as.character() -> plot$cluster
		p1 = rPlot(
			height=600,
			width=800, 
			x = xvar,
			y = yvar,
			data = plot,
			color ='cluster',
			type = 'point',
			size = list( const = 2),
			tooltip = tooltip)
		p1$facet(
			type = "wrap",
			var = list( var = facet, levels = plot$pos %>>% unique %>>% levels),
			cols = 5
		)
		#give name for x and y axis to match example
		p1$guides(
			x = list( title = xname ),
			y = list( title = yname ),
			numticks = length(10)
		)
		p1$guides(
			color = list(
				#title = input$cluster_bin,
				labels = seq(from = 1,to = length(input$clusters_kmeans),by = 1),
				position = "top"
			)
		)
		p1$params$height = 525
		paste0(s,' Kmeans Cluster Plot, ',yname,' by ',xname,', ',min_games,' Game Minimum, ',
					 min_minutes,' Minute Minimum') -> title
		p1$addParams(title = title)} else{
			all[,c(xvar,yvar)] -> plot
			plot  %>>% kmeans(centers = kmeans_clusters) %>>% tbl_df -> results
			all$age -> plot$age
			all$player -> plot$player
			all$pos -> plot$pos
			results$cluster %>>% as.numeric -> plot$cluster
			plot %>>% data.frame %>>% tbl_df -> plot
			plot %>>%
				arrange((cluster)) -> plot
			paste0("#!function(item){ return 'Cluster ' + item.cluster + ' '+ item.player + ' at ' +'age ' + item.age + ","' '",'+',
						 item.y," + ", "' ",yvar %>>% toupper,' on ',"'",' + ',
						 item.x," + ","' ",xvar %>>% toupper,"'"," }!#") -> tooltip	
			plot$cluster %>>% as.character() -> plot$cluster
			p1 = rPlot(
				height=600,
				width=800, 
				x = xvar,
				y = yvar,
				data = plot,
				color ='cluster',
				type = 'point',
				size = list( const = 2),
				tooltip = tooltip)
			#give name for x and y axis to match example
			p1$guides(
				x = list( title = xname ),
				y = list( title = yname ),
				numticks = length(10)
			)
			p1$guides(
				color = list(
					#title = input$cluster_bin,
					labels = seq(from = 1,to = length(input$clusters_kmeans),by = 1)+1,
					position = "top"
				)
			)
			p1$params$height = 525
			paste0(s,' Kmeans Cluster Plot, ',yname,' by ',xname,', ',min_games,' Game Minimum, ',
						 min_minutes,' Minute Minimum') -> title
			p1$addParams(title = title)
		}
	p1	
})	

output$kohonen_table = renderDataTable({
	input$players -> players
	input$season_end %>>% as.numeric() -> season_name
	set.seed(7)
	!input$season_end == (year(Sys.Date())+1) -> not_current_year
	(season_name - 1) %>>% ((paste0(.,'-',season_name))) -> s
	input$min_minutes -> min_minutes
	input$min_games -> min_games
	input$sort %>>% as.character() -> sort_var_name
	input$plot_var %>>% as.character() -> selected_names
	input$position_selection -> pos_selected
	input$xdim_t -> xdim
	input$ydim_t -> ydim
	input$scaled_per_minute_t -> scaled_per_minute
	input$clusters_t -> clusters
	
	if (not_current_year){
		'data/per_game_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
			filter(season == s) %>>%
			filter(g >= min_games) %>>% 
			select(-table_name,-data_source,-scrape_time) -> pg
		pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% (.$pos_1 -> pg$pos)
	} else{
		source('src/brefSeasonStatFunctions.R',keep.source = T)
		getBREFSeasonLeaders(year_end_season = season_name,stat_type = "Per Game") -> pg
		pg[2] %>>% data.frame() %>>% tbl_df() %>>% tbl_df() %>>%
			filter(season == s) %>>%
			filter(g >= min_games) %>>% 
			select(-table_name,-data_source,-scrape_time) -> pg
		pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% 
			(.$pos_1 -> pg$pos)
		
		
	}
	
	if (not_current_year){
		'data/advanced_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
			filter(season == s) %>>%
			filter(g >= min_games) %>>% 
			filter(mp >= min_minutes) %>>%
			select(-table_name,-data_source,-scrape_time) -> adv
		
		adv$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% 
			(.$pos_1 -> adv$pos)
	} else{
		source('src/brefSeasonStatFunctions.R',keep.source = T)
		getBREFSeasonLeaders(year_end_season = season_name,stat_type = "Advanced") -> adv
		adv[2] %>>% data.frame() %>>% tbl_df() %>>% tbl_df() %>>%
			filter(season == s) %>>%
			filter(g >= min_games) %>>% 
			filter(mp >= min_minutes) %>>%
			select(-table_name,-data_source,-scrape_time) -> adv
		adv$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% 
			(.$pos_1 -> adv$pos)
	}
	pg %>>% select(player,pos,mp,fg:pts,-ft.,-x3p.,-x2p.) -> df
	adv %>>%
		select(player,pos,orb.:tov.,usg.) %>%
		merge(df,.,all.y = T) -> df
	df[complete.cases(df),] -> df
	NULL -> row.names(df)
	df[,3:ncol(df)] -> df_data
	c('pts','ast.','blk.','fg.','fga','tov.','stl.','fga','x3p','x3pa','drb.','orb.','trb.','usg.') -> columns_pg
	c('pts','ast','blk','fg','fga','tov','stl','fga','x3p','x3pa','drb','orb','trb','usg.') -> columns_pm
	
	if(scaled_per_minute){
		apply(df_data[,2:ncol(df_data)],2, function(x) x/df_data$mp) %>>% data.frame() %>>% tbl_df() -> df_matrix
		df_matrix[,columns_pm] %>>% data.frame -> df_matrix
	} else{
		df_data %>>% data.frame() %>>% tbl_df() -> df_matrix
		df_matrix[,columns_pg] %>>% data.frame-> df_matrix
	}
	df_matrix %>>% as.matrix() %>>% data.frame  %>>% scale() -> nba_scaled_pg
	names(df_matrix) %>>% tolower() -> names(nba_scaled_pg)
	
	
	df %>>% tbl_df() -> df
	somgrid(xdim = xdim, ydim = ydim, topo = "hexagonal") ->  #could be 10x10
		som_grid
	nba_scaled_pg %>>% 
		som(grid = som_grid,
				rlen = 100, # could be 100
				alpha = c(.05,.01),
				keep.data = T,
				n.hood = 'circular') -> som_model_nba	
	som_model_nba$codes %>>% dist() %>>% hclust() %>>% cutree(clusters) -> som_cluster
	# Show the map with different colours for every cluster						  
	
	if(scaled_per_minute){
		'Scaled Per Minute' -> scale } else{
			'Scaled Per Game' -> scale
		}
	
	som_cluster[som_model_nba$unit.classif] -> df$cluster
	som_model_nba$unit.classif -> classification_group
	classification_group -> df$group
	df[,c(1:3,30:31,4:29)] -> df
	rm(som_model_nba);rm(som_grid)
	df[,sapply(df,is.character)] <- sapply(
		df[,sapply(df,is.character)],
		iconv,"WINDOWS-1252","UTF-8")
	df %>>% data.frame() -> df
	season_name -> df$season
	df[,c(32,1:31)] -> df
	df
	
},  options = list(orderClasses = TRUE,lengthMenu = c(10, 25, 50), pageLength = 10)
)

output$kohonen_plot <- renderPlot({
	input$players -> players
	input$season_end %>>% as.numeric() -> season_name
	!input$season_end == (year(Sys.Date())+1) -> not_current_year
	(season_name - 1) %>>% ((paste0(.,'-',season_name))) -> s
	input$min_minutes -> min_minutes
	set.seed(7)
	input$min_games -> min_games
	input$sort %>>% as.character() -> sort_var_name
	input$plot_var %>>% as.character() -> selected_names
	input$position_selection -> pos_selected
	input$xdim -> xdim
	input$ydim -> ydim
	input$scaled_per_minute_kp -> scaled_per_minute
	input$clusters -> clusters
	
	if (not_current_year){
		'data/per_game_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
			filter(season == s) %>>%
			filter(g >= min_games) %>>% 
			select(-table_name,-data_source,-scrape_time) -> pg
		pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% (.$pos_1 -> pg$pos)
		
		
	} else{
		source('src/brefSeasonStatFunctions.R',keep.source = T)
		getBREFSeasonLeaders(year_end_season = season_name,stat_type = "Per Game") -> pg
		pg[2] %>>% data.frame() %>>% tbl_df() %>>% tbl_df() %>>%
			filter(season == s) %>>%
			filter(g >= min_games) %>>% 
			select(-table_name,-data_source,-scrape_time) -> pg
		pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% 
			(.$pos_1 -> pg$pos)
	}
	
	if (not_current_year){
		'data/advanced_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
			filter(season == s) %>>%
			filter(g >= min_games) %>>% 
			filter(mp >= min_minutes) %>>%
			select(-table_name,-data_source,-scrape_time) -> adv
		
		adv$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% 
			(.$pos_1 -> adv$pos)
	} else{
		source('src/brefSeasonStatFunctions.R',keep.source = T)
		getBREFSeasonLeaders(year_end_season = season_name,stat_type = "Advanced") -> adv
		adv[2] %>>% data.frame() %>>% tbl_df() %>>% tbl_df() %>>%
			filter(season == s) %>>%
			filter(g >= min_games) %>>% 
			filter(mp >= min_minutes) %>>%
			select(-table_name,-data_source,-scrape_time) -> adv
		adv$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% 
			(.$pos_1 -> adv$pos)
	}
	pg %>>% select(player,pos,mp,fg:pts,-ft.,-x3p.,-x2p.) -> df
	adv %>>%
		select(player,pos,orb.:tov.,usg.) %>%
		merge(df,.,all.y = T) -> df
	df[complete.cases(df),] -> df
	NULL -> row.names(df)
	df[,3:ncol(df)] -> df_data
	c('pts','ast.','blk.','fg.','fga','tov.','stl.','fga','x3p','x3pa','drb.','orb.','trb.','usg.') -> columns_pg
	c('pts','ast','blk','fg','fga','tov','stl','fga','x3p','x3pa','drb','orb','trb','usg.') -> columns_pm
	df$player <- NULL
	
	if(scaled_per_minute){
		apply(df_data[,2:ncol(df_data)],2, function(x) x/df_data$mp) %>>% data.frame() %>>% tbl_df() -> df_matrix
		df_matrix[,columns_pm] -> df_matrix
	} else{
		df_data %>>% data.frame() %>>% tbl_df() -> df_matrix
		df_matrix[,columns_pg] -> df_matrix
	}
	df_matrix %>>% as.matrix()  %>>% scale() -> nba_scaled_pg
	names(df_matrix) %>>% tolower() -> names(nba_scaled_pg)
	
	
	df %>>% tbl_df() -> df
	somgrid(xdim = xdim, ydim = ydim, topo = "hexagonal") ->  #could be 10x10
		som_grid
	nba_scaled_pg %>>% 
		som(grid = som_grid,
				rlen = 100, # could be 100
				alpha = c(.05,.01),
				keep.data = T,
				n.hood = 'circular') -> som_model_nba	
	som_model_nba$codes %>>% dist() %>>% hclust() %>>% cutree(clusters) -> som_cluster
	# Show the map with different colours for every cluster						  
	
	if(scaled_per_minute){
		'Scaled Per Minute' -> scale } else{
		'Scaled Per Game' -> scale
	}
	
	som_cluster[som_model_nba$unit.classif] -> df$cluster
	som_model_nba$unit.classif -> classification_group
	classification_group -> df$group
	paste0(s,' NBA Kohonen Cluster Plot, ',min_games,' Game Minimum, ',min_minutes,' Total Minute Minimum\n' ,clusters,' Clusters, ',(xdim*ydim),' Groups ',scale) -> title 
	pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2','grey','pink','tomato1')
	c(pretty_palette,colors()[sample(1:100)]) -> pretty_palette
	som_model_nba %>>% plot(type="mapping", bgcol = pretty_palette[som_cluster], main = title)
	add.cluster.boundaries(som_model_nba, som_cluster)
	#show the same plot with the codes instead of just colours
	som_model_nba %>>% plot(type="codes", bgcol = pretty_palette[som_cluster], main = title)
	add.cluster.boundaries(som_model_nba, som_cluster)
	
}, width = 700, height = 500)

output$win_share_plot <- renderPlot({
		input$players -> players
		input$season_end %>>% as.numeric() -> season_name
		!input$season_end == (year(Sys.Date())+1) -> not_current_year
		(season_name - 1) %>>% ((paste0(.,'-',season_name))) -> s
		input$min_minutes -> min_minutes
		input$min_games -> min_games
		input$sort %>>% as.character() -> sort_var_name
		input$plot_var %>>% as.character() -> selected_names
		input$position_selection -> pos_selected
		if (not_current_year){
			'data/per_game_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
				filter(season == s) %>>%
				filter(g >= min_games) %>>% 
				select(-table_name,-data_source,-scrape_time) -> pg
			pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% (.$pos_1 -> pg$pos)
		} else{
			source('src/brefSeasonStatFunctions.R',keep.source = T)
			getBREFSeasonLeaders(year_end_season = season_name,stat_type = "Per Game") -> pg
			pg[2] %>>% data.frame() %>>% tbl_df() %>>% tbl_df() %>>%
				filter(season == s) %>>%
				filter(g >= min_games) %>>% 
				select(-table_name,-data_source,-scrape_time) -> pg
			pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% 
				(.$pos_1 -> pg$pos)
			
			
		}
		
		if (not_current_year){
			'data/advanced_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
				filter(season == s) %>>%
				filter(g >= min_games) %>>% 
				filter(mp >= min_minutes) %>>%
				select(-table_name,-data_source,-scrape_time,-mp) -> adv
			
			adv$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% 
				(.$pos_1 -> adv$pos)
		} else{
			source('src/brefSeasonStatFunctions.R',keep.source = T)
			getBREFSeasonLeaders(year_end_season = season_name,stat_type = "Advanced") -> adv
			adv[2] %>>% data.frame() %>>% tbl_df() %>>% tbl_df() %>>%
				filter(season == s) %>>%
				filter(g >= min_games) %>>% 
				filter(mp >= min_minutes) %>>%
				select(-table_name,-data_source,-scrape_time,-mp) -> adv
			adv$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% 
				(.$pos_1 -> adv$pos)
		}
		merge(pg,adv,by = names(pg)[names(pg) %in% names(adv)], all.x = T, all.y = T) -> plot_data
		plot_data %>>%
			filter(pos %in% pos_selected) -> plot_data
		0 -> plot_data[is.na(plot_data)]
		all_names[all_names$name %in% sort_var_name,'column_name'] -> sort_var
		sort_var$column_name -> sort_var
		call <- substitute(arrange(plot_data, desc(sort_var)), list(sort_var = as.name(sort_var)))
		eval(call) -> plot_data
		apply(plot_data[,!names(plot_data) %in% c('player','bref_team_id','season','pos')]
					,2, function(x) as.numeric(x)) -> plot_data[,!names(plot_data) %in% c('player','bref_team_id','season','pos')]
		all_names %>>%
			filter(name %in% selected_names) %>>%
			select(column_name) %>>% ((.$column_name) -> selected)
		selected -> star_cols
		
		all_names %>>%
			filter(name %in% selected_names) %>>%
			select(legend_name) %>>% ((.$legend_name) -> legend_names)
		# Set our color palette
		colors = c( "#AA66cc", "#FF4444" , "#000000","#1d2098",'#008000','#D0B240','#007198',
								'#F5811F','#008D97') -> pal
		c(pal,colors()[sample(1:100)]) -> pal
		# Set scale=FALSE so player stats are normalized over all players, not just the top 50
		add_legend <- function(...) {
			opar <- par(fig=c(0, 1, 0, 1), oma=c(5, 5, 5, 7), 
									mar=c(0, 0, 0, 0), new=TRUE)
			on.exit(par(opar))
			plot(0,0, type='n', bty='n', xaxt='n', yaxt='n')
			legend(...)
		}
		
		if(season_name < 2015){
			positions <- c('PG','SG','SF','PF','C')}else{
				c('G','F','C') -> positions
			}
		ifelse(input$position_selection == positions,
					 position_name <- 'All Positions',
					 input$position_selection %>>% (paste(.,collapse  = ", ")) -> position_name
		)
		
		paste0("NBA Player Statistics for ",position_name," in ", s, "\nPer Game, Top ", players, " Players Ranked on ", sort_var_name,'\nMinimum of ',min_games,' Games and ',min_minutes,' Total Minutes') -> title
		plot_data[1:players,(names(plot_data) %in% star_cols)] %>>% 
			stars(scale=T,radius = T,full = T,label=plot_data$player[1:players],flip.labels=F, frame.plot = TRUE,
						key.loc = c(15, 16.1),
						draw.segments=TRUE,col.segments=pal,ncol=10,cex=0.64,mar=c(1,0.5,4.5,0.5))
		title(main=title,
					cex.main=0.75)
		text(x=4,y=.5,labels="Data Source: basketball-reference.com",cex=0.5)
		paste0("Created at ", Sys.time(), " Using Alex Bresler's NBA Player Analysis Shiny Application") -> label_2
		text(x=18,y=.5,labels=label_2,cex=0.5)
	}, width = 800, height = 700)

output$corr_plot <- renderPlot({
	input$players -> players
	input$season_end %>>% as.numeric() -> season_name
	!input$season_end == (year(Sys.Date())+1) -> not_current_year
	(season_name - 1) %>>% ((paste0(.,'-',season_name))) -> s
	input$min_minutes -> min_minutes
	input$min_games -> min_games
	input$sort %>>% as.character() -> sort_var_name
	input$plot_var_cor %>>% as.character() -> selected_names
	input$position_selection_cor -> pos_selected
	if (not_current_year){
		'data/per_game_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
			filter(season == s) %>>%
			filter(g >= min_games) %>>% 
			select(-table_name,-data_source,-scrape_time) -> pg
		pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% (.$pos_1 -> pg$pos)
	} else{
		source('src/brefSeasonStatFunctions.R',keep.source = T)
		getBREFSeasonLeaders(year_end_season = season_name,stat_type = "Per Game") -> pg
		pg[2] %>>% data.frame() %>>% tbl_df() %>>% tbl_df() %>>%
			filter(season == s) %>>%
			filter(g >= min_games) %>>% 
			select(-table_name,-data_source,-scrape_time) -> pg
		pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% 
			(.$pos_1 -> pg$pos)
		
		
	}
	
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
	merge(pg,adv,by = names(pg)[names(pg) %in% names(adv)], all.x = T, all.y = T) -> plot_data
	0 -> plot_data[is.na(plot_data)]
	plot_data %>>%
		filter(pos %in% pos_selected) -> plot_data	
	all_names[all_names$name %in% sort_var_name,'column_name'] -> sort_var
	sort_var$column_name -> sort_var
	call <- substitute(arrange(plot_data, desc(sort_var)), list(sort_var = as.name(sort_var)))
	eval(call) -> plot_data
	apply(plot_data[,!names(plot_data) %in% c('player','bref_team_id','season','pos')]
				,2, function(x) as.numeric(x)) -> plot_data[,!names(plot_data) %in% c('player','bref_team_id','season','pos')]
	plot_data -> plot_data.norm
	plot_data.norm %>>%
		select(-season,-bref_team_id) -> plot_data.norm
	all_names %>>%
		filter(name %in% selected_names) %>>%
		select(column_name) %>>% ((.$column_name) -> selected)
	if(season_name < 2015){
		positions <- c('PG','SG','SF','PF','C')}else{
			c('G','F','C') -> positions
		}
	selected -> star_cols
	ifelse(input$position_selection_cor == positions,
				 position_name <- 'All Positions',
				 input$position_selection_cor %>>% (paste(.,collapse  = ", ")) -> position_name
	)
	call <- substitute(filter(plot_data.norm, !is.na(star_cols)), list(star_cols = as.name(star_cols)))
	eval(call) -> plot_data.norm
	plot_data.norm[, names(plot_data.norm)[names(plot_data.norm) %in% star_cols]] %>>%
		cor() %>>% (round(.,digits = 2)) -> pcor
	paste0("\n Correlation Matrix for ",position_name," ",s," Minimum of ",min_games," Games and ",min_minutes," minutes")-> plot_title-> plot_title
	col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
	corrplot(pcor, method="shade", shade.col=NA, tl.col="black", tl.srt=45,
					 col=col(200), addCoef.col="black", order="AOE",title = plot_title)	
},width = 900)

output$boxplot <- renderPlot({
	input$season_end %>>% as.numeric() -> season_name
	!input$season_end == (year(Sys.Date())+1) -> not_current_year
	(season_name - 1) %>>% ((paste0(.,'-',season_name))) -> s
	input$min_minutes -> min_minutes
	input$min_games -> min_games
	input$sort_boxplot %>>% as.character() -> sort_var_name
	all_names[all_names$name %in% sort_var_name,'column_name'] -> sort_var
	sort_var$column_name -> sort_var
	input$bin %>>% as.character() -> bin_var_name
	all_names[all_names$name %in% bin_var_name,'column_name'] -> bin_var
	bin_var$column_name -> bin_var
	
	if (not_current_year){
		'data/per_game_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
			filter(season == s) %>>%
			filter(g >= min_games) %>>% 
			select(-table_name,-data_source,-scrape_time) -> pg
		pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% (.$pos_1 -> pg$pos)
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
	merge(pg,adv,by = names(pg)[names(pg) %in% names(adv)], all.x = T, all.y = T) -> plot_data
	plot_data$age %>>% as.numeric() -> plot_data$age
	0 -> plot_data[is.na(plot_data)]
	if(plot_data$pos  %>>% length >= 3){
		position = c('PG','SG','SF','PF','C')
	}else{
		position = c('PG','SG','SF','PF','C','G','F')
	}
	substitute(plot_data %>>%
						 	filter(!is.na(sort_var)) %>>% filter(pos %in% position) %>>%
						 	arrange(desc(sort_var)),
						 list(sort_var = as.name(sort_var))) %>>% eval() -> plot_data
	
	paste0(s,", ",min_games," Game Minimum, ",min_minutes," Minute Minimum")	-> x_axis_title 
	y_title = sort_var
	plot_data$pos %>>% factor(levels = c('PG','SG',"G",'SF',"F",'PF','C')) -> plot_data$pos

	substitute(plot_data %>>%
						 	filter(!is.na(sort_var)) %>>% filter(pos %in% position) %>>% 
						 	select(player,pos,sort_var,bin_var),list(
						 		sort_var = as.name(sort_var),
						 		bin_var = as.name(bin_var)
						 		)) %>>% eval() -> plot_data
	
	

	 paste0(sort_var_name,' by ', bin_var_name,'\n',s,' Season\n',min_games,' Game Minimum\n',min_minutes,' Total Minute Minimum')-> plot_title 
	
	names(plot_data)[3] <- 'yvar'
	names(plot_data)[4] <- 'bin'
	ggthemr('fresh')
	plot_data$bin %>>% as.numeric() %>>% (cut_number(x = .,7)) %>>% factor() -> plot_data$bin_group
	ggplot(plot_data, aes(x=bin_group, y = yvar, fill = bin_group)) +
		geom_boxplot() + facet_grid(.~pos) + 
		scale_y_continuous(name = sort_var_name, breaks=seq(0,max(plot_data$yvar),by = max(plot_data$yvar)/20) %>>% round(digits = 2)) + 
		xlab(label = '') + 
		theme(strip.text = element_text(face="bold", size=rel(1)),
					strip.background = element_rect(fill="lightblue", colour="black", 
																					size=1)) +
		theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1, face = 'bold.italic')) + 
		
		ggtitle(plot_title)
},width = 820, height = 600)	

output$dist_plot <- renderPlot({
		input$players -> players
		input$season_end %>>% as.numeric() -> season_name
		!input$season_end == (year(Sys.Date())+1) -> not_current_year
		(season_name - 1) %>>% ((paste0(.,'-',season_name))) -> s
		input$min_minutes -> min_minutes
		input$min_games -> min_games
		input$sort_density %>>% as.character() -> sort_var_name
		input$plot_var %>>% as.character() -> selected_names
		input$position_selection -> pos_selected
		input$min_quantile -> min_quantile
		input$max_quantile -> max_quantile
		all_names[all_names$name %in% sort_var_name,'column_name'] -> sort_var
		sort_var$column_name -> sort_var
		if (not_current_year){
			'data/per_game_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
				filter(season == s) %>>%
				filter(g >= min_games) %>>% 
				select(-table_name,-data_source,-scrape_time) -> pg
			pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% (.$pos_1 -> pg$pos)
			
			
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
		merge(pg,adv,by = names(pg)[names(pg) %in% names(adv)], all.x = T, all.y = T) -> plot_data
		0 -> plot_data[is.na(plot_data)]
		substitute(plot_data %>>%
							 	filter(!is.na(sort_var)) %>>% filter(pos %in% c('PG',"G",'SG','SF',"F",'PF','C')) %>>% 
							 	filter(sort_var/max(sort_var,na.rm = T) >= min_quantile,sort_var/max(sort_var,na.rm = T) <= max_quantile) %>>%
							 	arrange(desc(sort_var)),
							 list(sort_var = as.name(sort_var))) %>>% eval() -> plot_data
		apply(plot_data[,!names(plot_data) %in% c('player','bref_team_id','season','pos')]
					,2, function(x) as.numeric(x)) -> plot_data[,!names(plot_data) %in% c('player','bref_team_id','season','pos')]
		plot_data -> plot_data.norm
		plot_data.norm %>>%
			select(-season,-bref_team_id) -> plot_data.norm
		call <- substitute(plot_data.norm %>>% 
											 	filter(!is.na(sort_var)) %>>% filter(pos %in% c('PG',"G",'SG','SF',"F",'PF','C')) %>>% 
											 	filter(sort_var/max(sort_var,na.rm = T) >= min_quantile,sort_var/max(sort_var,na.rm = max_quantile) <= max_quantile) %>>%
											 	summarise(mean = mean(sort_var,na.rm = T)), list(sort_var = as.name(sort_var))) 
		eval(call) -> mean
		mean$mean -> mean
		plot_data.norm$age %>>% factor(ordered = T) -> plot_data.norm$age
		call <- substitute(
			plot_data.norm %>>%
				filter(!is.na(sort_var)) %>>% filter(pos %in% c('PG','G','SG',"F",'SF','PF','C')) %>>% 
				select(player,pos,age,sort_var) %>>%
				filter(sort_var/max(sort_var,na.rm = T) >= min_quantile,sort_var/max(sort_var,na.rm = max_quantile) <= max_quantile) %>>%
				arrange(desc(sort_var)),
			list(sort_var = as.name(sort_var)))
		eval(call) -> plot_data.norm
		plot_data.norm$pos %>>% factor(levels = c("PG","SG","G","SF","PF","F","C")) -> plot_data.norm$pos
		'xvar' -> names(plot_data.norm)[4]
		plot_data.norm$pos %>>% factor(levels = c("PG","SG","G","SF","PF","F","C")) -> plot_data.norm$plot_data.norm
		
		paste0(sort_var_name, " Distributions by Position\n",
					 min_quantile * 100,'% Minimum Quantile, ', 100 * max_quantile,'% Maximum Quantile\n',
					 s," Season Minimum of ",min_games,' Games & ',min_minutes," Total Minutes")-> plot_title
		ggthemr('fresh')
		plot_data.norm %>>% 
			ggplot(aes(x=xvar,fill = pos)) +
			geom_density() + facet_grid(pos~.) +
			theme(strip.text = element_text(face="bold", size=rel(1)),
						strip.background = element_rect(fill="lightblue", colour="black", 
																						size=1)) +
			geom_vline(xintercept=mean) +
			scale_x_continuous(name = sort_var_name, breaks=seq(0,max(plot_data.norm$xvar),by = max(plot_data.norm$xvar)/20) %>>% round(digits = 2)) + 
			theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1, face = 'bold.italic')) + 
			ggtitle(plot_title)
	},width = 800, height = 520)
	
output$downloadPlotPDF <- downloadHandler(
		filename <- function() { input$players -> players
														 input$season_end %>>% as.numeric() -> season_name
														 (season_name - 1) %>>% ((paste0(.,'-',season_name))) -> s
														 input$min_minutes -> min_minutes
														 input$min_games -> min_games
														 input$sort %>>% as.character() -> sort_var_name
														 input$plot_var %>>% as.character() -> selected_names
														 ifelse(input$position_selection %>>% as.character()  == positions,
														 			 position_name <- 'All Positions',
														 			 input$position_selection %>>% (paste(.,collapse  = ", ")) -> position_name
														 )
														 paste0("NBA Player Statistics for ",position_name," in ", s, "\nPer Game, Top ", players, " Players Ranked on ", sort_var_name,'\nMinimum of ',min_games,' Games and ',min_minutes,' Total Minutes') -> title
														 paste0(title,'.pdf') },
		content <- function(file) {
			pdf(file, paper='A4r',width = 8.5,height = 11)
			## ---------------
			
			input$players -> players
			input$season_end %>>% as.numeric() -> season_name
			(season_name - 1) %>>% ((paste0(.,'-',season_name))) -> s
			input$min_minutes -> min_minutes
			input$min_games -> min_games
			input$sort -> sort_var_name
			input$plot_var -> selected_names
			!input$season_end == (year(Sys.Date())+1) -> not_current_year
			if (not_current_year){
				'data/per_game_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
					filter(season == s) %>>%
					filter(g >= min_games) %>>% 
					select(-table_name,-data_source,-scrape_time) -> pg
				pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% (.$pos_1 -> pg$pos)
				
				
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
			merge(pg,adv,by = names(pg)[names(pg) %in% names(adv)], all.x = T, all.y = T) -> plot_data
			0 -> plot_data[is.na(plot_data)]
			all_names[all_names$name %in% sort_var_name,'column_name'] -> sort_var
			sort_var$column_name -> sort_var
			call <- substitute(arrange(plot_data, desc(sort_var)), list(sort_var = as.name(sort_var)))
			eval(call) -> plot_data
			apply(plot_data[,!names(plot_data) %in% c('player','bref_team_id','season','pos')]
						,2, function(x) as.numeric(x)) -> plot_data[,!names(plot_data) %in% c('player','bref_team_id','season','pos')]
			all_names %>>%
				filter(name %in% selected_names) %>>%
				select(column_name) %>>% ((.$column_name) -> selected)
			selected -> star_cols
			
			all_names %>>%
				filter(name %in% selected_names) %>>%
				select(legend_name) %>>% ((.$legend_name) -> legend_names)
			# Set our color palette
			colors = c( "#AA66cc", "#FF4444" , "#000000","#1d2098",'#008000','#D0B240','#007198',
									'#F5811F','#008D97') -> pal
			# Set scale=FALSE so player stats are normalized over all players, not just the top 50
			add_legend <- function(...) {
				opar <- par(fig=c(0, 1, 0, 1), oma=c(5, 5, 5, 7), 
										mar=c(0, 0, 0, 0), new=TRUE)
				on.exit(par(opar))
				plot(0,0, type='n', bty='n', xaxt='n', yaxt='n')
				legend(...)
			}
			if(season_name < 2015){
				positions <- c('PG','SG','SF','PF','C')}else{
					c('G','F','C') -> positions
				}
			
			ifelse(input$position_selection == positions,
						 position_name <- 'All Positions',
						 input$position_selection %>>% (paste(.,collapse  = ", ")) -> position_name
			)
			
			paste0("NBA Player Statistics for ",position_name," in ", s, "\nPer Game, Top ", players, " Players Ranked on ", sort_var_name,'\nMinimum of ',min_games,' Games and ',min_minutes,' Total Minutes') -> title
			plot_data[1:players,(names(plot_data) %in% star_cols)] %>>% 
				stars(scale=T,radius = T,full = T,label=plot_data$player[1:players],flip.labels=F, frame.plot = TRUE,
							key.loc = c(15, 16.1),
							draw.segments=TRUE,col.segments=pal,ncol=10,cex=0.64,mar=c(1,0.5,4.5,0.5))
			title(main=title,
						cex.main=0.75)
			paste0("Created at ", Sys.time(), " Using Alex Bresler's NBA Player Analysis Shiny Application") -> label_2
			text(x=18,y=.5,labels=label_2,cex=0.5)
			## ---------------
			dev.off()
		},
		contentType = 'application/pdf' # MIME type of the image
	)	

output$downloadCOR <- downloadHandler(
	filename <- function() { input$players -> players
														 input$season_end %>>% as.numeric() -> season_name
														 (season_name - 1) %>>% ((paste0(.,'-',season_name))) -> s
														 input$min_minutes -> min_minutes
														 input$min_games -> min_games
														 input$sort %>>% as.character() -> sort_var_name
														 input$plot_var_cor %>>% as.character() -> selected_names
														 ifelse(input$position_selection %>>% as.character()  == positions,
														 			 position_name <- 'All Positions',
														 			 input$position_selection %>>% (paste(.,collapse  = ", ")) -> position_name
														 )
														 paste0("Correlation Matrix for ",position_name," ",s," Minimum of ",min_games," Games and ",min_minutes," minutes")  %>>% tolower() %>>% (gsub(pattern = '\\ ','_',.)) -> title
														 paste0(title,'.pdf') },
		content <- function(file) {
			pdf(file, paper='A4r',width = 8.5,height = 11)
			input$players -> players
			input$season_end %>>% as.numeric() -> season_name
			(season_name - 1) %>>% ((paste0(.,'-',season_name))) -> s
			input$min_minutes -> min_minutes
			input$min_games -> min_games
			input$sort %>>% as.character() -> sort_var_name
			input$plot_var_cor %>>% as.character() -> selected_names
			!input$season_end == (year(Sys.Date())+1) -> not_current_year
			if (not_current_year){
				'data/per_game_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
					filter(season == s) %>>%
					filter(g >= min_games) %>>% 
					select(-table_name,-data_source,-scrape_time) -> pg
				pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% (.$pos_1 -> pg$pos)
				
				
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
			merge(pg,adv,by = names(pg)[names(pg) %in% names(adv)], all.x = T, all.y = T) -> plot_data
			0 -> plot_data[is.na(plot_data)]
			all_names[all_names$name %in% sort_var_name,'column_name'] -> sort_var
			sort_var$column_name -> sort_var
			call <- substitute(arrange(plot_data, desc(sort_var)), list(sort_var = as.name(sort_var)))
			eval(call) -> plot_data
			apply(plot_data[,!names(plot_data) %in% c('player','bref_team_id','season','pos')]
						,2, function(x) as.numeric(x)) -> plot_data[,!names(plot_data) %in% c('player','bref_team_id','season','pos')]
			plot_data -> plot_data.norm
			plot_data.norm %>>%
				select(-season,-pos,-bref_team_id) -> plot_data.norm
			all_names %>>%
				filter(name %in% selected_names) %>>%
				select(column_name) %>>% ((.$column_name) -> selected)
			call <- substitute(filter(plot_data.norm, !is.na(star_cols)), list(star_cols = as.name(star_cols)))
			eval(call) -> plot_data.norm
			if(season_name < 2015){
				positions <- c('PG','SG','SF','PF','C')}else{
					c('G','F','C') -> positions
				}
			selected -> star_cols
			ifelse(input$position_selection_cor == positions,
						 position_name <- 'All Positions',
						 input$position_selection_cor %>>% (paste(.,collapse  = ", ")) -> position_name
			)
			plot_data.norm[, names(plot_data.norm)[names(plot_data.norm) %in% star_cols]] %>>%
				cor() %>>% (round(.,digits = 2)) -> pcor
			paste0("\n Correlation Matrix for ",position_name," ",s," Minimum of ",min_games," Games and ",min_minutes," minutes")-> plot_title
			col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
			corrplot(pcor, method="shade", shade.col=NA, tl.col="black", tl.srt=45,
							 col=col(200), addCoef.col="black", order="AOE",title = plot_title)
			## ---------------
			dev.off()
		},
		contentType = 'application/pdf' # MIME type of the image
	)
	
	output$downloadCSV <- 
		downloadHandler(
			filename <- function() { input$players -> players
															 input$season_end %>>% as.numeric() -> season_name
															 (season_name - 1) %>>% ((paste0(.,'-',season_name))) -> s
															 input$min_minutes -> min_minutes
															 input$min_games -> min_games
															 input$sort_table %>>% as.character() -> sort_var_name
															 input$position_selection_table -> pos_selected
															 ifelse(input$position_selection_table %>>% as.character()  == positions,
															 			 position_name <- 'All Positions',
															 			 input$position_selection_table %>>% (paste(.,collapse  = ", ")) -> position_name
															 )
															 paste0("NBA Player Statistics Star Chart ",position_name," in ", s, "Per Game, Top ", players, " Players Ranked on ", sort_var_name) %>>% towlower() %>>% (gsub(pattern = '\\ ','_',.)) -> title
															 paste0(title,'.csv') },
			content <- function(file) {
				input$players -> players
				input$season_end %>>% as.numeric() -> season_name
				(season_name - 1) %>>% ((paste0(.,'-',season_name))) -> s
				input$min_minutes -> min_minutes
				input$min_games -> min_games
				input$sort_table %>>% as.character() -> sort_var_name
				input$plot_var_table %>>% as.character() -> selected_names
				!input$season_end == (year(Sys.Date())+1) -> not_current_year
				if (not_current_year){
					'data/per_game_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
						filter(season == s) %>>%
						filter(g >= min_games) %>>% 
						select(-table_name,-data_source,-scrape_time) -> pg
					pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% (.$pos_1 -> pg$pos)
					
					
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
				merge(pg,adv,by = names(pg)[names(pg) %in% names(adv)], all.x = T, all.y = T) -> plot_data
				0 -> plot_data[is.na(plot_data)]
				all_names[all_names$name %in% sort_var_name,'column_name'] -> sort_var
				sort_var$column_name -> sort_var
				call <- substitute(arrange(plot_data, desc(sort_var)), list(sort_var = as.name(sort_var)))
				eval(call) -> plot_data
				apply(plot_data[,!names(plot_data) %in% c('player','bref_team_id','season','pos')]
							,2, function(x) as.numeric(x)) -> plot_data[,!names(plot_data) %in% c('player','bref_team_id','season','pos')]
				
				input$plot_var_table %>>% as.character() -> selected_names
				all_names %>>%
					filter(name %in% selected_names) %>>%
					select(column_name) %>>% ((.$column_name) -> selected)
				c('season','player','pos',selected) ->selected_columns
				
				plot_data[,selected_columns] -> plot_data
				write.csv(
					x=plot_data,file, row.names = F)
			}
		)
	output$table = renderDataTable({
		input$players -> players
		input$season_end %>>% as.numeric() -> season_name
		!input$season_end == (year(Sys.Date())+1) -> not_current_year
		(season_name - 1) %>>% ((paste0(.,'-',season_name))) -> s
		input$min_minutes -> min_minutes
		input$min_games -> min_games
		input$sort_table %>>% as.character() -> sort_var_name
		input$plot_var_table %>>% as.character() -> selected_names
		input$position_selection_table -> pos_selected
		
		if (not_current_year){
			'data/per_game_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
				filter(season == s) %>>%
				filter(g >= min_games) %>>% 
				select(-table_name,-data_source,-scrape_time) -> pg
			pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% (.$pos_1 -> pg$pos)
			
			
		} 		else{
			source('src/brefSeasonStatFunctions.R',keep.source = T)
			getBREFSeasonLeaders(year_end_season = season_name,stat_type = "Per Game") -> pg
			pg[2] %>>% data.frame() %>>% tbl_df() %>>% tbl_df() %>>%
				filter(season == s) %>>%
				filter(g >= min_games) %>>% 
				select(-table_name,-data_source,-scrape_time) -> pg
			pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% 
				(.$pos_1 -> pg$pos)
		}
		
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
		merge(pg,adv,by = names(pg)[names(pg) %in% names(adv)], all.x = T, all.y = T) -> plot_data
		0 -> plot_data[is.na(plot_data)]
		plot_data %>>%
			filter(pos %in% pos_selected) -> plot_data	
		all_names[all_names$name %in% sort_var_name,'column_name'] -> sort_var
		sort_var$column_name -> sort_var
		call <- substitute(arrange(plot_data, desc(sort_var)), list(sort_var = as.name(sort_var)))
		eval(call) -> plot_data
		apply(plot_data[,!names(plot_data) %in% c('player','bref_team_id','season','pos')]
					,2, function(x) as.numeric(x)) -> plot_data[,!names(plot_data) %in% c('player','bref_team_id','season','pos')]
		input$plot_var_table %>>% as.character() -> selected_names
		all_names %>>%
			filter(name %in% selected_names) %>>%
			select(column_name) %>>% ((.$column_name) -> selected)
		c('season','player','pos',selected) -> selected_columns
		plot_data[,sapply(plot_data,is.character)] <- sapply(
			plot_data[,sapply(plot_data,is.character)],
			iconv,"WINDOWS-1252","UTF-8")
		plot_data[,selected_columns]
	},   options = list(orderClasses = TRUE,lengthMenu = c(10, 25, 50), pageLength = 10)
	)}
)