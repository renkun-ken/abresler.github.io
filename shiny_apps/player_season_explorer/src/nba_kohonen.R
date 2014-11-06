packages = c('dplyr','pipeR','reshape2','data.table','kohonen')
lapply(packages,library,character.only = T)
s = "2013-2014"
min_games = 10
min_minutes = 400
setwd("~/Desktop/Github/asb_shiny_apps/nba/explorer/v3")
'data/per_game_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
	filter(season == s) %>>%
	filter(g >= min_games) %>>% 
	select(-table_name,-data_source,-scrape_time) -> pg
pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% (.$pos_1 -> pg$pos)
'data/advanced_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
	filter(season == s) %>>%
	filter(g >= min_games) %>>% 
	select(-table_name,-data_source,-scrape_time) -> adv
adv$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% (.$pos_1 -> adv$pos)
set.seed(7)
xdim = 10
ydim = 20
scaled_per_minute = T
somgrid(xdim = xdim, ydim = ydim, topo = "hexagonal") ->  #could be 10x10
	som_grid
pg %>>% select(mp,fg:pts,-ft.,-x3p.,-x2p.) -> df
adv %>>%
	select(orb.:tov.,usg.) %>%
	cbind(df,.) -> df
scaled_per_minute = F
c('pts','ast.','blk.','fg','fga','tov.','stl.','fga','x3p','x3pa','drb.','orb.','trb.','usg.') -> columns_pm
c('pts','ast','blk','fg','fga','tov','stl','fga','x3p','x3pa','drb','orb','trb','usg.') -> columns_pg
if(scaled_per_minute){
	apply(df[,2:ncol(df)],2, function(x) x/df$mp) %>>% data.frame() %>>% tbl_df() -> df_matrix
	df_matrix[,columns_pm] -> df_matrix
} else{
	df %>>% data.frame() %>>% tbl_df() -> df_matrix
	df_matrix[,columns_pg] -> df_matrix
}
source('src/coolBlueHotRed.R')
source('src/plotHeatMap.R')
df_matrix %>>% as.matrix()  %>>% scale() -> nba_scaled_pg
names(df_matrix) %>>% tolower() -> names(nba_scaled_pg)
adv[,c('player','pos')] %>>% (cbind(.,df))-> df
df %>>% tbl_df() -> df
nba_scaled_pg %>>% 
	som(grid = som_grid,
			rlen = 100, # could be 100
			alpha = c(.05,.01),
			keep.data = T,
			n.hood = 'circular') -> som_model_nba
som_model_nba %>>% plot(type = "changes")
som_model_nba %>>% plot(type = "counts",palette.name = coolBlueHotRed)
som_model_nba %>>% plot(type = "codes", palette.name = coolBlueHotRed)
som_model_nba %>>% plot(type = "dist.neighbours",palette.name = coolBlueHotRed)

som_model_nba %>>% plot(type = "property",property = som_model_nba$codes[,4],main = 
													 	names(som_model_nba$data)[4],palette.name = coolBlueHotRed)
var <- 2
som_model_nba %>>% plot(type = "property", 
													 property = som_model_nba$codes[,var], main=names(som_model_nba$data)[var], palette.name=coolBlueHotRed)

# Plot the original scale heatmap for a variable from the training set:
var <- 26 #define the variable to plot
var_unscaled <- aggregate(as.numeric(nba_scaled_pg[,var]), by=list(som_model_nba$unit.classif), FUN=mean, simplify=TRUE)[,2]
som_model_nba %>>% plot(type = "property", 
													 property=som_model_nba$codes[,var], main=names(nba_scaled_pg)[var], palette.name=coolBlueHotRed)
add.cluster.boundaries(som_model_nba, som_cluster)
som_model_nba$unit.classif
#interactive plot

#som_model_nba %>>% (plotHeatMap(som_model = .,data = df_matrix %>>% data.frame, 
#																	 variable=0))

returnClusterPlot <- function(){
	mydata <- som_model_nba$codes
	wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
	for (i in 2:15) wss[i] <- sum(kmeans(mydata,
																			 centers=i)$withinss)
	par(mar=c(5.1,4.1,4.1,2.1))
	plot(1:15, wss, type="b", xlab="Number of Clusters",
			 ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)")
}
returnClusterPlot()

## use hierarchical clustering to cluster the codebook vectors
clusters = 6
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
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2','grey','pink','tomato1','')
som_model_nba %>>% plot(type="mapping", bgcol = pretty_palette[som_cluster], main = title)
add.cluster.boundaries(som_model_nba, som_cluster)
#show the same plot with the codes instead of just colours
som_model_nba %>>% plot(type="codes", bgcol = pretty_palette[som_cluster], main = title)
add.cluster.boundaries(som_model_nba, som_cluster)

df %>>% 
	filter(player %in% c('Carmelo Anthony','J.R. Smith','Brook Lopez','LaMarcus Aldridge')) %>>%
	select(player,pos,group,cluster)

df %>>% 
	group_by(cluster)  %>>%
	select(cluster) %>>%
	summarise(n())
df %>>%
	 filter(cluster %in% c(4,6)) %>>% select(player,pos,cluster,group)
df %>>% filter(group == 185)
