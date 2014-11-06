library(ggplot2)
source('src/kmeans_cluster.R')
all %>>%
	 tbl_df() -> mds
mds %>>%
	select(-age,-player,-bref_team_id,-pos,-g,-season,-gs,fg:x2p.1,orb.:tov.) %>>%
	scale() %>%
	dist() -> d

d %>>% cmdscale(eig = T, 2) -> fit
x <- fit$points[, 1]
y <- fit$points[, 2]
data.frame(x,y, position = mds$pos, player = mds$player) -> df

title = paste0(s,' Season Player MDS Clusters\n',min_games,' Game Minimum, ',min_minutes,' Total Minute Minimum')
df %>>% ggplot(aes(x = x,y = y,label = player, colour = position)) + 
	geom_point() +
	geom_text(vjust = -.2, size = 4) +
	scale_colour_manual(values = c('red','purple','#008000','blue','black')
										, labels = c('PG','SG','SF','PF','C'), name = 'Position') + 
	ggtitle(label = title) -> gg
gg

## Scaled

mds %>>%
	select(-age,-player,-bref_team_id,-pos,-g,-season,-gs) -> mds_scale
apply(mds_scale[,names(mds_scale) %in% c('fg','fga','x3p','x3pa','x2p','x2pa','ft','fta','orb','drb','trb','ast','stl','blk','tov','pf','pts')],2, 
			function(x) x / mds_scale$mp) -> mds_scale[,names(mds_scale) %in% c('fg','fga','x3p','x3pa','x2p','x2pa','ft','fta','orb','drb','trb','ast','stl','blk','tov','pf','pts')]
#select(-mp,fg:pts,orb.:tov.,-usg.:-ws.48)
mds_scale  %>% select(-mp,fg:pts,orb.:tov.,-usg.:-ws.48) %>>% 
	scale() %>%  dist() %>>% 
	cmdscale(eig = T, 2) -> fit_scale
x_scale <- fit_scale$points[, 1]
y_scale <- fit_scale$points[, 2]
data.frame(x = x_scale,y = y_scale, position = mds$pos, player = mds$player, age = mds$age) -> df_scale

title = paste0(s,' Season Player MDS Clusters, Scaled Per Minute\n',min_games,' Game Minimum, ',min_minutes,' Total Minute Minimum')
df %>>% ggplot(aes(x = x,y = y,label = player, colour = position)) + 
	geom_point() +
	geom_text(vjust = -.2, size = 4) +
	scale_colour_manual(values = c('red','purple','#008000','blue','black')
											, labels = c('PG','SG','SF','PF','C'), name = 'Position') + 
	ggtitle(label = title) -> gg
gg
