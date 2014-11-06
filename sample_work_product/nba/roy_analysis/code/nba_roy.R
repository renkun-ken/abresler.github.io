## Rookie of the Year Analysis
packages = c('RCurl','XML','dplyr','stringr','lubridate','reshape2')
library()
lapply(packages, require, character.only=T)
url = 'http://www.basketball-reference.com/awards/roy.html'
tables = readHTMLTable(url, trim = T)
page = htmlParse(getURL(url), asText=T)
nba =
	data.frame(tables[1])
nba = nba[4:67,]
names(nba) = c('season','league', 'player', 'voting','age','team_id','games_played','min_pg','pts_pg','trb_pg','ast_pg','stl_pg','blk_pg','fg_pct','3pt_pct','ft_pct','ws','ws_per48')

teams_ids <- 
	xpathSApply(page,'//*[(@id = "NBA-roy")]//td[(((count(preceding-sibling::*) + 1) = 6) and parent::*)]//a',xmlValue)

seasons = xpathSApply(page,'//*[(@id = "NBA-roy")]//td[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]//a',xmlValue)
teams_seasons_links <- 
	xpathSApply(page,'//*[(@id = "NBA-roy")]//td[(((count(preceding-sibling::*) + 1) = 6) and parent::*)]//a',xmlAttrs)
names(teams_seasons_links) = NULL
teams_seasons_links = paste0('http://www.basketball-reference.com/',teams_seasons_links)
teams_seasons = data.frame('season' = seasons,'team_id'= teams_ids, 'team_season_url' =  teams_seasons_links)

players = colsplit(nba$player,'\\(',c('player_name','tie'))
players$tie = gsub('\\)','',players$tie)
players$player_name = Trim(players$player_name)
roy = cbind(players, nba)
roy$player = NULL
roy$voting = NULL
roy$winner_tie = grepl('Tie',roy$tie, ignore.case = T)
roy$tie = NULL
ds = merge(roy, teams_seasons)
write.csv(ds,'nba_roy_data.csv', row.names = F)
