library(XML)
library(RCurl)
library(reshape2)
library(stringr)

agents.url <- "http://hoopshype.com/agents.htm"
tables <- readHTMLTable(agents.url,skip.rows=1:3)
agencies <- data.frame(tables[10])
names(agencies) <- c('agency','total_salary')
agents <- data.frame(tables[11])
agents <- agents[-1,]
names(agents) <- c('agent','total_salary','players','all_stars','max_contracts')

agents.urls <- read.csv(list.files()[1])
urls <- as.character(agents.urls$agent_hoopshype_url)
names(urls) <- as.character(agents.urls$agent)

readHTMLTable(urls[1],trim=T,skip.rows=1)[12]

agents <- as.character(agents.urls$agent)

all.agents <- data.frame() #create blank data table
#loop
for(agent in agents){
  tables <- readHTMLTable(urls[agent],skip.rows=1,trim=T)
  agents.players <- tables[[12]] 
  agents.players$Agent <- agent
  all.agents <- rbind(all.agents, agents.players)
  rm(agents.players, tables)
}

names(all.agents) <- c('player','2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','agent')
all.agents <- all.agents[!grepl("TOTALS",x=as.character(all.agents$player)),]	
all.agents.players <- merge(all.agents, agents.urls[,c('agent','agency')],all.x=T)
write.csv(all.agents.players,'All Agents Players 2013-2018.csv')

all.agents.players <- read.csv(list.files()[1])
players_agents_agencies <- unique(all.agents.players[,c(1:3)])

melted.agents.players <- melt(all.agents.players,id.vars=names(all.agents.players)[1:3],na.rm=F)
names(melted.agents.players)[4:5] <- c('season','salary')
melted.agents.players$salary_amenisty <- grepl("\\*",melted.agents.players$salary)
melted.agents.players$salary <- str_trim(gsub("[\\*]","",melted.agents.players$salary))
write.csv(melted.agents.players,'Players Agents List 2013-2018.csv')
