#Basketball ReferenceScraper for All Teams in 1 Data Frame

library(RCurl)
library(XML)
t<-"http://www.basketball-reference.com/friv/birthplaces.cgi?country=TT&state="
tables<-readHTMLTable(t)

url<-"http://www.basketball-reference.com/friv/birthplaces.cgi"
stem <- "http://www.basketball-reference.com"
countries<-htmlParse(getURL(url), asText=T)
countries<-xpathSApply(countries,"//*/a[contains(@href,'birthplaces.cgi?country=')]", xmlAttrs)
countries<-gsub("birthplaces.cgi?country=(.*)/", "\\1", countries)

urls<-paste0(stem, countries)

names(countries)<-NULL
names(urls) <- countries
names(urls) <-gsub("[\\/friv/birthplaces.cgi?country=]","",names(urls))
urls
#Create a data frame with all the results

results <- data.frame() #create blank data table
#loop
for(i in urls){
	tables <- readHTMLTable(i)
	country.results <- tables[[2]] 
	country.results$LocationCode <- i
	results <- rbind(results, country.results)
}

results$CountryUrl<-results$LocationCode
results$LocationCode<-gsub("[\\http://www.basketball-reference.com/friv/birthplaces.cgi?country=-]","",results$LocationCode)
results$LocationCode<-gsub("[&]","",results$LocationCode)
results$LocationCode<-gsub("US","",results$LocationCode)
locations<-unique(results$LocationCode)
write.cb(locations)
ccwrite.csv(results, file="All NBA-ABA Countries.csv")

url<-"http://www.basketball-reference.com/friv/birthplaces.cgi"
tables<-readHTMLTable(url)
tables<-data.frame(tables[1])
tables<-tables[-11,]
tables

page<-htmlParse(getURL(url), asText=T)
xpathSApply(page,"//a[contains(@href,'birthplaces.cgi?country=')]/text()", xmlAttrs)
players<-read.csv(list.files()[1])
players$PlayerWikipedia<-
players$PlayerDBPediaData<-paste('http://live.dbpedia.org/data/',gsub("[ ]","_",players$Player),".json",sep="")

write.csv(players,"All NBA-ABA Players by Birthplace.csv")
us<-players[players$CountryCode=='US',]
non.us<-players[!players$CountryCode=='US',]