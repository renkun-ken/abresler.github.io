library(XML)
url12 <- 'http://www.forbes.com/nba-valuations/list/'
table <- readHTMLTable(url12)
table
tables12 <- data.frame(table)
names(tables12) <- c('rank','team','value','value_change','debt_percent','revenue','operating_income')
tables12$year <- 2012

url11 <- 'http://web.archive.org/web/20111202142604/http://www.forbes.com/nba-valuations/list'
table <- readHTMLTable(url11)
table
tables11 <- data.frame(table[4])
names(tables11) <- c('rank','team','value','value_change','debt_percent','revenue','operating_income')
tables11$year <- 2011

url10<-'http://www.forbes.com/lists/2011/32/basketball-valuations-11_rank.html'
table <- readHTMLTable(url10)
tables10 <- data.frame(table)
names(tables10) <- c('rank','team','value','value_change','debt_percent','revenue','operating_income')
tables10$year <- 2010

url09<-'http://web.archive.org/web/20101223133608/http://www.forbes.com/lists/2009/32/basketball-values-09_NBA-Team-Valuations_Rank.html'
table <- readHTMLTable(url09)
tables09 <- data.frame(table[1])
names(tables09) <- c('rank','team','value','value_change','debt_percent','revenue','operating_income')
tables09$year <- 2009
tables09<-tables09[-1,]

url08 <-'http://www.forbes.com/lists/2008/32/nba08_NBA-Team-Valuations_Rank.html'
table <- readHTMLTable(url08)
table[1]
tables08 <- data.frame(table[1])
names(tables08) <- c('rank','team','value','value_change','debt_percent','revenue','operating_income')
tables08$year <- 2008
tables08 <- tables08[-1,]

url07 <- 'http://www.forbes.com/lists/2007/32/biz_07nba_NBA-Team-Valuations_Rank.html'
table <- readHTMLTable(url07)
table[7]
tables07 <- data.frame(table[7])
names(tables07) <- c('rank','team','value','value_change','debt_percent','revenue','operating_income')
tables07$year <- 2007
tables07 <- tables07[-1,]


url06 <- 'http://www.forbes.com/lists/2006/32/biz_06nba_NBA-Team-Valuations_Rank.html'
table <- readHTMLTable(url06)
table[6]
tables06 <- data.frame(table[6])
names(tables06) <- c('rank','team','value','value_change','debt_percent','revenue','operating_income')
tables06$year <- 2006
tables06 <- tables06[-1,]

nbatables06to12<-rbind(tables06,tables07,tables08,tables09,tables10,tables11,tables12)
write.csv(tables06to12,'NBA Values 2006 to 2012.csv')
library(gdata)
nba_team_data = ldply(list.files(pattern = "xls"), function(fname) {
	dum = read.xls(fname,sheet=1)
	dum$fname = fname  # adds the filename it was read from as a column
	return(dum)
})



nba_franchise_values <- merge(nba_team_data,nbatables06to12, all.x=T, all.y=T)
nba_franchise_values$value <- round(as.numeric(gsub('\\,','',nba_franchise_values$value)),digits=3)
nba_franchise_values$revenue <- as.numeric(nba_franchise_values$revenue)
nba_franchise_values$operating_income <- as.numeric(nba_franchise_values$operating_income)
nba_franchise_values$expense<-nba_franchise_values$revenue-nba_franchise_values$operating_income
nba_franchise_values$value_change <- round(as.numeric(nba_franchise_values$value_change) / 100 , 3)
nba_franchise_values$debt <- (as.numeric(nba_franchise_values$debt_percent) / 100) * nba_franchise_values$value
names(nba_franchise_values)
nba_franchise_values <- nba_franchise_values[ , c(2,8,1,3:4,12,7,5,11,6,9:10)]
nba_franchise_values <- nba_franchise_values[order(nba_franchise_values$year, decreasing=T) , ]
nba_franchise_values$revenue_multiple <- nba_franchise_values$value/nba_franchise_values$revenue
nba_franchise_values$ebitda_multiple <- nba_franchise_values$value/nba_franchise_values$operating_income
write.csv(nba_franchise_values, 'NBA Franchise Values 1991 to 2012.csv')