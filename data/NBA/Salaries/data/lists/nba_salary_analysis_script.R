#business of basketball salary analysis
library(data.table)
library(reshape2)
library(stringr)
library(lubridate)
library(rCharts)

options(stringsAsFactors=)
setwd("~/Desktop/Github/Aragorn/data/Data/Datasets for Launch/Sports/NBA/Salaries/data")
id_contract <- read.csv(list.files()[1])
setwd("~/Desktop/Github/Aragorn/data/Data/Datasets for Launch/Sports/NBA/Salaries/data/lists")
salaries_list <- read.csv(list.files()[1])

salaries_dt <- data.table(salaries_list)
cols <- names(salaries_list)[c(1:2,5,9,11)]
setkeyv(x=salaries_dt,cols)

agency_salaries <- data.frame(salaries_dt[ , list( salaries_millions = sum(salary)
	), by = list(agency)])
agency_salaries <- agency_salaries[order(agency_salaries$salaries_millions,decreasing=T),]

top_agencies <- agency_salaries[1:18,1]
salaries_list[!salaries_list$agency %in% top_agencies,'agency'] <- 'Other'

cap_list <- subset(salaries_list, salaries_list$salary_amnesty == F )
cap_dt <- data.table(cap_list)
setkeyv(x=cap_dt,cols)

cap_salaries <- data.frame(cap_dt[ , list(salary_millions = sum(salary)/1000000), by = list(team, season, agency, id_contract_type)])
totals <- (aggregate(salary_millions~team,data=cap_salaries,sum)
names(totals)[2] <- 'total_salary_millions'
cap_salaries <- merge(cap_salaries,totals)
cap_salaries <- cap_salaries[order(cap_salaries$total_salary_millions,decreasing=T),]
