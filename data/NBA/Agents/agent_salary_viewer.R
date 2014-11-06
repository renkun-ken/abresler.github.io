library(data.table)
salaries <- read.csv(list.files()[1])
ds <- data.table(salaries)
names(ds)
ds$season <- as.character(ds$season)
ds$salary <- as.numeric(ds$salary)

ds[,salaries = sum(salary),list(id_contract_type)]
sum(ds$salary)/1000000
t.s <- aggregate(salary~id_contract_type,ds,sum)
t.s <- t.s[!t.s$id_contract_type == "U",]
t.s$salary <- t.s$salary/1000000
sum(t.s$salary)
guaranteed <- salaries[!salaries$id_contract_type == "U",]
guaranteed$agency <- as.character(guaranteed$agency)

agencies <- aggregate(salary~agency,guaranteed,sum)
agencies$salary <- agencies$salary/1000000
agencies <- agencies[order(agencies$salary,decreasing=T),]
sum(head(agencies$salary),10)
