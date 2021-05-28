#run in crowdsourcing/data/raw/

rm(list=ls())
path <- getwd()
path <- strsplit(path, "/raw")[[1]]

dta <- read.csv("soja_data.csv")
dta2 <- read.csv("maize_data.csv")
dta2[c("TAname","Date","Time")] <- NULL 
dta$crop <- "soy_bean"
dta2$crop <- "maize"

dta <- rbind(dta,dta2)
dta[c("EPA","GVH")] <- NULL

write.csv(dta,file = paste(path,"public/crowd_sourced_all.csv", sep="/"), row.names=FALSE)
