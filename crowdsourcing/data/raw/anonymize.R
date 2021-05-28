#run in crowdsourcing/data/raw/

rm(list=ls())
path <- getwd()

dta <- read.csv("soja_data.csv")
dta2 <- read.csv("maize_data.csv")
dta2[c("TAname","Date","Time")] <- NULL 
dta$crop <- "soy_bean"
dta2$crop <- "maize"

dta <- rbind(dta,dta2)
dta[c("EPA","GVH")] <- NULL
