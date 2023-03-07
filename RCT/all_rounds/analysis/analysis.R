### IMPORTANT NOTE: dta_sept and dta_dec were created to run the analysis on mock/midline reports
### they do not have raw data but only the constructed variables merged to the raw data

rm(list=ls())
library(fixest)
library(car)
path <- getwd()
path <- strsplit(path, "/analysis")[[1]]
dta_sept <- read.csv(paste(path,"data/midline_sept.csv", sep="/"))
dta_dec <- read.csv(paste(path,"data/midline_dec.csv", sep="/"))
dta_sept$survey <- 1
dta_dec$survey <- 2
### I should just be able to stack them

pool <- data.frame(rbind(cbind(dta_sept$farmer_ID, dta_sept$fe_vil,dta_sept$stock_maize_abs,dta_sept$sold_maize,dta_sept$sold_maize_kg,dta_sept$price_maize,dta_sept$bought_maize,dta_sept$bought_maize_amt,dta_sept$bought_maize_price,dta_sept$survey,dta_sept$treatment),
cbind(dta_dec$farmer_ID, dta_dec$fe_vil,dta_dec$stock_maize_abs,dta_dec$sold_maize,dta_dec$sold_maize_kg,dta_dec$price_maize,dta_dec$bought_maize,dta_dec$bought_maize_amt,dta_dec$bought_maize_price,dta_dec$survey,dta_dec$treatment)))

names(pool) <- c("farmer_id", "fe_vil","stock_maize_abs","sold_maize","sold_maize_kg","price_maize","bought_maize","bought_maize_amt","bought_maize_price","survey","treatment")
##restrict to farmers that grow maize
pool <- merge(pool, dta_sept[c("farmer_ID","q39")], by.x="farmer_id",by.y="farmer_ID", all.x=T)
pool <- subset(pool, q39!="88")

pool$survey <- as.factor(pool$survey)
pool$survey <- relevel(pool$survey, ref = "sept")
summary(lm(stock_maize_abs~treatment*survey+fe_vil,data=pool))

fe_ols <- feols(as.numeric(as.character(stock_maize_abs))~treatment*survey |fe_vil, pool)
summary(fe_ols)



fe_ols <- feols(as.numeric(as.character(sold_maize)=="TRUE") ~treatment*survey |fe_vil, pool)
summary(fe_ols)

fe_ols <- feols(as.numeric(as.character(sold_maize_kg))~treatment*survey |fe_vil, pool)
summary(fe_ols)

fe_ols <- feols(as.numeric(as.character(price_maize))~treatment*survey |fe_vil, pool)
summary(fe_ols)

fe_ols <- feols(as.numeric(as.character(bought_maize)=="TRUE") ~treatment*survey |fe_vil, pool)
summary(fe_ols)
#fe_ols <- feols(as.numeric(as.character(bought_maize)=="TRUE") ~treatment*survey |fe_vil, pool[pool$survey==2,])
#summary(fe_ols)

fe_ols <- feols(as.numeric(as.character(bought_maize)=="TRUE") ~treatment*survey | fe_vil , pool)
summary(fe_ols)
fe_ols <- feols(as.numeric(as.character(bought_maize)=="TRUE") ~treatment*survey , pool[pool$survey==2,])
summary(fe_ols)

linearHypothesis(fe_ols, "treatmentT1 + treatmentT1:survey2  = 0")
linearHypothesis(fe_ols, "treatmentT2 + treatmentT2:survey2  = 0")

fe_ols <- feols(as.numeric(as.character(bought_maize_amt))~treatment*survey |fe_vil, pool)
summary(fe_ols)
