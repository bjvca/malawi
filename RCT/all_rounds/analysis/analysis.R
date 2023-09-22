### IMPORTANT NOTE: dta_sept and dta_dec were created to run the analysis on mock/midline reports
### they do not have raw data but only the constructed variables merged to the raw data

rm(list=ls())
library(fixest)
library(car)
path <- getwd()
path <- strsplit(path, "/analysis")[[1]]
dta_sept <- read.csv(paste(path,"data/midline_sept.csv", sep="/"))
dta_dec <- read.csv(paste(path,"data/midline_dec.csv", sep="/"))
dta_apr <- read.csv(paste(path,"data/endline2023.csv", sep="/"))
dta_sept$survey <- 1
dta_dec$survey <- 2
dta_apr$survey <- 3
### I should just be able to stack them

pool <- data.frame(
  rbind(
    cbind(dta_sept$farmer_ID, dta_sept$fe_vil, dta_sept$stock_maize_abs, dta_sept$sold_maize, dta_sept$sold_maize_kg, dta_sept$price_maize, dta_sept$bought_maize, dta_sept$bought_maize_amt, dta_sept$bought_maize_price, dta_sept$price_dec_maize, dta_sept$survey, dta_sept$treatment),
    cbind(dta_dec$farmer_ID, dta_dec$fe_vil, dta_dec$stock_maize_abs, dta_dec$sold_maize, dta_dec$sold_maize_kg, dta_dec$price_maize, dta_dec$bought_maize, dta_dec$bought_maize_amt, dta_dec$bought_maize_price, dta_dec$price_dec_maize, dta_dec$survey, dta_dec$treatment),
    cbind(dta_apr$farmer_ID, dta_apr$fe_vil, dta_apr$stock_maize_abs, dta_apr$sold_maize, dta_apr$sold_maize_kg, dta_apr$price_maize, dta_apr$bought_maize, dta_apr$bought_maize_amt, dta_apr$bought_maize_price, dta_apr$price_dec_maize, dta_apr$survey, dta_apr$treatment)
  )
)

names(pool) <- c("farmer_id", "fe_vil","stock_maize_abs","sold_maize","sold_maize_kg","price_maize","bought_maize","bought_maize_amt","bought_maize_price","price_exp_maize","survey","treatment")
##restrict to farmers that grow maize
pool <- merge(pool, dta_sept[c("farmer_ID","q39")], by.x="farmer_id",by.y="farmer_ID", all.x=T)
pool <- subset(pool, q39!="88")

pool$survey_1 <- 0
pool$survey_2 <- 0
pool$survey_3 <- 0

pool$survey_1[pool$survey==1] <- 1 
pool$survey_2[pool$survey==2] <- 1
pool$survey_3[pool$survey==3] <- 1

pool$T1_R1 <- 0
pool$T1_R2 <- 0
pool$T1_R3 <- 0
pool$T2_R1 <- 0
pool$T2_R2 <- 0
pool$T2_R3 <- 0

pool$T1_R1[pool$survey==1 & pool$treatment=="T1"] <- 1
pool$T2_R1[pool$survey==1 & pool$treatment=="T2"] <- 1
pool$T1_R2[pool$survey==2 & pool$treatment=="T1"] <- 1
pool$T2_R2[pool$survey==2 & pool$treatment=="T2"] <- 1
pool$T1_R3[pool$survey==3 & pool$treatment=="T1"] <- 1
pool$T2_R3[pool$survey==3 & pool$treatment=="T2"] <- 1


###This is for maize
pool$stock_maize_abs <- as.numeric(as.character(pool$stock_maize_abs))
pool$sold_maize_kg <- as.numeric(as.character(pool$sold_maize_kg))
pool$price_maize <- as.numeric(as.character(pool$price_maize))
pool$price_maize[pool$price_maize<100] <- NA
pool$bought_maize_amt <- as.numeric(as.character(pool$bought_maize_amt))
pool$bought_maize_price <- as.numeric(as.character(pool$bought_maize_price))
pool$bought_maize_price[pool$bought_maize_price<100] <- NA
pool$price_exp_maize <- as.numeric(as.character(pool$price_exp_maize))

pool$bought_maize <- as.numeric(pool$bought_maize==TRUE)
pool$sold_maize <- as.numeric(pool$sold_maize==TRUE)

pool$revenue <- pool$sold_maize_kg*pool$price_maize 
pool$revenue[is.na(pool$revenue)] <- 0
pool$expenses <- pool$bought_maize_amt*pool$bought_maize_price
pool$expenses[is.na(pool$expenses)] <- 0
pool$balance <- pool$revenue - pool$expenses 

##prices (at transaction level)
feols(as.formula( "price_maize~treatment|fe_vil"),cluster = ~farmer_id, pool)
feols(as.formula( "bought_maize_price~treatment|fe_vil"),cluster = ~farmer_id, pool)

feols(as.formula( "revenue~treatment|fe_vil"),cluster = ~farmer_id, pool)
feols(as.formula( "expenses~treatment|fe_vil"),cluster = ~farmer_id, pool)
feols(as.formula( "balance~treatment|fe_vil"),cluster = ~farmer_id, pool)

### how different is this from first taking averages at farmer level?
pool$treatment_num <- as.numeric(as.factor(pool$treatment))
pool$fe_vil_num <- as.numeric(as.factor(pool$fe_vil))
hh_level <- aggregate(pool[c("price_maize","bought_maize_price","revenue","expenses","balance","treatment_num","fe_vil_num")], list(pool$farmer_id), FUN=mean, na.rm=T)
summary(lm(price_maize~as.factor(treatment_num),data=hh_level)) ## no village fixed effects
feols(as.formula( "price_maize~as.factor(treatment_num)|fe_vil_num"), hh_level) ## with village fixed effects
feols(as.formula( "bought_maize_price~as.factor(treatment_num)|fe_vil_num"), hh_level) ## with village fixed effects

## now revenue and expenses (at transaction level)
feols(as.formula( "revenue~as.factor(treatment_num)|fe_vil_num"), hh_level) ## with village fixed effects
feols(as.formula( "expenses~as.factor(treatment_num)|fe_vil_num"), hh_level) ## with village fixed effects
feols(as.formula( "balance~as.factor(treatment_num)|fe_vil_num"), hh_level) ## with village fixed effects



#iterate over outcomes
outcomes <- c("stock_maize_abs","sold_maize","sold_maize_kg","price_maize","bought_maize", "bought_maize_amt", "bought_maize_price","price_exp_maize")
#matrix to store results
res_tab <-  array(NA,dim=c(21,3,length(outcomes)))
for (i in 1:length(outcomes)) {

### pooled regression (for marginal effects)
fe_ols <- feols(as.formula( paste(paste(outcomes[i],"T1_R1+T2_R1+T1_R2+T2_R2+T1_R3+T2_R3+survey_2+survey_3",sep="~"),"fe_vil",sep="|")),cluster = ~farmer_id, pool)
res_tab[1:8,1,i] <- coef(fe_ols)
res_tab[1:8,2,i] <- se(fe_ols)
res_tab[1:8,3,i] <- pvalue(fe_ols)
#p-values for test
ftest <- linearHypothesis(fe_ols,test="F", "T1_R1  = T1_R2", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
res_tab[9,3,i] <- ftest[["Pr(>F)"]][2]
ftest <- linearHypothesis(fe_ols,test="F", "T2_R1  = T2_R2", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
res_tab[10,3,i] <- ftest[["Pr(>F)"]][2]

ftest <- linearHypothesis(fe_ols,test="F", "T1_R1  = T1_R3", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
res_tab[11,3,i] <- ftest[["Pr(>F)"]][2]
ftest <- linearHypothesis(fe_ols,test="F", "T2_R1  = T2_R3", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
res_tab[12,3,i] <- ftest[["Pr(>F)"]][2]
ftest <- linearHypothesis(fe_ols,test="F", "T1_R2  = T1_R3", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
res_tab[13,3,i] <- ftest[["Pr(>F)"]][2]
ftest <- linearHypothesis(fe_ols,test="F", "T2_R2  = T2_R3", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
res_tab[14,3,i] <- ftest[["Pr(>F)"]][2]
ftest <- linearHypothesis(fe_ols,test="F", "T1_R1  = T2_R1", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
res_tab[15,3,i] <- ftest[["Pr(>F)"]][2]
ftest <- linearHypothesis(fe_ols,test="F", "T1_R2  = T2_R2", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
res_tab[16,3,i] <- ftest[["Pr(>F)"]][2]
ftest <- linearHypothesis(fe_ols,test="F", "T1_R3  = T2_R3", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
res_tab[17,3,i] <- ftest[["Pr(>F)"]][2]

res_tab[18,1,i] <- nobs(fe_ols)

res_tab[19,1,i] <-  r2(fe_ols, type="ar2")
res_tab[20,1,i] <-  r2(fe_ols, type="wr2")
#regression with only a constant to get control group mean
res_tab[21,1,i] <- coefficients(lm(as.formula(paste(outcomes[i],"1", sep="~")), data=pool[pool$treatment=="C" & pool$survey==1 ,]))

}
save(res_tab, file="res_tab.Rdata")

save(res_tab, file="/home/bjvca/data/temp/malawi_pooled/res_tab.Rdata")


### ###This is for soy

pool <- data.frame(rbind(cbind(dta_sept$farmer_ID, dta_sept$fe_vil,dta_sept$stock_soy_abs,dta_sept$sold_soy,dta_sept$sold_soy_kg,dta_sept$price_soy,dta_sept$bought_soy,dta_sept$bought_soy_amt,dta_sept$bought_soy_price,dta_sept$price_dec_soy,dta_sept$survey,dta_sept$treatment),
                         cbind(dta_dec$farmer_ID, dta_dec$fe_vil,dta_dec$stock_soy_abs,dta_dec$sold_soy,dta_dec$sold_soy_kg,dta_dec$price_soy,dta_dec$bought_soy,dta_dec$bought_soy_amt,dta_dec$bought_soy_price,dta_dec$price_dec_soy,dta_dec$survey,dta_dec$treatment),
                         cbind(dta_apr$farmer_ID, dta_apr$fe_vil,dta_apr$stock_soy_abs,dta_apr$sold_soy,dta_apr$sold_soy_kg,dta_apr$price_soy,dta_apr$bought_soy,dta_apr$bought_soy_amt,dta_apr$bought_soy_price,dta_apr$price_dec_soy,dta_apr$survey,dta_apr$treatment)))

names(pool) <- c("farmer_id", "fe_vil","stock_soy_abs","sold_soy","sold_soy_kg","price_soy","bought_soy","bought_soy_amt","bought_soy_price","price_exp_soy","survey","treatment")
##restrict to farmers that grow soy
pool <- merge(pool, dta_sept[c("farmer_ID","q48")], by.x="farmer_id",by.y="farmer_ID", all.x=T)
pool <- subset(pool, q48!="88")

pool$survey_1 <- 0
pool$survey_2 <- 0
pool$survey_3 <- 0

pool$survey_1[pool$survey==1] <- 1 
pool$survey_2[pool$survey==2] <- 1
pool$survey_3[pool$survey==3] <- 1

pool$T1_R1 <- 0
pool$T1_R2 <- 0
pool$T1_R3 <- 0
pool$T2_R1 <- 0
pool$T2_R2 <- 0
pool$T2_R3 <- 0

pool$T1_R1[pool$survey==1 & pool$treatment=="T1"] <- 1
pool$T2_R1[pool$survey==1 & pool$treatment=="T2"] <- 1
pool$T1_R2[pool$survey==2 & pool$treatment=="T1"] <- 1
pool$T2_R2[pool$survey==2 & pool$treatment=="T2"] <- 1
pool$T1_R3[pool$survey==3 & pool$treatment=="T1"] <- 1
pool$T2_R3[pool$survey==3 & pool$treatment=="T2"] <- 1

pool$stock_soy_abs <- as.numeric(as.character(pool$stock_soy_abs))
pool$sold_soy_kg <- as.numeric(as.character(pool$sold_soy_kg))
pool$price_soy <- as.numeric(as.character(pool$price_soy))
pool$price_soy[pool$price_soy<200] <- NA
pool$bought_soy_amt <- as.numeric(as.character(pool$bought_soy_amt))
pool$bought_soy_price <- as.numeric(as.character(pool$bought_soy_price))
pool$bought_soy_price[pool$bought_soy_price<200] <- NA
pool$price_exp_soy <- as.numeric(as.character(pool$price_exp_soy))

pool$bought_soy <- as.numeric(pool$bought_soy==TRUE)
pool$sold_soy <- as.numeric(pool$sold_soy==TRUE)

pool$revenue <- pool$sold_soy_kg*pool$price_soy 
pool$revenue[is.na(pool$revenue)] <- 0
pool$expenses <- pool$bought_soy_amt*pool$bought_soy_price
pool$expenses[is.na(pool$expenses)] <- 0
pool$balance <- pool$revenue - pool$expenses 

##prices (at transaction level)
feols(as.formula( "price_soy~treatment|fe_vil"),cluster = ~farmer_id, pool)
feols(as.formula( "bought_soy_price~treatment|fe_vil"),cluster = ~farmer_id, pool)

feols(as.formula( "revenue~treatment|fe_vil"),cluster = ~farmer_id, pool)
feols(as.formula( "expenses~treatment|fe_vil"),cluster = ~farmer_id, pool)
feols(as.formula( "balance~treatment|fe_vil"),cluster = ~farmer_id, pool)

### how different is this from first taking averages at farmer level?
pool$treatment_num <- as.numeric(as.factor(pool$treatment))
pool$fe_vil_num <- as.numeric(as.factor(pool$fe_vil))
hh_level <- aggregate(pool[c("price_soy","bought_soy_price","revenue","expenses","balance","treatment_num","fe_vil_num")], list(pool$farmer_id), FUN=mean, na.rm=T)
summary(lm(price_soy~as.factor(treatment_num),data=hh_level)) ## no village fixed effects
feols(as.formula( "price_soy~as.factor(treatment_num)|fe_vil_num"), hh_level) ## with village fixed effects
feols(as.formula( "bought_soy_price~as.factor(treatment_num)|fe_vil_num"), hh_level) ## with village fixed effects

## now revenue and expenses (at transaction level)
feols(as.formula( "revenue~as.factor(treatment_num)|fe_vil_num"), hh_level) ## with village fixed effects
feols(as.formula( "expenses~as.factor(treatment_num)|fe_vil_num"), hh_level) ## with village fixed effects
feols(as.formula( "balance~as.factor(treatment_num)|fe_vil_num"), hh_level) ## with village fixed effects


#iterate over outcomes
outcomes <- c("stock_soy_abs","sold_soy","sold_soy_kg","price_soy","bought_soy", "bought_soy_amt", "bought_soy_price","price_exp_soy")
#matrix to store results
res_tab_soy <-  array(NA,dim=c(21,3,length(outcomes)))
for (i in 1:length(outcomes)) {
  
  ### pooled regression (for marginal effects)
  fe_ols <- feols(as.formula( paste(paste(outcomes[i],"T1_R1+T2_R1+T1_R2+T2_R2+T1_R3+T2_R3+survey_2+survey_3",sep="~"),"fe_vil",sep="|")),cluster = ~farmer_id, pool)
  res_tab_soy[1:8,1,i] <- coef(fe_ols)
  res_tab_soy[1:8,2,i] <- se(fe_ols)
  res_tab_soy[1:8,3,i] <- pvalue(fe_ols)
  #p-values for test
  ftest <- linearHypothesis(fe_ols,test="F", "T1_R1  = T1_R2", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
  res_tab_soy[9,3,i] <- ftest[["Pr(>F)"]][2]
  ftest <- linearHypothesis(fe_ols,test="F", "T2_R1  = T2_R2", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
  res_tab_soy[10,3,i] <- ftest[["Pr(>F)"]][2]
  
  ftest <- linearHypothesis(fe_ols,test="F", "T1_R1  = T1_R3", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
  res_tab_soy[11,3,i] <- ftest[["Pr(>F)"]][2]
  ftest <- linearHypothesis(fe_ols,test="F", "T2_R1  = T2_R3", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
  res_tab_soy[12,3,i] <- ftest[["Pr(>F)"]][2]
  ftest <- linearHypothesis(fe_ols,test="F", "T1_R2  = T1_R3", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
  res_tab_soy[13,3,i] <- ftest[["Pr(>F)"]][2]
  ftest <- linearHypothesis(fe_ols,test="F", "T2_R2  = T2_R3", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
  res_tab_soy[14,3,i] <- ftest[["Pr(>F)"]][2]
  ftest <- linearHypothesis(fe_ols,test="F", "T1_R1  = T2_R1", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
  res_tab_soy[15,3,i] <- ftest[["Pr(>F)"]][2]
  ftest <- linearHypothesis(fe_ols,test="F", "T1_R2  = T2_R2", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
  res_tab_soy[16,3,i] <- ftest[["Pr(>F)"]][2]
  ftest <- linearHypothesis(fe_ols,test="F", "T1_R3  = T2_R3", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
  res_tab_soy[17,3,i] <- ftest[["Pr(>F)"]][2]
  
  res_tab_soy[18,1,i] <- nobs(fe_ols)
  
  res_tab_soy[19,1,i] <-  r2(fe_ols, type="ar2")
  res_tab_soy[20,1,i] <-  r2(fe_ols, type="wr2")
  #regression with only a constant to get control group mean
  res_tab_soy[21,1,i] <- coefficients(lm(as.formula(paste(outcomes[i],"1", sep="~")), data=pool[pool$treatment=="C" & pool$survey==1 ,]))
}
save(res_tab_soy, file="res_tab_soy.Rdata")

save(res_tab_soy, file="/home/bjvca/data/temp/malawi_pooled/res_tab_soy.Rdata")

## ###This is for gnuts

pool <- data.frame(rbind(cbind(dta_sept$farmer_ID, dta_sept$fe_vil,dta_sept$stock_gnuts_abs,dta_sept$sold_gnuts,dta_sept$sold_gnuts_kg,dta_sept$price_gnuts,dta_sept$bought_gnuts,dta_sept$bought_gnuts_amt,dta_sept$bought_gnuts_price,dta_sept$price_dec_gnuts,dta_sept$survey,dta_sept$treatment),
                         cbind(dta_dec$farmer_ID, dta_dec$fe_vil,dta_dec$stock_gnuts_abs,dta_dec$sold_gnuts,dta_dec$sold_gnuts_kg,dta_dec$price_gnuts,dta_dec$bought_gnuts,dta_dec$bought_gnuts_amt,dta_dec$bought_gnuts_price,dta_dec$price_dec_gnuts,dta_dec$survey,dta_dec$treatment),
                         cbind(dta_apr$farmer_ID, dta_apr$fe_vil,dta_apr$stock_gnuts_abs,dta_apr$sold_gnuts,dta_apr$sold_gnuts_kg,dta_apr$price_gnuts,dta_apr$bought_gnuts,dta_apr$bought_gnuts_amt,dta_apr$bought_gnuts_price,dta_apr$price_dec_gnuts,dta_apr$survey,dta_apr$treatment)))

names(pool) <- c("farmer_id", "fe_vil","stock_gnuts_abs","sold_gnuts","sold_gnuts_kg","price_gnuts","bought_gnuts","bought_gnuts_amt","bought_gnuts_price","price_exp_gnuts","survey","treatment")
##restrict to farmers that grow gnuts
pool <- merge(pool, dta_sept[c("farmer_ID","q48")], by.x="farmer_id",by.y="farmer_ID", all.x=T)
pool <- subset(pool, q48!="88")

pool$survey_1 <- 0
pool$survey_2 <- 0
pool$survey_3 <- 0

pool$survey_1[pool$survey==1] <- 1 
pool$survey_2[pool$survey==2] <- 1
pool$survey_3[pool$survey==3] <- 1

pool$T1_R1 <- 0
pool$T1_R2 <- 0
pool$T1_R3 <- 0
pool$T2_R1 <- 0
pool$T2_R2 <- 0
pool$T2_R3 <- 0

pool$T1_R1[pool$survey==1 & pool$treatment=="T1"] <- 1
pool$T2_R1[pool$survey==1 & pool$treatment=="T2"] <- 1
pool$T1_R2[pool$survey==2 & pool$treatment=="T1"] <- 1
pool$T2_R2[pool$survey==2 & pool$treatment=="T2"] <- 1
pool$T1_R3[pool$survey==3 & pool$treatment=="T1"] <- 1
pool$T2_R3[pool$survey==3 & pool$treatment=="T2"] <- 1

pool$stock_gnuts_abs <- as.numeric(as.character(pool$stock_gnuts_abs))
pool$sold_gnuts_kg <- as.numeric(as.character(pool$sold_gnuts_kg))
pool$price_gnuts <- as.numeric(as.character(pool$price_gnuts))
pool$bought_gnuts_amt <- as.numeric(as.character(pool$bought_gnuts_amt))
pool$bought_gnuts_price <- as.numeric(as.character(pool$bought_gnuts_price))
pool$price_exp_gnuts <- as.numeric(as.character(pool$price_exp_gnuts))

pool$bought_gnuts <- as.numeric(pool$bought_gnuts==TRUE)
pool$sold_gnuts <- as.numeric(pool$sold_gnuts==TRUE)


pool$revenue <- pool$sold_gnuts_kg*pool$price_gnuts 
pool$revenue[is.na(pool$revenue)] <- 0
pool$expenses <- pool$bought_gnuts_amt*pool$bought_gnuts_price
pool$expenses[is.na(pool$expenses)] <- 0
pool$balance <- pool$revenue - pool$expenses 

##prices (at transaction level)
feols(as.formula( "price_gnuts~treatment|fe_vil"),cluster = ~farmer_id, pool)
feols(as.formula( "bought_gnuts_price~treatment|fe_vil"),cluster = ~farmer_id, pool)

feols(as.formula( "revenue~treatment|fe_vil"),cluster = ~farmer_id, pool)
feols(as.formula( "expenses~treatment|fe_vil"),cluster = ~farmer_id, pool)
feols(as.formula( "balance~treatment|fe_vil"),cluster = ~farmer_id, pool)

### how different is this from first taking averages at farmer level?
pool$treatment_num <- as.numeric(as.factor(pool$treatment))
pool$fe_vil_num <- as.numeric(as.factor(pool$fe_vil))
hh_level <- aggregate(pool[c("price_gnuts","bought_gnuts_price","revenue","expenses","balance","treatment_num","fe_vil_num")], list(pool$farmer_id), FUN=mean, na.rm=T)
feols(as.formula( "price_gnuts~as.factor(treatment_num)|fe_vil_num"), hh_level) ## with village fixed effects
feols(as.formula( "bought_gnuts_price~as.factor(treatment_num)|fe_vil_num"), hh_level) ## with village fixed effects

## now revenue and expenses (at transaction level)
feols(as.formula( "revenue~as.factor(treatment_num)|fe_vil_num"), hh_level) ## with village fixed effects
feols(as.formula( "expenses~as.factor(treatment_num)|fe_vil_num"), hh_level) ## with village fixed effects
feols(as.formula( "balance~as.factor(treatment_num)|fe_vil_num"), hh_level) ## with village fixed effects



#iterate over outcomes
outcomes <- c("stock_gnuts_abs","sold_gnuts","sold_gnuts_kg","price_gnuts","bought_gnuts", "bought_gnuts_amt", "bought_gnuts_price","price_exp_gnuts")
#matrix to store results
res_tab_gnuts <-  array(NA,dim=c(21,3,length(outcomes)))
for (i in 1:length(outcomes)) {
  
  ### pooled regression (for marginal effects)
  fe_ols <- feols(as.formula( paste(paste(outcomes[i],"T1_R1+T2_R1+T1_R2+T2_R2+T1_R3+T2_R3+survey_2+survey_3",sep="~"),"fe_vil",sep="|")),cluster = ~farmer_id, pool)
  res_tab_gnuts[1:8,1,i] <- coef(fe_ols)
  res_tab_gnuts[1:8,2,i] <- se(fe_ols)
  res_tab_gnuts[1:8,3,i] <- pvalue(fe_ols)
  #p-values for test
  ftest <- linearHypothesis(fe_ols,test="F", "T1_R1  = T1_R2", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
  res_tab_gnuts[9,3,i] <- ftest[["Pr(>F)"]][2]
  ftest <- linearHypothesis(fe_ols,test="F", "T2_R1  = T2_R2", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
  res_tab_gnuts[10,3,i] <- ftest[["Pr(>F)"]][2]
  
  ftest <- linearHypothesis(fe_ols,test="F", "T1_R1  = T1_R3", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
  res_tab_gnuts[11,3,i] <- ftest[["Pr(>F)"]][2]
  ftest <- linearHypothesis(fe_ols,test="F", "T2_R1  = T2_R3", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
  res_tab_gnuts[12,3,i] <- ftest[["Pr(>F)"]][2]
  ftest <- linearHypothesis(fe_ols,test="F", "T1_R2  = T1_R3", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
  res_tab_gnuts[13,3,i] <- ftest[["Pr(>F)"]][2]
  ftest <- linearHypothesis(fe_ols,test="F", "T2_R2  = T2_R3", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
  res_tab_gnuts[14,3,i] <- ftest[["Pr(>F)"]][2]
  ftest <- linearHypothesis(fe_ols,test="F", "T1_R1  = T2_R1", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
  res_tab_gnuts[15,3,i] <- ftest[["Pr(>F)"]][2]
  ftest <- linearHypothesis(fe_ols,test="F", "T1_R2  = T2_R2", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
  res_tab_gnuts[16,3,i] <- ftest[["Pr(>F)"]][2]
  ftest <- linearHypothesis(fe_ols,test="F", "T1_R3  = T2_R3", error.df= degrees_freedom(fe_ols, "resid", vcov = "iid"))
  res_tab_gnuts[17,3,i] <- ftest[["Pr(>F)"]][2]
  
  res_tab_gnuts[18,1,i] <- nobs(fe_ols)
  
  res_tab_gnuts[19,1,i] <-  r2(fe_ols, type="ar2")
  res_tab_gnuts[20,1,i] <-  r2(fe_ols, type="wr2")
  #regression with only a constant to get control group mean
  res_tab_gnuts[21,1,i] <- coefficients(lm(as.formula(paste(outcomes[i],"1", sep="~")), data=pool[pool$treatment=="C" & pool$survey==1 ,]))
  
}
save(res_tab_gnuts, file="res_tab_gnuts.Rdata")

save(res_tab_gnuts, file="/home/bjvca/data/temp/malawi_pooled/res_tab_gnuts.Rdata")