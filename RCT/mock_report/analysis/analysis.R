##### Balance tables for the malawi study ####################
#run in malawi/RCT/mock_report/analysis
rm(list=ls())
library(nnet)
library(lmtest)
library(car)
library(clubSandwich)

### function we will use for triming contineous variables
trim <- function(var, dataset, trim_perc=.05) {
  ### function for triming a variable in a dataset - replaces with NA
  dataset[var][dataset[var] < quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)), na.rm=T)[1] | dataset[var] > quantile(dataset[var], c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2] ] <- NA
  return(dataset)
}

ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
return(y)}



path <- getwd()
path <- strsplit(path, "/mock_report/analysis")[[1]]

dta <- read.csv(paste(path,"baseline/data/public/baseline_data.csv", sep="/"))

###create unique dist_ta_vil variable to use for fixed effects
dta$fe_vil <- as.factor(paste(paste(dta$distID, dta$taID, sep="_"),dta$vilID, sep ="_"))

dta$treatment[dta$treatment=="n/a"] <- NA
### 1. calculate baseline (expected) production to simulate stocks and to use as denominator in quantity sold (%)
### current production

##this is for maize

set <- c("group7.q55b", "groupx1.q55c1","group8.q55f" )

dta[set] <- lapply(dta[set],  function(x) as.numeric(as.character(x)))
dta[set] <- lapply(dta[set],  function(x) replace(x,is.na(x),0))

set <- c("group7.q55b1", "groupx1.q55c2","group8.q55f1" )
dta[set] <- lapply(dta[set],  function(x) replace(x,is.na(x),"XXX"))

dta$group7.q55b[dta$group7.q55b1=="kg"] <- dta$group7.q55b[dta$group7.q55b1=="kg"]
dta$group7.q55b[dta$group7.q55b1=="Bags_50kg"] <- dta$group7.q55b[dta$group7.q55b1=="Bags_50kg"]*50
dta$group7.q55b[dta$group7.q55b1=="OX cart"] <- dta$group7.q55b[dta$group7.q55b1=="OX cart"]*500

dta$groupx1.q55c1[dta$groupx1.q55c2=="kg"] <- dta$groupx1.q55c1[dta$groupx1.q55c2=="kg"]
dta$groupx1.q55c1[dta$groupx1.q55c2=="Bags_50kg"] <- dta$groupx1.q55c1[dta$groupx1.q55c2=="Bags_50kg"]*50
dta$groupx1.q55c1[dta$groupx1.q55c2=="OX cart"] <- dta$groupx1.q55c1[dta$groupx1.q55c2=="OX cart"]*500

dta$group8.q55f[dta$group8.q55f1=="kg"] <- dta$group8.q55f[dta$group8.q55f1=="kg"]
dta$group8.q55f[dta$group8.q55f1=="Bags_50kg"] <- dta$group8.q55f[dta$group8.q55f1=="Bags_50kg"]*50
dta$group8.q55f[dta$group8.q55f1=="OX cart"] <- dta$group8.q55f[dta$group8.q55f1=="OX cart"]*500

set <- c("group7.q55b", "groupx1.q55c1","group8.q55f" )
dta$prod_maize <-rowSums(dta[set])
###now for gnuts
set <- c("group10.q59b", "groupx2.q59c1","group11.q59f" )

dta[set] <- lapply(dta[set],  function(x) as.numeric(as.character(x)))
dta[set] <- lapply(dta[set],  function(x) replace(x,is.na(x),0))

set <- c("group10.q59b1", "groupx2.q59c2","group11.q59f1" )
dta[set] <- lapply(dta[set],  function(x) replace(x,is.na(x),"XXX"))

dta$group10.q59b[dta$group10.q59b1=="kg"] <- dta$group10.q59b[dta$group10.q59b1=="kg"]
dta$group10.q59b[dta$group10.q59b1=="Bags_50kg"] <- dta$group10.q59b[dta$group10.q59b1=="Bags_50kg"]*13
dta$group10.q59b[dta$group10.q59b1=="Debbe_Ndowa"] <- dta$group10.q59b[dta$group10.q59b1=="Debbe_Ndowa"]*5

dta$groupx2.q59c1[dta$groupx2.q59c2=="kg"] <- dta$groupx2.q59c1[dta$groupx2.q59c2=="kg"]
dta$groupx2.q59c1[dta$groupx2.q59c2=="Bags_50kg"] <- dta$groupx2.q59c1[dta$groupx2.q59c2=="Bags_50kg"]*13
dta$groupx2.q59c1[dta$groupx2.q59c2=="Debbe_Ndowa"] <- dta$groupx2.q59c1[dta$groupx2.q59c2=="Debbe_Ndowa"]*5

dta$group11.q59f[dta$group11.q59f1=="kg"] <- dta$group11.q59f[dta$group11.q59f1=="kg"]
dta$group11.q59f[dta$group11.q59f1=="Bags_50kg"] <- dta$group11.q59f[dta$group11.q59f1=="Bags_50kg"]*13
dta$group11.q59f[dta$group11.q59f1=="Debbe_Ndowa"] <- dta$group11.q59f[dta$group11.q59f1=="Debbe_Ndowa"]*5

set <- c("group10.q59b", "groupx2.q59c1","group11.q59f" )
dta$prod_gnuts <-rowSums(dta[set])
###now for soy

set <- c("group13.q63b", "groupx3.q63c1","group14.q63f" )

dta[set] <- lapply(dta[set],  function(x) as.numeric(as.character(x)))
dta[set] <- lapply(dta[set],  function(x) replace(x,is.na(x),0))

set <- c("group13.q63b1", "groupx3.q63c2","group14.q63f1" )
dta[set] <- lapply(dta[set],  function(x) replace(x,is.na(x),"XXX"))

dta$group13.q63b[dta$group13.q63b1=="kg"] <- dta$group13.q63b[dta$group13.q63b1=="kg"]
dta$group13.q63b[dta$group13.q63b1=="Bags_50kg"] <- dta$group13.q63b[dta$group13.q63b1=="Bags_50kg"]*50
dta$group13.q63b[dta$group13.q63b1=="Debbe_Ndowa"] <- dta$group13.q63b[dta$group13.q63b1=="Debbe_Ndowa"]*20

dta$groupx3.q63c1[dta$groupx3.q63c2=="kg"] <- dta$groupx3.q63c1[dta$groupx3.q63c2=="kg"]
dta$groupx3.q63c1[dta$groupx3.q63c2=="Bags_50kg"] <- dta$groupx3.q63c1[dta$groupx3.q63c2=="Bags_50kg"]*50
dta$groupx3.q63c1[dta$groupx3.q63c2=="Debbe_Ndowa"] <- dta$groupx3.q63c1[dta$groupx3.q63c2=="Debbe_Ndowa"]*20

dta$group14.q63f[dta$group14.q63f1=="kg"] <- dta$group14.q63f[dta$group14.q63f1=="kg"]
dta$group14.q63f[dta$group14.q63f1=="Bags_50kg"] <- dta$group14.q63f[dta$group14.q63f1=="Bags_50kg"]*50
dta$group14.q63f[dta$group14.q63f1=="Debbe_Ndowa"] <- dta$group14.q63f[dta$group14.q63f1=="Debbe_Ndowa"]*20

set <- c("group13.q63b", "groupx3.q63c1","group14.q63f" )
dta$prod_soy <-rowSums(dta[set])

## 2. extract transactions in previous year to simulate sales from and to use as baseline data
sel <- c( "trans.1..q43b", "trans.1..q43d","trans.1..group2.q43c","trans.1..q43e","trans.1..q43f","trans.1..q43g")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.1..q43a","trans.1..group2.q43c1")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans1 <- data.frame(dta$farmer_ID,dta$trans.1..q43a,dta$trans.1..q43b,dta$trans.1..q43d,dta$trans.1..group2.q43c,dta$trans.1..group2.q43c1,dta$trans.1..q43e,dta$trans.1..q43f,dta$trans.1..q43g)
names(trans1) <- c("farmer_ID", "date","form","price","quant","measure","buyer","decision","use")

sel <- c( "trans.2..q43b", "trans.2..q43d","trans.2..group2.q43c","trans.2..q43e","trans.2..q43f","trans.2..q43g")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.2..q43a","trans.2..group2.q43c1")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans2 <- data.frame(dta$farmer_ID,dta$trans.2..q43a,dta$trans.2..q43b,dta$trans.2..q43d,dta$trans.2..group2.q43c,dta$trans.2..group2.q43c1,dta$trans.2..q43e,dta$trans.2..q43f,dta$trans.2..q43g)
names(trans2) <- c("farmer_ID", "date","form","price","quant","measure","buyer","decision","use")

sel <- c( "trans.3..q43b", "trans.3..q43d","trans.3..group2.q43c","trans.3..q43e","trans.3..q43f","trans.3..q43g")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.3..q43a","trans.3..group2.q43c1")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans3 <- data.frame(dta$farmer_ID,dta$trans.3..q43a,dta$trans.3..q43b,dta$trans.3..q43d,dta$trans.3..group2.q43c,dta$trans.3..group2.q43c1,dta$trans.3..q43e,dta$trans.3..q43f,dta$trans.3..q43g)
names(trans3) <- c("farmer_ID", "date","form","price","quant","measure","buyer","decision","use")

sel <- c( "trans.4..q43b", "trans.4..q43d","trans.4..group2.q43c","trans.4..q43e","trans.4..q43f","trans.4..q43g")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.4..q43a","trans.4..group2.q43c1")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans4 <- data.frame(dta$farmer_ID,dta$trans.4..q43a,dta$trans.4..q43b,dta$trans.4..q43d,dta$trans.4..group2.q43c,dta$trans.4..group2.q43c1,dta$trans.4..q43e,dta$trans.4..q43f,dta$trans.4..q43g)
names(trans4) <- c("farmer_ID", "date","form","price","quant","measure","buyer","decision","use")

sel <- c( "trans.5..q43b", "trans.5..q43d","trans.5..group2.q43c","trans.5..q43e","trans.5..q43f","trans.5..q43g")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.5..q43a","trans.5..group2.q43c1")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans5 <- data.frame(dta$farmer_ID,dta$trans.5..q43a,dta$trans.5..q43b,dta$trans.5..q43d,dta$trans.5..group2.q43c,dta$trans.5..group2.q43c1,dta$trans.5..q43e,dta$trans.5..q43f,dta$trans.5..q43g)
names(trans5) <- c("farmer_ID", "date","form","price","quant","measure","buyer","decision","use")

sel <- c( "trans.6..q43b", "trans.6..q43d","trans.6..group2.q43c","trans.6..q43e","trans.6..q43f","trans.6..q43g")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.6..q43a","trans.6..group2.q43c1")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans6 <- data.frame(dta$farmer_ID,dta$trans.6..q43a,dta$trans.6..q43b,dta$trans.6..q43d,dta$trans.6..group2.q43c,dta$trans.6..group2.q43c1,dta$trans.6..q43e,dta$trans.6..q43f,dta$trans.6..q43g)
names(trans6) <- c("farmer_ID", "date","form","price","quant","measure","buyer","decision","use")

sel <- c( "trans.7..q43b", "trans.7..q43d","trans.7..group2.q43c","trans.7..q43e","trans.7..q43f","trans.7..q43g")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.7..q43a","trans.7..group2.q43c1")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans7 <- data.frame(dta$farmer_ID,dta$trans.7..q43a,dta$trans.7..q43b,dta$trans.7..q43d,dta$trans.7..group2.q43c,dta$trans.7..group2.q43c1,dta$trans.7..q43e,dta$trans.7..q43f,dta$trans.7..q43g)
names(trans7) <- c("farmer_ID", "date","form","price","quant","measure","buyer","decision","use")

sel <- c( "trans.8..q43b", "trans.8..q43d","trans.8..group2.q43c","trans.8..q43e","trans.8..q43f","trans.8..q43g")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.8..q43a","trans.8..group2.q43c1")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans8 <- data.frame(dta$farmer_ID,dta$trans.8..q43a,dta$trans.8..q43b,dta$trans.8..q43d,dta$trans.8..group2.q43c,dta$trans.8..group2.q43c1,dta$trans.8..q43e,dta$trans.8..q43f,dta$trans.8..q43g)
names(trans8) <- c("farmer_ID", "date","form","price","quant","measure","buyer","decision","use")

all <- rbind(trans1, trans2, trans3, trans4, trans5, trans6, trans7,trans8)
names(all) <-  c("farmer_ID", "date","form","price","quant","measure","buyer","decision","use")
all <- subset(all, form ==2)
all$price[all$price > 999] <- NA
all$price[all$price < 25] <- NA


maize_trans <- all

#now for gnuts

sel <- c( "trans1.1..q47b", "trans1.1..q47d","trans1.1..group4.q47c","trans1.1..q47e","trans1.1..q47f","trans1.1..q47g")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans1.1..q47a","trans1.1..group4.q47c1")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans11 <- data.frame(dta$farmer_ID,dta$trans1.1..q47a,dta$trans1.1..q47b,dta$trans1.1..q47d,dta$trans1.1..group4.q47c,dta$trans1.1..group4.q47c1,dta$trans1.1..q47e,dta$trans1.1..q47f,dta$trans1.1..q47g)
names(trans11) <- c("farmer_ID", "date","form","price","quant","measure","buyer","decision","use")

sel <- c( "trans1.2..q47b", "trans1.2..q47d","trans1.2..group4.q47c","trans1.2..q47e","trans1.2..q47f","trans1.2..q47g")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans1.2..q47a","trans1.2..group4.q47c1")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans12 <- data.frame(dta$farmer_ID,dta$trans1.2..q47a,dta$trans1.2..q47b,dta$trans1.2..q47d,dta$trans1.2..group4.q47c,dta$trans1.2..group4.q47c1,dta$trans1.2..q47e,dta$trans1.2..q47f,dta$trans1.2..q47g)
names(trans12) <- c("farmer_ID", "date","form","price","quant","measure","buyer","decision","use")

sel <- c( "trans1.3..q47b", "trans1.3..q47d","trans1.3..group4.q47c","trans1.3..q47e","trans1.3..q47f","trans1.3..q47g")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans1.3..q47a","trans1.3..group4.q47c1")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans13 <- data.frame(dta$farmer_ID,dta$trans1.3..q47a,dta$trans1.3..q47b,dta$trans1.3..q47d,dta$trans1.3..group4.q47c,dta$trans1.3..group4.q47c1,dta$trans1.3..q47e,dta$trans1.3..q47f,dta$trans1.3..q47g)
names(trans13) <- c("farmer_ID", "date","form","price","quant","measure","buyer","decision","use")

sel <- c( "trans1.4..q47b", "trans1.4..q47d","trans1.4..group4.q47c","trans1.4..q47e","trans1.4..q47f","trans1.4..q47g")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans1.4..q47a","trans1.4..group4.q47c1")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans14 <- data.frame(dta$farmer_ID,dta$trans1.4..q47a,dta$trans1.4..q47b,dta$trans1.4..q47d,dta$trans1.4..group4.q47c,dta$trans1.4..group4.q47c1,dta$trans1.4..q47e,dta$trans1.4..q47f,dta$trans1.4..q47g)
names(trans14) <- c("farmer_ID", "date","form","price","quant","measure","buyer","decision","use")

sel <- c( "trans1.5..q47b", "trans1.5..q47d","trans1.5..group4.q47c","trans1.5..q47e","trans1.5..q47f","trans1.5..q47g")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans1.5..q47a","trans1.5..group4.q47c1")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans15 <- data.frame(dta$farmer_ID,dta$trans1.5..q47a,dta$trans1.5..q47b,dta$trans1.5..q47d,dta$trans1.5..group4.q47c,dta$trans1.5..group4.q47c1,dta$trans1.5..q47e,dta$trans1.5..q47f,dta$trans1.5..q47g)
names(trans15) <- c("farmer_ID", "date","form","price","quant","measure","buyer","decision","use")

all <- rbind(trans11, trans12, trans13, trans14, trans15)
names(all) <-  c("farmer_ID", "date","form","price","quant","measure","buyer","decision","use")
all <- subset(all, form ==2)
all$price[all$price > 19999] <- NA
all$price[all$price < 999] <- NA

gnuts_trans <- all

### for soybean

sel <- c( "trans2.1..q52b", "trans2.1..q52d","trans2.1..group6.q52c","trans2.1..q52e","trans2.1..q52f","trans2.1..q52g")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans2.1..q52a","trans2.1..group6.q52c1")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans21 <- data.frame(dta$farmer_ID,dta$trans2.1..q52a,dta$trans2.1..q52b,dta$trans2.1..q52d,dta$trans2.1..group6.q52c,dta$trans2.1..group6.q52c1,dta$trans2.1..q52e,dta$trans2.1..q52f,dta$trans2.1..q52g)
names(trans21) <- c("farmer_ID", "date","form","price","quant","measure","buyer","decision","use")

sel <- c( "trans2.2..q52b", "trans2.2..q52d","trans2.2..group6.q52c","trans2.2..q52e","trans2.2..q52f","trans2.2..q52g")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans2.2..q52a","trans2.2..group6.q52c1")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans22 <- data.frame(dta$farmer_ID,dta$trans2.2..q52a,dta$trans2.2..q52b,dta$trans2.2..q52d,dta$trans2.2..group6.q52c,dta$trans2.2..group6.q52c1,dta$trans2.2..q52e,dta$trans2.2..q52f,dta$trans2.2..q52g)
names(trans22) <- c("farmer_ID", "date","form","price","quant","measure","buyer","decision","use")

sel <- c( "trans2.3..q52b", "trans2.3..q52d","trans2.3..group6.q52c","trans2.3..q52e","trans2.3..q52f","trans2.3..q52g")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans2.3..q52a","trans2.3..group6.q52c1")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans23 <- data.frame(dta$farmer_ID,dta$trans2.3..q52a,dta$trans2.3..q52b,dta$trans2.3..q52d,dta$trans2.3..group6.q52c,dta$trans2.3..group6.q52c1,dta$trans2.3..q52e,dta$trans2.3..q52f,dta$trans2.3..q52g)
names(trans23) <- c("farmer_ID", "date","form","price","quant","measure","buyer","decision","use")

sel <- c( "trans2.4..q52b", "trans2.4..q52d","trans2.4..group6.q52c","trans2.4..q52e","trans2.4..q52f","trans2.4..q52g")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans2.4..q52a","trans2.4..group6.q52c1")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans24 <- data.frame(dta$farmer_ID,dta$trans2.4..q52a,dta$trans2.4..q52b,dta$trans2.4..q52d,dta$trans2.4..group6.q52c,dta$trans2.4..group6.q52c1,dta$trans2.4..q52e,dta$trans2.4..q52f,dta$trans2.4..q52g)
names(trans24) <- c("farmer_ID", "date","form","price","quant","measure","buyer","decision","use")

all <- rbind(trans21, trans22, trans23, trans24)
names(all) <-  c("farmer_ID", "date","form","price","quant","measure","buyer","decision","use")
all <- subset(all, form ==1)
all$price[all$price > 1999] <- NA
all$price[all$price < 99] <- NA

soy_trans <- all
###
maize_trans$sold_before_oct <- as.numeric(as.Date(maize_trans$date))<18871
gnuts_trans$sold_before_oct <- as.numeric(as.Date(gnuts_trans$date))<18871
soy_trans$sold_before_oct <- as.numeric(as.Date(soy_trans$date))<18871
## aggregate to household level - thought about doing this at the transaction level (and cluster at the household level) but it is not clear how you can then include baseline ctrls

sold_maize <- aggregate(maize_trans$sold_before_oct , list(maize_trans$farmer_ID),FUN=max)
names(sold_maize) <- c("farmer_ID","sold_maize_b")
dta <- merge(dta,sold_maize, by="farmer_ID", all.x=TRUE)

sold_gnuts <- aggregate(gnuts_trans$sold_before_oct , list(gnuts_trans$farmer_ID),FUN=max)
names(sold_gnuts) <- c("farmer_ID","sold_gnuts_b")
dta <- merge(dta,sold_gnuts, by="farmer_ID", all.x=TRUE)

sold_soy <- aggregate(soy_trans$sold_before_oct , list(soy_trans$farmer_ID),FUN=max)
names(sold_soy) <- c("farmer_ID","sold_soy_b")
dta <- merge(dta,sold_soy, by="farmer_ID", all.x=TRUE)

### calculate quantities sold up to october for baseline and to simulate from - subset everything sold before October
maize_trans <- maize_trans[maize_trans$sold_before_oct,]
maize_trans$crop <- "maize"
gnuts_trans <- gnuts_trans[gnuts_trans$sold_before_oct,]
gnuts_trans$crop <- "gnuts" 
soy_trans <- soy_trans[soy_trans$sold_before_oct,]
soy_trans$crop <- "soy" 
all_transactions <- rbind(maize_trans,gnuts_trans, soy_trans)
# convert amounts sold to kgs! - do this in one go
all_transactions$quant_kg <- NA
all_transactions$measure[is.na(all_transactions$measure)] <- "XX"
all_transactions[(all_transactions$crop %in% c("maize","soy")) & all_transactions$measure=="Bags_50kg",]$quant_kg <- all_transactions[(all_transactions$crop %in% c("maize","soy")) & all_transactions$measure=="Bags_50kg",]$quant*50
all_transactions[(all_transactions$crop=="gnuts") & all_transactions$measure=="Bags_50kg",]$quant_kg <- all_transactions[(all_transactions$crop=="gnuts") & all_transactions$measure=="Bags_50kg",]$quant*13
all_transactions[all_transactions$measure=="kg",]$quant_kg <- all_transactions[ all_transactions$measure=="kg",]$quant

all_transactions[(all_transactions$crop=="gnuts") & all_transactions$measure=="Debbe_Ndowa",]$quant_kg <- all_transactions[(all_transactions$crop=="gnuts") & all_transactions$measure=="Debbe_Ndowa",]$quant*5
all_transactions[(all_transactions$crop=="soy") & all_transactions$measure=="Debbe_Ndowa",]$quant_kg <- all_transactions[(all_transactions$crop=="soy") & all_transactions$measure=="Debbe_Ndowa",]$quant*20

all_transactions[(all_transactions$crop=="soy") & all_transactions$measure=="OX cart",]$quant_kg <- all_transactions[(all_transactions$crop=="soy") & all_transactions$measure=="OX cart",]$quant*500

all_transactions$revenue <- all_transactions$price * all_transactions$quant_kg 

###aggregate at household level

sold_maize_kg <- aggregate(all_transactions$quant_kg[all_transactions$crop=="maize"] , list(all_transactions$farmer_ID[all_transactions$crop=="maize"] ),FUN=sum)
names(sold_maize_kg) <- c("farmer_ID","sold_maize_kg_b")
sold_gnuts_kg <- aggregate(all_transactions$quant_kg[all_transactions$crop=="gnuts"] , list(all_transactions$farmer_ID[all_transactions$crop=="gnuts"] ),FUN=sum)
names(sold_gnuts_kg) <- c("farmer_ID","sold_gnuts_kg_b")
sold_soy_kg <- aggregate(all_transactions$quant_kg[all_transactions$crop=="soy"] , list(all_transactions$farmer_ID[all_transactions$crop=="soy"] ),FUN=sum)
names(sold_soy_kg) <- c("farmer_ID","sold_soy_kg_b")

dta <- merge(dta,sold_maize_kg, by="farmer_ID", all.x=TRUE)
dta <- merge(dta,sold_gnuts_kg, by="farmer_ID", all.x=TRUE)
dta <- merge(dta,sold_soy_kg, by="farmer_ID", all.x=TRUE)

# 3. we also need production in 2021 to calculate sales share at baseline to control for in ANCOVA regressions
dta$q40 <-as.numeric(dta$group1.q40)
dta$maize_harv <- NA
dta$group1.q40a[is.na(dta$group1.q40a)] <- "n/a"
dta$maize_harv[dta$group1.q40a=="Bags_50kg"] <- dta$q40[dta$group1.q40a=="Bags_50kg"]*50
dta$maize_harv[dta$group1.q40a=="OX cart"] <- dta$q40[dta$group1.q40a=="OX cart"]*500
dta$maize_harv[dta$group1.q40a=="kg"] <- dta$q40[dta$group1.q40a=="kg"]
dta$maize_harv[dta$maize_harv> 20000] <- NA 
mean(dta$maize_harv, na.rm=T)

dta$q45 <-as.numeric(dta$group3.q45)
dta$gnuts_harv <- NA
dta$group3.q45a[is.na(dta$group3.q45a)] <- "n/a"
dta$gnuts_harv[dta$group3.q45a=="Bags_50kg"] <- dta$q45[dta$group3.q45a=="Bags_50kg"]*13
dta$gnuts_harv[dta$group3.q45a=="Debbe_Ndowa"] <- dta$q45[dta$group3.q45a=="Debbe_Ndowa"]*5
dta$gnuts_harv[dta$group3.q45a=="kg"] <- dta$q45[dta$group3.q45a=="kg"]
dta$gnuts_harv[dta$gnuts_harv> 4000] <- NA 
mean(dta$gnuts_harv, na.rm=T)

dta$q49 <-as.numeric(dta$group5.q49)
dta$soy_harv <- NA
dta$group5.q49a[is.na(dta$group5.q49a)] <- "n/a"
dta$soy_harv[dta$group5.q49a=="Bags_50kg"] <- dta$q49[dta$group5.q49a=="Bags_50kg"]*50
dta$soy_harv[dta$group5.q49a=="Debbe_Ndowa"] <- dta$q49[dta$group5.q49a=="Debbe_Ndowa"]*20
dta$soy_harv[dta$group5.q49a=="kg"] <- dta$q49[dta$group5.q49a=="kg"]
dta$soy_harv[dta$soy_harv> 4000] <- NA 
mean(dta$soy_harv, na.rm=T)

#4. use trans_maize to get average pricem
price_maize <- aggregate(maize_trans$price, list(maize_trans$farmer_ID), mean, na.rm=TRUE)
names(price_maize) <- c("farmer_ID","price_maize_b")
price_gnuts <- aggregate(gnuts_trans$price, list(gnuts_trans$farmer_ID), mean, na.rm=TRUE)
names(price_gnuts) <- c("farmer_ID","price_gnuts_b")
price_soy <- aggregate(soy_trans$price, list(soy_trans$farmer_ID), mean, na.rm=TRUE)
names(price_soy) <- c("farmer_ID","price_soy_b")

dta <- merge(dta,price_maize,by="farmer_ID", all.x=TRUE)
dta <- merge(dta,price_gnuts,by="farmer_ID", all.x=TRUE)
dta <- merge(dta,price_soy,by="farmer_ID", all.x=TRUE)

##@@@ this sumulates outcomes - this is what needs to be replace with id/endline data
dta$stock_maize_abs <- dta$prod_maize*runif(dim(dta)[1], min = 0, max = 1)## take random share of production
dta$stock_maize_abs <-dta$stock_maize_abs -0.1*(dta$stock_maize_abs*(dta$treatment=="T1")) -0.2*(dta$stock_maize_abs*(dta$treatment=="T2"))## simulate TE

dta$stock_gnuts_abs <- dta$prod_gnuts*runif(dim(dta)[1], min = 0, max = 1)## take random share of production
dta$stock_gnuts_abs <-dta$stock_gnuts_abs -0.1*(dta$stock_gnuts_abs*(dta$treatment=="T1")) -0.2*(dta$stock_gnuts_abs*(dta$treatment=="T2"))## simulate TE

dta$stock_soy_abs <- dta$prod_soy*runif(dim(dta)[1], min = 0, max = 1)## take random share of production
dta$stock_soy_abs <-dta$stock_soy_abs -0.1*(dta$stock_soy_abs*(dta$treatment=="T1")) -0.2*(dta$stock_soy_abs*(dta$treatment=="T2"))## simulate TE

tau_maize <- -(.1*mean(dta$sold_maize_b, na.rm=T))
tau_gnuts <- -(.5*mean(dta$sold_gnuts_b, na.rm=T))
tau_soy <- -(.2*mean(dta$sold_soy_b, na.rm=T))
dta$sold_maize <- rbinom(n=dim(dta)[1], size=1, prob=mean(dta$sold_maize_b, na.rm=T)+tau_maize*(dta$treatment == "T1")+tau_maize/2*(dta$treatment == "T2"))
dta$sold_gnuts <- rbinom(n=dim(dta)[1], size=1, prob=mean(dta$sold_gnuts_b, na.rm=T)+tau_gnuts*(dta$treatment == "T1")+tau_gnuts/2*(dta$treatment == "T2"))
dta$sold_soy <- rbinom(n=dim(dta)[1], size=1, prob=mean(dta$sold_soy_b, na.rm=T)+tau_soy*(dta$treatment == "T1")+tau_soy/2*(dta$treatment == "T2"))

dta$sold_maize_kg <- rnorm(dim(dta)[1],mean(dta$sold_maize_kg_b, na.rm=T), sd(dta$sold_maize_kg_b, na.rm=T)) + 0.3*dta$sold_maize_kg_b  -.1*sd(dta$sold_maize_kg_b, na.rm=T)*(dta$treatment=="T1")-.05*sd(dta$sold_maize_kg_b, na.rm=T)*(dta$treatment=="T2")
dta$sold_maize_kg [dta$sold_maize_kg <0] <- 0
dta$sold_gnuts_kg <- rnorm(dim(dta)[1],mean(dta$sold_gnuts_kg_b, na.rm=T), sd(dta$sold_gnuts_kg_b, na.rm=T)) + 0.3*dta$sold_gnuts_kg_b  -.1*sd(dta$sold_gnuts_kg_b, na.rm=T)*(dta$treatment=="T1")-.05*sd(dta$sold_gnuts_kg_b, na.rm=T)*(dta$treatment=="T2")
dta$sold_gnuts_kg [dta$sold_gnuts_kg <0] <- 0
dta$sold_soy_kg <- rnorm(dim(dta)[1],mean(dta$sold_soy_kg_b, na.rm=T), sd(dta$sold_soy_kg_b, na.rm=T)) + 0.3*dta$sold_soy_kg_b  -.1*sd(dta$sold_soy_kg_b, na.rm=T)*(dta$treatment=="T1")-.05*sd(dta$sold_soy_kg_b, na.rm=T)*(dta$treatment=="T2")
dta$sold_soy_kg [dta$sold_soy_kg <0] <- 0

dta$price_maize <- rnorm(dim(dta)[1],mean(dta$price_maize_b, na.rm=T), sd(dta$price_maize_b, na.rm=T)) + 0.5*dta$price_maize_b + .1*sd(dta$price_maize_b, na.rm=T)*(dta$treatment=="T1")+.05*sd(dta$price_maize_b, na.rm=T)*(dta$treatment=="T2")
dta$price_gnuts <- rnorm(dim(dta)[1],mean(dta$price_gnuts_b, na.rm=T), sd(dta$price_gnuts_b, na.rm=T)) + 0.5*dta$price_gnuts_b + .1*sd(dta$price_gnuts_b, na.rm=T)*(dta$treatment=="T1")+.05*sd(dta$price_gnuts_b, na.rm=T)*(dta$treatment=="T2")
dta$price_soy <- rnorm(dim(dta)[1],mean(dta$price_soy_b, na.rm=T), sd(dta$price_soy_b, na.rm=T)) + 0.5*dta$price_soy_b + .1*sd(dta$price_soy_b, na.rm=T)*(dta$treatment=="T1")+.05*sd(dta$price_soy_b, na.rm=T)*(dta$treatment=="T2")

##results
dta$stock_maize_pct <- dta$stock_maize_abs/dta$prod_maize
dta$stock_gnuts_pct <- dta$stock_gnuts_abs/dta$prod_gnuts
dta$stock_soy_pct <- dta$stock_soy_abs/dta$prod_soy

dta$sold_maize_pct <- dta$sold_maize_kg/dta$prod_maize
dta$sold_gnuts_pct <-  dta$sold_gnuts_kg/dta$prod_gnuts
dta$sold_soy_pct <-  dta$sold_soy_kg/dta$prod_soy

dta$sold_maize_pct[dta$sold_maize_pct > 1] <- NA
dta$sold_gnuts_pct[dta$sold_gnuts_pct > 1] <- NA
dta$sold_soy_pct[dta$sold_soy_pct > 1] <- NA

dta$sold_maize_pct_b <- dta$sold_maize_kg_b/dta$maize_harv
dta$sold_gnuts_pct_b <- dta$sold_gnuts_kg_b/dta$gnuts_harv
dta$sold_soy_pct_b <- dta$sold_soy_kg_b/dta$soy_harv

## trim stocks as this is a continuous variable - also check skewness
dta <- trim("stock_maize_abs", dta)
dta <- trim("stock_gnuts_abs", dta)
dta <- trim("stock_soy_abs", dta)

prim_maize <- matrix(NA,13,10)
prim_maize[1,1] <- mean(dta$stock_maize_abs[dta$treatment=="C"], na.rm=T)
prim_maize[1,2] <- sd(dta$stock_maize_abs[dta$treatment=="C"], na.rm=T)

prim_maize[1,4] <- summary(lm(stock_maize_abs~treatment + fe_vil,data=dta))$coefficients[2,1]
prim_maize[1,5] <- summary(lm(stock_maize_abs~treatment + fe_vil,data=dta))$coefficients[2,2]
prim_maize[1,6] <- summary(lm(stock_maize_abs~treatment + fe_vil,data=dta))$coefficients[2,4]

prim_maize[1,7] <- summary(lm(stock_maize_abs~treatment + fe_vil,data=dta))$coefficients[3,1]
prim_maize[1,8] <- summary(lm(stock_maize_abs~treatment + fe_vil,data=dta))$coefficients[3,2]
prim_maize[1,9] <- summary(lm(stock_maize_abs~treatment + fe_vil,data=dta))$coefficients[3,4]
prim_maize[1,10] <- nobs(lm(stock_maize_abs~treatment + fe_vil,data=dta))

prim_gnuts <- matrix(NA,13,10)
prim_gnuts[1,1] <- mean(dta$stock_gnuts_abs[dta$treatment=="C"], na.rm=T)
prim_gnuts[1,2] <- sd(dta$stock_gnuts_abs[dta$treatment=="C"], na.rm=T)

prim_gnuts[1,4] <- summary(lm(stock_gnuts_abs~treatment + fe_vil,data=dta))$coefficients[2,1]
prim_gnuts[1,5] <- summary(lm(stock_gnuts_abs~treatment + fe_vil,data=dta))$coefficients[2,2]
prim_gnuts[1,6] <- summary(lm(stock_gnuts_abs~treatment + fe_vil,data=dta))$coefficients[2,4]

prim_gnuts[1,7] <- summary(lm(stock_gnuts_abs~treatment + fe_vil,data=dta))$coefficients[3,1]
prim_gnuts[1,8] <- summary(lm(stock_gnuts_abs~treatment + fe_vil,data=dta))$coefficients[3,2]
prim_gnuts[1,9] <- summary(lm(stock_gnuts_abs~treatment + fe_vil,data=dta))$coefficients[3,4]
prim_gnuts[1,10] <- nobs(lm(stock_gnuts_abs~treatment + fe_vil,data=dta))

prim_soy <- matrix(NA,13,10)
prim_soy[1,1] <- mean(dta$stock_soy_abs[dta$treatment=="C"], na.rm=T)
prim_soy[1,2] <- sd(dta$stock_soy_abs[dta$treatment=="C"], na.rm=T)

prim_soy[1,4] <- summary(lm(stock_soy_abs~treatment + fe_vil,data=dta))$coefficients[2,1]
prim_soy[1,5] <- summary(lm(stock_soy_abs~treatment + fe_vil,data=dta))$coefficients[2,2]
prim_soy[1,6] <- summary(lm(stock_soy_abs~treatment + fe_vil,data=dta))$coefficients[2,4]

prim_soy[1,7] <- summary(lm(stock_soy_abs~treatment + fe_vil,data=dta))$coefficients[3,1]
prim_soy[1,8] <- summary(lm(stock_soy_abs~treatment + fe_vil,data=dta))$coefficients[3,2]
prim_soy[1,9] <- summary(lm(stock_soy_abs~treatment + fe_vil,data=dta))$coefficients[3,4]
prim_soy[1,10] <- nobs(lm(stock_soy_abs~treatment + fe_vil,data=dta))

prim_maize[2,1] <- mean(dta$stock_maize_pct[dta$treatment=="C"], na.rm=T)
prim_maize[2,2] <- sd(dta$stock_maize_pct[dta$treatment=="C"], na.rm=T)

prim_maize[2,4] <- summary(lm(stock_maize_pct~treatment + fe_vil,data=dta))$coefficients[2,1]
prim_maize[2,5] <- summary(lm(stock_maize_pct~treatment + fe_vil,data=dta))$coefficients[2,2]
prim_maize[2,6] <- summary(lm(stock_maize_pct~treatment + fe_vil,data=dta))$coefficients[2,4]

prim_maize[2,7] <- summary(lm(stock_maize_pct~treatment + fe_vil,data=dta))$coefficients[3,1]
prim_maize[2,8] <- summary(lm(stock_maize_pct~treatment + fe_vil,data=dta))$coefficients[3,2]
prim_maize[2,9] <- summary(lm(stock_maize_pct~treatment + fe_vil,data=dta))$coefficients[3,4]
prim_maize[2,10] <- nobs(lm(stock_maize_pct~treatment + fe_vil,data=dta))

prim_gnuts[2,1] <- mean(dta$stock_gnuts_pct[dta$treatment=="C"], na.rm=T)
prim_gnuts[2,2] <- sd(dta$stock_gnuts_pct[dta$treatment=="C"], na.rm=T)

prim_gnuts[2,4] <- summary(lm(stock_gnuts_pct~treatment + fe_vil,data=dta))$coefficients[2,1]
prim_gnuts[2,5] <- summary(lm(stock_gnuts_pct~treatment + fe_vil,data=dta))$coefficients[2,2]
prim_gnuts[2,6] <- summary(lm(stock_gnuts_pct~treatment + fe_vil,data=dta))$coefficients[2,4]

prim_gnuts[2,7] <- summary(lm(stock_gnuts_pct~treatment + fe_vil,data=dta))$coefficients[3,1]
prim_gnuts[2,8] <- summary(lm(stock_gnuts_pct~treatment + fe_vil,data=dta))$coefficients[3,2]
prim_gnuts[2,9] <- summary(lm(stock_gnuts_pct~treatment + fe_vil,data=dta))$coefficients[3,4]
prim_gnuts[2,10] <- nobs(lm(stock_gnuts_pct~treatment + fe_vil,data=dta))

prim_soy[2,1] <- mean(dta$stock_soy_pct[dta$treatment=="C"], na.rm=T)
prim_soy[2,2] <- sd(dta$stock_soy_pct[dta$treatment=="C"], na.rm=T)

prim_soy[2,4] <- summary(lm(stock_soy_pct~treatment + fe_vil,data=dta))$coefficients[2,1]
prim_soy[2,5] <- summary(lm(stock_soy_pct~treatment + fe_vil,data=dta))$coefficients[2,2]
prim_soy[2,6] <- summary(lm(stock_soy_pct~treatment + fe_vil,data=dta))$coefficients[2,4]

prim_soy[2,7] <- summary(lm(stock_soy_pct~treatment + fe_vil,data=dta))$coefficients[3,1]
prim_soy[2,8] <- summary(lm(stock_soy_pct~treatment + fe_vil,data=dta))$coefficients[3,2]
prim_soy[2,9] <- summary(lm(stock_soy_pct~treatment + fe_vil,data=dta))$coefficients[3,4]
prim_soy[2,10] <- nobs(lm(stock_soy_pct~treatment + fe_vil,data=dta))

prim_maize[3,1] <- mean(dta$sold_maize[dta$treatment=="C"], na.rm=T)
prim_maize[3,2] <- sd(dta$sold_maize[dta$treatment=="C"], na.rm=T)

prim_maize[3,4] <- summary(lm(sold_maize~treatment + sold_maize_b +fe_vil,data=dta))$coefficients[2,1]
prim_maize[3,5] <- summary(lm(sold_maize~treatment + sold_maize_b + fe_vil,data=dta))$coefficients[2,2]
prim_maize[3,6] <- summary(lm(sold_maize~treatment + sold_maize_b + fe_vil,data=dta))$coefficients[2,4]

prim_maize[3,7] <- summary(lm(sold_maize~treatment + sold_maize_b + fe_vil,data=dta))$coefficients[3,1]
prim_maize[3,8] <- summary(lm(sold_maize~treatment + sold_maize_b + fe_vil,data=dta))$coefficients[3,2]
prim_maize[3,9] <- summary(lm(sold_maize~treatment + sold_maize_b + fe_vil,data=dta))$coefficients[3,4]
prim_maize[3,10] <- nobs(lm(sold_maize~treatment + sold_maize_b + fe_vil,data=dta))

prim_gnuts[3,1] <- mean(dta$sold_gnuts[dta$treatment=="C"], na.rm=T)
prim_gnuts[3,2] <- sd(dta$sold_gnuts[dta$treatment=="C"], na.rm=T)

prim_gnuts[3,4] <- summary(lm(sold_gnuts~treatment + sold_gnuts_b + fe_vil,data=dta))$coefficients[2,1]
prim_gnuts[3,5] <- summary(lm(sold_gnuts~treatment + sold_gnuts_b + fe_vil,data=dta))$coefficients[2,2]
prim_gnuts[3,6] <- summary(lm(sold_gnuts~treatment + sold_gnuts_b + fe_vil,data=dta))$coefficients[2,4]

prim_gnuts[3,7] <- summary(lm(sold_gnuts~treatment + sold_gnuts_b + fe_vil,data=dta))$coefficients[3,1]
prim_gnuts[3,8] <- summary(lm(sold_gnuts~treatment + sold_gnuts_b + fe_vil,data=dta))$coefficients[3,2]
prim_gnuts[3,9] <- summary(lm(sold_gnuts~treatment + sold_gnuts_b + fe_vil,data=dta))$coefficients[3,4]
prim_gnuts[3,10] <- nobs(lm(sold_gnuts~treatment + sold_gnuts_b + fe_vil,data=dta))

prim_soy[3,1] <- mean(dta$sold_soy[dta$treatment=="C"], na.rm=T)
prim_soy[3,2] <- sd(dta$sold_soy[dta$treatment=="C"], na.rm=T)

prim_soy[3,4] <- summary(lm(sold_soy~treatment + sold_soy_b + fe_vil,data=dta))$coefficients[2,1]
prim_soy[3,5] <- summary(lm(sold_soy~treatment + sold_soy_b + fe_vil,data=dta))$coefficients[2,2]
prim_soy[3,6] <- summary(lm(sold_soy~treatment + sold_soy_b + fe_vil,data=dta))$coefficients[2,4]

prim_soy[3,7] <- summary(lm(sold_soy~treatment + sold_soy_b + fe_vil,data=dta))$coefficients[3,1]
prim_soy[3,8] <- summary(lm(sold_soy~treatment + sold_soy_b + fe_vil,data=dta))$coefficients[3,2]
prim_soy[3,9] <- summary(lm(sold_soy~treatment + sold_soy_b + fe_vil,data=dta))$coefficients[3,4]
prim_soy[3,10] <- nobs(lm(sold_soy~treatment + sold_soy_b + fe_vil,data=dta))

prim_maize[4,1] <- mean(dta$sold_maize_kg[dta$treatment=="C"], na.rm=T)
prim_maize[4,2] <- sd(dta$sold_maize_kg[dta$treatment=="C"], na.rm=T)

prim_maize[4,4] <- summary(lm(sold_maize_kg~treatment + sold_maize_kg_b +fe_vil,data=dta))$coefficients[2,1]
prim_maize[4,5] <- summary(lm(sold_maize_kg~treatment + sold_maize_kg_b + fe_vil,data=dta))$coefficients[2,2]
prim_maize[4,6] <- summary(lm(sold_maize_kg~treatment + sold_maize_kg_b + fe_vil,data=dta))$coefficients[2,4]

prim_maize[4,7] <- summary(lm(sold_maize_kg~treatment + sold_maize_kg_b + fe_vil,data=dta))$coefficients[3,1]
prim_maize[4,8] <- summary(lm(sold_maize_kg~treatment + sold_maize_kg_b + fe_vil,data=dta))$coefficients[3,2]
prim_maize[4,9] <- summary(lm(sold_maize_kg~treatment + sold_maize_kg_b + fe_vil,data=dta))$coefficients[3,4]
prim_maize[4,10] <- nobs(lm(sold_maize_kg~treatment + sold_maize_kg_b + fe_vil,data=dta))

prim_gnuts[4,1] <- mean(dta$sold_gnuts_kg[dta$treatment=="C"], na.rm=T)
prim_gnuts[4,2] <- sd(dta$sold_gnuts_kg[dta$treatment=="C"], na.rm=T)

prim_gnuts[4,4] <- summary(lm(sold_gnuts_kg~treatment + sold_gnuts_kg_b + fe_vil,data=dta))$coefficients[2,1]
prim_gnuts[4,5] <- summary(lm(sold_gnuts_kg~treatment + sold_gnuts_kg_b + fe_vil,data=dta))$coefficients[2,2]
prim_gnuts[4,6] <- summary(lm(sold_gnuts_kg~treatment + sold_gnuts_kg_b + fe_vil,data=dta))$coefficients[2,4]

prim_gnuts[4,7] <- summary(lm(sold_gnuts_kg~treatment + sold_gnuts_kg_b + fe_vil,data=dta))$coefficients[3,1]
prim_gnuts[4,8] <- summary(lm(sold_gnuts_kg~treatment + sold_gnuts_kg_b + fe_vil,data=dta))$coefficients[3,2]
prim_gnuts[4,9] <- summary(lm(sold_gnuts_kg~treatment + sold_gnuts_kg_b + fe_vil,data=dta))$coefficients[3,4]
prim_gnuts[4,10] <- nobs(lm(sold_gnuts_kg~treatment + sold_gnuts_kg_b + fe_vil,data=dta))

prim_soy[4,1] <- mean(dta$sold_soy_kg[dta$treatment=="C"], na.rm=T)
prim_soy[4,2] <- sd(dta$sold_soy_kg[dta$treatment=="C"], na.rm=T)

prim_soy[4,4] <- summary(lm(sold_soy_kg~treatment + sold_soy_kg_b + fe_vil,data=dta))$coefficients[2,1]
prim_soy[4,5] <- summary(lm(sold_soy_kg~treatment + sold_soy_kg_b + fe_vil,data=dta))$coefficients[2,2]
prim_soy[4,6] <- summary(lm(sold_soy_kg~treatment + sold_soy_kg_b + fe_vil,data=dta))$coefficients[2,4]

prim_soy[4,7] <- summary(lm(sold_soy_kg~treatment + sold_soy_kg_b + fe_vil,data=dta))$coefficients[3,1]
prim_soy[4,8] <- summary(lm(sold_soy_kg~treatment + sold_soy_kg_b + fe_vil,data=dta))$coefficients[3,2]
prim_soy[4,9] <- summary(lm(sold_soy_kg~treatment + sold_soy_kg_b + fe_vil,data=dta))$coefficients[3,4]
prim_soy[4,10] <- nobs(lm(sold_soy_kg~treatment + sold_soy_kg_b + fe_vil,data=dta))

prim_maize[5,1] <- mean(dta$sold_maize_pct[dta$treatment=="C"], na.rm=T)
prim_maize[5,2] <- sd(dta$sold_maize_pct[dta$treatment=="C"], na.rm=T)

prim_maize[5,4] <- summary(lm(sold_maize_pct~treatment + sold_maize_pct_b +fe_vil,data=dta))$coefficients[2,1]
prim_maize[5,5] <- summary(lm(sold_maize_pct~treatment + sold_maize_pct_b + fe_vil,data=dta))$coefficients[2,2]
prim_maize[5,6] <- summary(lm(sold_maize_pct~treatment + sold_maize_pct_b + fe_vil,data=dta))$coefficients[2,4]

prim_maize[5,7] <- summary(lm(sold_maize_pct~treatment + sold_maize_pct_b + fe_vil,data=dta))$coefficients[3,1]
prim_maize[5,8] <- summary(lm(sold_maize_pct~treatment + sold_maize_pct_b + fe_vil,data=dta))$coefficients[3,2]
prim_maize[5,9] <- summary(lm(sold_maize_pct~treatment + sold_maize_pct_b + fe_vil,data=dta))$coefficients[3,4]
prim_maize[5,10] <- nobs(lm(sold_maize_pct~treatment + sold_maize_pct_b + fe_vil,data=dta))

prim_gnuts[5,1] <- mean(dta$sold_gnuts_pct[dta$treatment=="C"], na.rm=T)
prim_gnuts[5,2] <- sd(dta$sold_gnuts_pct[dta$treatment=="C"], na.rm=T)

prim_gnuts[5,4] <- summary(lm(sold_gnuts_pct~treatment + sold_gnuts_pct_b + fe_vil,data=dta))$coefficients[2,1]
prim_gnuts[5,5] <- summary(lm(sold_gnuts_pct~treatment + sold_gnuts_pct_b + fe_vil,data=dta))$coefficients[2,2]
prim_gnuts[5,6] <- summary(lm(sold_gnuts_pct~treatment + sold_gnuts_pct_b + fe_vil,data=dta))$coefficients[2,4]

prim_gnuts[5,7] <- summary(lm(sold_gnuts_pct~treatment + sold_gnuts_pct_b + fe_vil,data=dta))$coefficients[3,1]
prim_gnuts[5,8] <- summary(lm(sold_gnuts_pct~treatment + sold_gnuts_pct_b + fe_vil,data=dta))$coefficients[3,2]
prim_gnuts[5,9] <- summary(lm(sold_gnuts_pct~treatment + sold_gnuts_pct_b + fe_vil,data=dta))$coefficients[3,4]
prim_gnuts[5,10] <- nobs(lm(sold_gnuts_pct~treatment + sold_gnuts_pct_b + fe_vil,data=dta))

prim_soy[5,1] <- mean(dta$sold_soy_pct[dta$treatment=="C"], na.rm=T)
prim_soy[5,2] <- sd(dta$sold_soy_pct[dta$treatment=="C"], na.rm=T)

prim_soy[5,4] <- summary(lm(sold_soy_pct~treatment + sold_soy_pct_b + fe_vil,data=dta))$coefficients[2,1]
prim_soy[5,5] <- summary(lm(sold_soy_pct~treatment + sold_soy_pct_b + fe_vil,data=dta))$coefficients[2,2]
prim_soy[5,6] <- summary(lm(sold_soy_pct~treatment + sold_soy_pct_b + fe_vil,data=dta))$coefficients[2,4]

prim_soy[5,7] <- summary(lm(sold_soy_pct~treatment + sold_soy_pct_b + fe_vil,data=dta))$coefficients[3,1]
prim_soy[5,8] <- summary(lm(sold_soy_pct~treatment + sold_soy_pct_b + fe_vil,data=dta))$coefficients[3,2]
prim_soy[5,9] <- summary(lm(sold_soy_pct~treatment + sold_soy_pct_b + fe_vil,data=dta))$coefficients[3,4]
prim_soy[5,10] <- nobs(lm(sold_soy_pct~treatment + sold_soy_pct_b + fe_vil,data=dta))

prim_maize[6,1] <- mean(dta$price_maize[dta$treatment=="C"], na.rm=T)
prim_maize[6,2] <- sd(dta$price_maize[dta$treatment=="C"], na.rm=T)

prim_maize[6,4] <- summary(lm(price_maize~treatment + price_maize_b +fe_vil,data=dta))$coefficients[2,1]
prim_maize[6,5] <- summary(lm(price_maize~treatment + price_maize_b + fe_vil,data=dta))$coefficients[2,2]
prim_maize[6,6] <- summary(lm(price_maize~treatment + price_maize_b + fe_vil,data=dta))$coefficients[2,4]

prim_maize[6,7] <- summary(lm(price_maize~treatment + price_maize_b + fe_vil,data=dta))$coefficients[3,1]
prim_maize[6,8] <- summary(lm(price_maize~treatment + price_maize_b + fe_vil,data=dta))$coefficients[3,2]
prim_maize[6,9] <- summary(lm(price_maize~treatment + price_maize_b + fe_vil,data=dta))$coefficients[3,4]
prim_maize[6,10] <- nobs(lm(price_maize~treatment + price_maize_b + fe_vil,data=dta))

prim_gnuts[6,1] <- mean(dta$price_gnuts[dta$treatment=="C"], na.rm=T)
prim_gnuts[6,2] <- sd(dta$price_gnuts[dta$treatment=="C"], na.rm=T)

prim_gnuts[6,4] <- summary(lm(price_gnuts~treatment + price_gnuts_b + fe_vil,data=dta))$coefficients[2,1]
prim_gnuts[6,5] <- summary(lm(price_gnuts~treatment + price_gnuts_b + fe_vil,data=dta))$coefficients[2,2]
prim_gnuts[6,6] <- summary(lm(price_gnuts~treatment + price_gnuts_b + fe_vil,data=dta))$coefficients[2,4]

prim_gnuts[6,7] <- summary(lm(price_gnuts~treatment + price_gnuts_b + fe_vil,data=dta))$coefficients[3,1]
prim_gnuts[6,8] <- summary(lm(price_gnuts~treatment + price_gnuts_b + fe_vil,data=dta))$coefficients[3,2]
prim_gnuts[6,9] <- summary(lm(price_gnuts~treatment + price_gnuts_b + fe_vil,data=dta))$coefficients[3,4]
prim_gnuts[6,10] <- nobs(lm(price_gnuts~treatment + price_gnuts_b + fe_vil,data=dta))

prim_soy[6,1] <- mean(dta$price_soy[dta$treatment=="C"], na.rm=T)
prim_soy[6,2] <- sd(dta$price_soy[dta$treatment=="C"], na.rm=T)

prim_soy[6,4] <- summary(lm(price_soy~treatment + price_soy_b + fe_vil,data=dta))$coefficients[2,1]
prim_soy[6,5] <- summary(lm(price_soy~treatment + price_soy_b + fe_vil,data=dta))$coefficients[2,2]
prim_soy[6,6] <- summary(lm(price_soy~treatment + price_soy_b + fe_vil,data=dta))$coefficients[2,4]

prim_soy[6,7] <- summary(lm(price_soy~treatment + price_soy_b + fe_vil,data=dta))$coefficients[3,1]
prim_soy[6,8] <- summary(lm(price_soy~treatment + price_soy_b + fe_vil,data=dta))$coefficients[3,2]
prim_soy[6,9] <- summary(lm(price_soy~treatment + price_soy_b + fe_vil,data=dta))$coefficients[3,4]
prim_soy[6,10] <- nobs(lm(price_soy~treatment + price_soy_b + fe_vil,data=dta))

prim_maize <-round(prim_maize,digits=3)
prim_gnuts <-round(prim_gnuts,digits=3)
prim_soy <-round(prim_soy,digits=3)

saveRDS(prim_maize, paste(path,"mock_report/results/prim_maize.RData", sep="/"))
saveRDS(prim_gnuts, paste(path,"mock_report/results/prim_gnuts.RData", sep="/"))
saveRDS(prim_soy, paste(path,"mock_report/results/prim_soy.RData", sep="/"))


#### control for FWER using simulation

library(randomizr)

#we need to create an ID within block for merging
#do this in a loop
dta$id_in_blocks <- NA
for (i in levels(dta$fe_vil)) {
  dta$id_in_blocks[dta$fe_vil==i] <- 1:length(dta$id_in_blocks[dta$fe_vil==i])

}
to_drop <- "treatment"
dta <- dta[ , !(names(dta) %in% to_drop)]

blocks <- rep(1:114, times = 31)
id_in_blocks <- rep(1:31, times = 114)
block_m_each <- matrix(NA,114,3)
block_m_each[,1] <- 13
block_m_each[,2] <- 9 
block_m_each[,3] <- 9 
f_s <- c("stock_maize_abs~treatment + fe_vil","stock_maize_pct~treatment + fe_vil","sold_maize~treatment + sold_maize_b +fe_vil" ,"sold_maize_kg~treatment + sold_maize_kg_b +fe_vil","sold_maize_pct~treatment + sold_maize_pct_b +fe_vil","price_soy~treatment + price_soy_b + fe_vil")

# Helper functions
do_reg <- function(data_set, f_s){
  summary(lm(as.formula(f_s),data=data_set))$coefficients[2,4]
  }

permute_treat <- function(dta_fun, f_s){
  dta_fun$blocks <- as.numeric(dta_fun$fe_vil)
  assignment <-    data.frame(blocks,id_in_blocks,block_ra(blocks = blocks, block_m_each = block_m_each,conditions = c("control", "T1", "T2")))
  names(assignment) <- c("blocks","id_in_blocks", "treatment")
  dta_sim <- merge(dta_fun, assignment, by.x=c("blocks", "id_in_blocks"), by.y=c("blocks", "id_in_blocks"))
    
  ps_sim <- matrix(NA,1,length(f_s))               
  for (i in 1:length(f_s)) {            
  ps_sim[i] <- do_reg(dta_sim, f_s[i])
  }
  return(ps_sim)
}
threshold_finder<- function(threshold){
  mean(apply(many_ps, 2, x <- function(x) sum(x <= threshold) > 0 ))
}

#we can get the p_obs from the prim_ matrices

p_obs <- prim_maize[1:6,6]
# Simulate under the sharp null
many_ps <- replicate(1000, permute_treat(dta,f_s), simplify = TRUE)
# Obtain the Type I error rate for a series of thresholds
thresholds <- seq(0, 0.05, length.out = 1000)
type_I_rate <- sapply(thresholds, threshold_finder)
# Find the largest threshold that yields an alpha type I error rate
target_p_value <- thresholds[max(which(type_I_rate <=0.05))]
# Apply target p_value to observed p_values
sig_simulated <- p_obs <= target_p_value
# Compare to raw p-values
sig <- p_obs <= 0.05

