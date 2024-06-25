##### Balance tables for the malawi study ####################
#run in malawi/RCT/endline_2024/analysis
rm(list=ls())
library(nnet)
library(lmtest)
library(car)
library(clubSandwich)

set.seed(16082022)

### function we will use for triming contineous variables
trim <- function(var, dataset, trim_perc=.01) {
  ### function for triming a variable in a dataset - replaces with NA
  dataset[var][dataset[var] < quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)), na.rm=T)[1] | dataset[var] > quantile(dataset[var], c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2] ] <- NA
  return(dataset)
}

ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
return(y)}



path <- getwd()
path <- strsplit(path, "/endline_2024/analysis")[[1]]

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
total_revenue <- aggregate(all_transactions$revenue , list(all_transactions$farmer_ID),FUN=sum, na.rm=T)
names(total_revenue) <- c("farmer_ID","total_revenue")

dta <- merge(dta,total_revenue, by="farmer_ID", all.x=TRUE)

sold_maize_kg <- aggregate(all_transactions$quant_kg[all_transactions$crop=="maize"] , list(all_transactions$farmer_ID[all_transactions$crop=="maize"] ),FUN=sum, na.rm=T)
names(sold_maize_kg) <- c("farmer_ID","sold_maize_kg_b")
sold_gnuts_kg <- aggregate(all_transactions$quant_kg[all_transactions$crop=="gnuts"] , list(all_transactions$farmer_ID[all_transactions$crop=="gnuts"] ),FUN=sum, na.rm=T)
names(sold_gnuts_kg) <- c("farmer_ID","sold_gnuts_kg_b")
sold_soy_kg <- aggregate(all_transactions$quant_kg[all_transactions$crop=="soy"] , list(all_transactions$farmer_ID[all_transactions$crop=="soy"] ),FUN=sum, na.rm=T)
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

# 4. control for variables that show excessive baseline imbalance:
dta$tot_acre_m <- as.numeric(as.character(dta$q54a))
dta$tot_acre_g <- as.numeric(as.character(dta$q58a))
dta$tot_acre_s <- as.numeric(as.character(dta$q62a))

#assume NA is not grown
dta$tot_acre_m[is.na(dta$tot_acre_m)] <- 0
dta$tot_acre_g[is.na(dta$tot_acre_g)] <- 0
dta$tot_acre_s[is.na(dta$tot_acre_s)] <- 0

dta$tot_acre_m[dta$tot_acre_m>25] <- NA
dta$tot_acre_g[dta$tot_acre_g>25] <- NA
dta$tot_acre_s[dta$tot_acre_s>25] <- NA
dta$tot_acre <-  dta$tot_acre_m + dta$tot_acre_g + dta$tot_acre_s

dta$hired_labour <- as.character(dta$q68)
dta$hired_labour[dta$hired_labour=="n/a"] <- NA
dta$hired_labour <- dta$hired_labour== "Yes"

dta$hhsize <- as.numeric(as.character(dta$q19))
dta$ironroof <- as.numeric(as.character(dta$q20)) == 2

##@@@ this is where midline data gets read in

endline_2024 <- read.csv(paste(path,"endline_2024/data/public/endline_2024.csv", sep="/"))
### quick look at attrition

dta$interviewed <- merge(dta,endline_2024[c("farmer_ID","q4a")],by="farmer_ID", all.x=T  )$q4a
dta$interviewed[is.na(dta$interviewed)] <- "No"
prop.table(table(dta$treatment,dta$interviewed),1)
chisq.test(table(dta$treatment,dta$interviewed))

### merge into data new treatment in baseline (as treatment2 in sept midline is missing for attriters)
treatment_assign <-   read.csv(paste(path,"midline_jan_2024/data/public/treatment_assignment.csv", sep="/"))

dta$treat2_assigned <-  merge(dta,treatment_assign[c("farmer_ID","treat2")],by="farmer_ID", all.x=T  )$treat2

prop.table(table(dta$treat2,dta$interviewed),1)
chisq.test(table(dta$treat2,dta$interviewed))
  
dta$stock_maize_abs <- as.numeric(as.character(merge(dta,endline_2024[c("farmer_ID","q43gy")],by="farmer_ID", all.x=T  )$q43gy))
dta$stock_gnuts_abs <- as.numeric(as.character(merge(dta,endline_2024[c("farmer_ID","q47gy")],by="farmer_ID", all.x=T  )$q47gy))
dta$stock_soy_abs <- as.numeric(as.character(merge(dta,endline_2024[c("farmer_ID","q52gy")],by="farmer_ID", all.x=T  )$q52gy))

##sold any ?
endline_2024$q41[endline_2024$q41 =="n/a"] <- NA
endline_2024$q46[endline_2024$q46 =="n/a"] <- NA
endline_2024$q50[endline_2024$q50 =="n/a"] <- NA

##maize sold? (yes/no)
set <- c("trans.1..q43a", "trans.2..q43a", "trans.3..q43a", "trans.4..q43a")
endline_2024[set] <- lapply(endline_2024[set], function(x)  as.numeric(as.POSIXct(x, format="%Y-%m-%d"))>=1704063600)
endline_2024$sold_maize <- (rowSums(endline_2024[set],na.rm=T) >0) 
endline_2024$sold_maize[is.na(endline_2024$q41)] <- NA

###gnuts sold? (yes/no)
set <- c("trans1.1..q47a", "trans1.2..q47a","trans1.3..q47a","trans1.4..q47a","trans1.5..q47a","trans1.6..q47a")
endline_2024[set] <- lapply(endline_2024[set], function(x)  as.numeric(as.POSIXct(x, format="%Y-%m-%d"))>=1704063600)
endline_2024$sold_gnuts <- (rowSums(endline_2024[set],na.rm=T) >0)
endline_2024$sold_gnuts[is.na(endline_2024$q46)] <- NA

#soybean sold? (yes/no)
set <- c("trans2.1..q52a", "trans2.2..q52a","trans2.3..q52a")
endline_2024[set] <- lapply(endline_2024[set], function(x)  as.numeric(as.POSIXct(x, format="%Y-%m-%d"))>=1704063600)
endline_2024$sold_soy <- (rowSums(endline_2024[set],na.rm=T) >0) 
endline_2024$sold_soy[is.na(endline_2024$q50)] <- NA 

dta$sold_maize <-  merge(dta,endline_2024[c("farmer_ID","sold_maize")],by="farmer_ID", all.x=T  )$sold_maize
dta$sold_gnuts <-  merge(dta,endline_2024[c("farmer_ID","sold_gnuts")],by="farmer_ID", all.x=T  )$sold_gnuts
dta$sold_soy <- merge(dta,endline_2024[c("farmer_ID","sold_soy")],by="farmer_ID", all.x=T  )$sold_soy

### quantity sold - maize
set <- c("trans.1..group2.q43c", "trans.2..group2.q43c","trans.3..group2.q43c","trans.4..group2.q43c")
endline_2024[set] <- lapply(endline_2024[set],  function(x) as.numeric(as.character(x)))
endline_2024[set] <- lapply(endline_2024[set],  function(x) replace(x,is.na(x),0))
endline_2024$sold_maize_kg <- rowSums(endline_2024[set]) 
endline_2024$sold_maize_kg[is.na(endline_2024$q41)] <- NA
dta$sold_maize_kg <- merge(dta,endline_2024[c("farmer_ID","sold_maize_kg")],by="farmer_ID", all.x=T)$sold_maize_kg

set_1 <- c("trans.1..q43a", "trans.2..q43a", "trans.3..q43a", "trans.4..q43a")
endline_2024$sold_maize_kg_2 <- rowSums(endline_2024[set]*endline_2024[set_1], na.rm=T)
endline_2024$sold_maize_kg_2[is.na(endline_2024$q41)] <- NA
dta$sold_maize_kg <- merge(dta,endline_2024[c("farmer_ID","sold_maize_kg_2")],by="farmer_ID", all.x=T)$sold_maize_kg_2

### quantity sold - gnuts
set <- c("trans1.1..group4.q47c", "trans1.2..group4.q47c","trans1.3..group4.q47c","trans1.4..group4.q47c","trans1.5..group4.q47c","trans1.6..group4.q47c")
endline_2024[set] <- lapply(endline_2024[set],  function(x) as.numeric(as.character(x)))
endline_2024[set] <- lapply(endline_2024[set],  function(x) replace(x,is.na(x),0))
endline_2024$sold_gnuts_kg <- rowSums(endline_2024[set]) 
endline_2024$sold_gnuts_kg[is.na(endline_2024$q46)] <- NA
dta$sold_gnuts_kg <- merge(dta,endline_2024[c("farmer_ID","sold_gnuts_kg")],by="farmer_ID", all.x=T)$sold_gnuts_kg

set_1 <- c("trans1.1..q47a", "trans1.2..q47a","trans1.3..q47a","trans1.4..q47a","trans1.5..q47a","trans1.6..q47a")
endline_2024$sold_gnuts_kg_2 <- rowSums(endline_2024[set]*endline_2024[set_1], na.rm=T)
endline_2024$sold_gnuts_kg_2[is.na(endline_2024$q46)] <- NA
dta$sold_gnuts_kg <- merge(dta,endline_2024[c("farmer_ID","sold_gnuts_kg_2")],by="farmer_ID", all.x=T)$sold_gnuts_kg_2

### quantity sold - soy  
set <- c("trans2.1..group6.q52c", "trans2.2..group6.q52c","trans2.3..group6.q52c")
endline_2024[set] <- lapply(endline_2024[set],  function(x) as.numeric(as.character(x)))
endline_2024[set] <- lapply(endline_2024[set],  function(x) replace(x,is.na(x),0))
endline_2024$sold_soy_kg <- rowSums(endline_2024[set]) 
endline_2024$sold_soy_kg[is.na(endline_2024$q50)] <- NA 
dta$sold_soy_kg <- merge(dta,endline_2024[c("farmer_ID","sold_soy_kg")],by="farmer_ID", all.x=T)$sold_soy_kg

set_1 <- c("trans2.1..q52a", "trans2.2..q52a","trans2.3..q52a")
endline_2024$sold_soy_kg_2 <- rowSums(endline_2024[set]*endline_2024[set_1], na.rm=T)
endline_2024$sold_soy_kg_2[is.na(endline_2024$q50)] <- NA 
dta$sold_soy_kg <- merge(dta,endline_2024[c("farmer_ID","sold_soy_kg_2")],by="farmer_ID", all.x=T)$sold_soy_kg_2

### price -maize
set <-c("trans.1..group2.q43d", "trans.2..group2.q43d","trans.3..group2.q43d","trans.4..group2.q43d")
endline_2024[set] <- lapply(endline_2024[set],  function(x) as.numeric(as.character(x)))
endline_2024$price_maize <- rowMeans(endline_2024[set],na.rm=T) 
dta$price_maize <- merge(dta,endline_2024[c("farmer_ID","price_maize")],by="farmer_ID", all.x=T)$price_maize

##multiply prices with indicator of sales post treatment 
set_1 <- c("trans.1..q43a", "trans.2..q43a", "trans.3..q43a", "trans.4..q43a")
endline_2024$price_maize_2 <-rowMeans(endline_2024[set]*endline_2024[set_1],na.rm=T)
endline_2024$price_maize_2[endline_2024$price_maize_2==0] <- NA
dta$price_maize <- merge(dta,endline_2024[c("farmer_ID","price_maize_2")],by="farmer_ID", all.x=T)$price_maize_2
### price - gnuts
set <-  c("trans1.1..group4.q47d", "trans1.2..group4.q47d","trans1.3..group4.q47d","trans1.4..group4.q47d","trans1.5..group4.q47d","trans1.6..group4.q47d")
endline_2024[set] <- lapply(endline_2024[set],  function(x) as.numeric(as.character(x)))
endline_2024$price_gnuts <- rowMeans(endline_2024[set],na.rm=T) 
dta$price_gnuts <- merge(dta,endline_2024[c("farmer_ID","price_gnuts")],by="farmer_ID", all.x=T)$price_gnuts

set_1 <- c("trans1.1..q47a", "trans1.2..q47a","trans1.3..q47a","trans1.4..q47a","trans1.5..q47a","trans1.6..q47a")
endline_2024$price_gnuts_2 <- rowMeans(endline_2024[set]*endline_2024[set_1], na.rm=T)
endline_2024$price_gnuts_2[endline_2024$price_gnuts_2==0] <- NA
dta$price_gnuts <- merge(dta,endline_2024[c("farmer_ID","price_gnuts_2")],by="farmer_ID", all.x=T)$price_gnuts_2
### price - soy
set <- c("trans2.1..group6.q52d", "trans2.2..group6.q52d","trans2.3..group6.q52d")
endline_2024[set] <- lapply(endline_2024[set],  function(x) as.numeric(as.character(x)))
endline_2024$price_soy <- rowMeans(endline_2024[set],na.rm=T) 
dta$price_soy <- merge(dta,endline_2024[c("farmer_ID","price_soy")],by="farmer_ID", all.x=T)$price_soy

set_1 <- c("trans2.1..q52a", "trans2.2..q52a","trans2.3..q52a")
endline_2024$price_soy_2 <- rowMeans(endline_2024[set]*endline_2024[set_1], na.rm=T)
endline_2024$price_soy_2[endline_2024$price_soy_2==0] <- NA
dta$price_soy <- merge(dta,endline_2024[c("farmer_ID","price_soy_2")],by="farmer_ID", all.x=T)$price_soy_2

###secondary outcomes - who decided sales maize
set <- c("trans.1..group2.q43f", "trans.2..group2.q43f","trans.3..group2.q43f","trans.4..group2.q43f")
endline_2024[set] <- lapply(endline_2024[set],  function(x) as.numeric(as.character(x))==5)
set_1 <- c("trans.1..q43a", "trans.2..q43a", "trans.3..q43a", "trans.4..q43a")
endline_2024$joint_maize <- rowMeans(endline_2024[set]*endline_2024[set_1],na.rm=T)==1 
dta$joint_maize <- merge(dta,endline_2024[c("farmer_ID","joint_maize")],by="farmer_ID", all.x=T)$joint_maize

###secondary outcomes - who decided sales gnuts
set <- c("trans1.1..group4.q47f", "trans1.2..group4.q47f","trans1.3..group4.q47f","trans1.4..group4.q47f","trans1.5..group4.q47f","trans1.6..group4.q47f")
endline_2024[set] <- lapply(endline_2024[set],  function(x) as.numeric(as.character(x))==5)
set_1 <- c("trans1.1..q47a", "trans1.2..q47a","trans1.3..q47a","trans1.4..q47a","trans1.5..q47a","trans1.6..q47a")
endline_2024$joint_gnuts <- rowMeans(endline_2024[set]*endline_2024[set_1], na.rm=T)==1 
dta$joint_gnuts <- merge(dta,endline_2024[c("farmer_ID","joint_gnuts")],by="farmer_ID", all.x=T)$joint_gnuts

###secondary outcomes - who decided sales soy
set <- c("trans2.1..group6.q52f", "trans2.2..group6.q52f","trans2.3..group6.q52f")
endline_2024[set] <- lapply(endline_2024[set],  function(x) as.numeric(as.character(x))==5)
set_1 <- c("trans2.1..q52a", "trans2.2..q52a","trans2.3..q52a")
endline_2024$joint_soy <- rowMeans(endline_2024[set]*endline_2024[set_1], na.rm=T)==1 
dta$joint_soy <- merge(dta,endline_2024[c("farmer_ID","joint_soy")],by="farmer_ID", all.x=T)$joint_soy

##what market sold to? maize
set <- c("trans.1..group2.q43e", "trans.2..group2.q43e","trans.3..group2.q43e","trans.4..group2.q43e")
endline_2024[set] <- lapply(endline_2024[set],  function(x) as.numeric(as.character(x))==2)
set_1 <- c("trans.1..q43a", "trans.2..q43a", "trans.3..q43a", "trans.4..q43a")
endline_2024$market_maize <- rowMeans(endline_2024[set]*endline_2024[set_1],na.rm=T)==1 
dta$market_maize <- merge(dta,endline_2024[c("farmer_ID","market_maize")],by="farmer_ID", all.x=T)$market_maize
##what market sold to? gnuts
set <- c("trans1.1..group4.q47e", "trans1.2..group4.q47e","trans1.3..group4.q47e","trans1.4..group4.q47e","trans1.5..group4.q47e","trans1.6..group4.q47e")
endline_2024[set] <- lapply(endline_2024[set],  function(x) as.numeric(as.character(x))==2)
set_1 <- c("trans1.1..q47a", "trans1.2..q47a","trans1.3..q47a","trans1.4..q47a","trans1.5..q47a","trans1.6..q47a")
endline_2024$market_gnuts <- rowMeans(endline_2024[set]*endline_2024[set_1], na.rm=T)==1 
dta$market_gnuts <- merge(dta,endline_2024[c("farmer_ID","market_gnuts")],by="farmer_ID", all.x=T)$market_gnuts
##what market sold to? soy
set <- c("trans2.1..group6.q52e", "trans2.2..group6.q52e","trans2.3..group6.q52e")
endline_2024[set] <- lapply(endline_2024[set],  function(x) as.numeric(as.character(x))==2)
set_1 <- c("trans2.1..q52a", "trans2.2..q52a","trans2.3..q52a")
endline_2024$market_soy <- rowMeans(endline_2024[set]*endline_2024[set_1], na.rm=T)==1 
dta$market_soy <- merge(dta,endline_2024[c("farmer_ID","market_soy")],by="farmer_ID", all.x=T)$market_soy

### proceeds used for education
##maize
endline_2024_c <- endline_2024
set <- c("trans.1..group2.q43g", "trans.2..group2.q43g","trans.3..group2.q43g","trans.4..group2.q43g")
endline_2024[set] <- lapply(endline_2024_c[set],  function(x) as.numeric(as.character(x))==7)
set_1 <- c("trans.1..q43a", "trans.2..q43a", "trans.3..q43a", "trans.4..q43a")
endline_2024$edu_maize <- rowMeans(endline_2024[set]*endline_2024[set_1],na.rm=T)>0 
dta$edu_maize <- merge(dta,endline_2024[c("farmer_ID","edu_maize")],by="farmer_ID", all.x=T)$edu_maize
##gnuts
set <- c("trans1.1..group4.q47g", "trans1.2..group4.q47g","trans1.3..group4.q47g","trans1.4..group4.q47g","trans1.5..group4.q47g","trans1.6..group4.q47g")
endline_2024[set] <- lapply(endline_2024_c[set],  function(x) as.numeric(as.character(x))==7)
set_1 <- c("trans1.1..q47a", "trans1.2..q47a","trans1.3..q47a","trans1.4..q47a","trans1.5..q47a","trans1.6..q47a")
endline_2024$edu_gnuts <- rowMeans(endline_2024[set]*endline_2024[set_1], na.rm=T)>0 
dta$edu_gnuts <- merge(dta,endline_2024[c("farmer_ID","edu_gnuts")],by="farmer_ID", all.x=T)$edu_gnuts
##for soy
set <- c("trans2.1..group6.q52g", "trans2.2..group6.q52g","trans2.3..group6.q52g")
endline_2024[set] <- lapply(endline_2024_c[set],  function(x) as.numeric(as.character(x))==7)
set_1 <- c("trans2.1..q52a", "trans2.2..q52a","trans2.3..q52a")
endline_2024$edu_soy <- rowMeans(endline_2024[set]*endline_2024[set_1], na.rm=T)>0 
dta$edu_soy <- merge(dta,endline_2024[c("farmer_ID","edu_soy")],by="farmer_ID", all.x=T)$edu_soy

###expenditure for health
set <- c("trans.1..group2.q43g", "trans.2..group2.q43g","trans.3..group2.q43g","trans.4..group2.q43g")
endline_2024[set] <- lapply(endline_2024_c[set],  function(x) as.numeric(as.character(x))==4)
set_1 <- c("trans.1..q43a", "trans.2..q43a", "trans.3..q43a", "trans.4..q43a")
endline_2024$health_maize <-  rowMeans(endline_2024[set]*endline_2024[set_1],na.rm=T)>0 
dta$health_maize <- merge(dta,endline_2024[c("farmer_ID","health_maize")],by="farmer_ID", all.x=T)$health_maize

set <- c("trans1.1..group4.q47g", "trans1.2..group4.q47g","trans1.3..group4.q47g","trans1.4..group4.q47g","trans1.5..group4.q47g","trans1.6..group4.q47g")
endline_2024[set] <- lapply(endline_2024_c[set],  function(x) as.numeric(as.character(x))==4)
set_1 <- c("trans1.1..q47a", "trans1.2..q47a","trans1.3..q47a","trans1.4..q47a","trans1.5..q47a","trans1.6..q47a")
endline_2024$health_gnuts <- rowMeans(endline_2024[set]*endline_2024[set_1], na.rm=T)>0 
dta$health_gnuts <- merge(dta,endline_2024[c("farmer_ID","health_gnuts")],by="farmer_ID", all.x=T)$health_gnuts

set <- c("trans2.1..group6.q52g", "trans2.2..group6.q52g","trans2.3..group6.q52g")
endline_2024[set] <- lapply(endline_2024_c[set],  function(x) as.numeric(as.character(x))==4)
set_1 <- c("trans2.1..q52a", "trans2.2..q52a","trans2.3..q52a")
endline_2024$health_soy <- rowMeans(endline_2024[set]*endline_2024[set_1], na.rm=T)>0 
dta$health_soy <- merge(dta,endline_2024[c("farmer_ID","health_soy")],by="farmer_ID", all.x=T)$health_soy

endline_2024$x1[endline_2024$x1=="n/a"] <- NA
endline_2024$x4[endline_2024$x4=="n/a"] <- NA
endline_2024$x7[endline_2024$x7=="n/a"] <- NA

dta$bought_maize <- merge(dta,endline_2024[c("farmer_ID","x1")],by="farmer_ID", all.x=T  )$x1=="Yes"
dta$bought_gnuts <- merge(dta,endline_2024[c("farmer_ID","x4")],by="farmer_ID", all.x=T  )$x4=="Yes"
dta$bought_soy <-merge(dta,endline_2024[c("farmer_ID","x7")],by="farmer_ID", all.x=T  )$x7=="Yes"


dta$bought_maize_amt <- as.numeric(as.character(merge(dta,endline_2024[c("farmer_ID","x2")],by="farmer_ID", all.x=T  )$x2))
dta$bought_gnuts_amt <- as.numeric(as.character(merge(dta,endline_2024[c("farmer_ID","x5")],by="farmer_ID", all.x=T  )$x5))
dta$bought_soy_amt <- as.numeric(as.character(merge(dta,endline_2024[c("farmer_ID","x8")],by="farmer_ID", all.x=T  )$x8))

dta$bought_maize_amt[!(dta$bought_maize)] <- 0
dta$bought_gnuts_amt[!(dta$bought_gnuts)] <- 0 
dta$bought_soy_amt[!(dta$bought_soy)] <- 0 

#dta$bought_maize_amt[dta$sold_maize==FALSE] <- NA 
#dta$bought_gnuts_amt[dta$sold_gnuts==FALSE] <- NA 
#dta$bought_soy_amt[dta$sold_soy==FALSE] <- NA 


dta$bought_maize_price <- as.numeric(as.character(merge(dta,endline_2024[c("farmer_ID","x3")],by="farmer_ID", all.x=T  )$x3))
dta$bought_gnuts_price <- as.numeric(as.character(merge(dta,endline_2024[c("farmer_ID","x6")],by="farmer_ID", all.x=T  )$x6))
dta$bought_soy_price <- as.numeric(as.character(merge(dta,endline_2024[c("farmer_ID","x9")],by="farmer_ID", all.x=T  )$x9))


#dta$bought_maize_price[dta$sold_maize==FALSE] <- NA 
#dta$bought_gnuts_price[dta$sold_gnuts==FALSE] <- NA 
#dta$bought_soy_price[dta$sold_soy==FALSE] <- NA 

#caclulate production of maize -  no production data collected in jan 2024 midline
#dta$prod_maize <- as.numeric(as.character(merge(dta,endline_2024[c("farmer_ID","maize_hrv")],by="farmer_ID", all.x=T  )$maize_hrv))
#dta$prod_gnuts <- as.numeric(as.character(merge(dta,endline_2024[c("farmer_ID","gnut_hrv")],by="farmer_ID", all.x=T  )$gnut_hrv))
#dta$prod_soy <- as.numeric(as.character(merge(dta,endline_2024[c("farmer_ID","soy_hrv")],by="farmer_ID", all.x=T  )$soy_hrv))

#expected prices
dta$price_dec_maize <- as.numeric(as.character(merge(dta,endline_2024[c("farmer_ID","M_exp1")],by="farmer_ID", all.x=T  )$M_exp1))
dta$price_dec_soy <- as.numeric(as.character(merge(dta,endline_2024[c("farmer_ID","S_exp1")],by="farmer_ID", all.x=T  )$S_exp1))
dta$price_dec_gnuts <- as.numeric(as.character(merge(dta,endline_2024[c("farmer_ID","G_exp1")],by="farmer_ID", all.x=T  )$G_exp1))



#dta$prod_maize[dta$prod_maize>400] <- NA 
#dta$prod_gnuts[dta$prod_gnuts>200] <- NA 
#dta$prod_soy[dta$prod_soy>200] <- NA 

dta <- trim("sold_maize_kg",dta)
dta <- trim("sold_gnuts_kg",dta)
dta <- trim("sold_soy_kg",dta)


dta <- trim("price_maize",dta)
dta <- trim("price_gnuts",dta)
dta <- trim("price_soy",dta)


dta$sold_maize_b[is.na(dta$sold_maize_b)]<- 0
dta$sold_gnuts_b[is.na(dta$sold_gnuts_b)]<- 0
dta$sold_soy_b[is.na(dta$sold_soy_b)]<- 0


dta$sold_maize_kg_b[is.na(dta$sold_maize_kg_b)] <-0
dta$sold_gnuts_kg_b[is.na(dta$sold_gnuts_kg_b)] <-0
dta$sold_soy_kg_b[is.na(dta$sold_soy_kg_b)] <-0

## trim stocks as this is a continuous variable - also check skewness
## edit: better to remove outliers (likely to be kg instead of bags)
### we need a better way here - compare to produced and sold???
dta <- trim("stock_maize_abs",dta)
dta <- trim("stock_gnuts_abs", dta)
dta <- trim("stock_soy_abs", dta)



#first primary outcome - stocks in absolute value

prim_maize <- matrix(NA,14,22)
prim_maize_tc <- matrix(NA,14,22)

prim_maize[1,1] <- mean(dta$stock_maize_abs[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize[1,2] <- sd(dta$stock_maize_abs[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize_tc[1,1] <- mean(dta$stock_maize_abs[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize_tc[1,2] <- sd(dta$stock_maize_abs[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize[1,c(4,7,10,13,16,19)] <- summary(lm(ihs(stock_maize_abs)~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_maize[1,c(4,7,10,13,16,19)+1] <- summary(lm(ihs(stock_maize_abs)~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_maize[1,c(4,7,10,13,16,19)+2] <- summary(lm(ihs(stock_maize_abs)~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,4]
prim_maize[1,22]  <- nobs(lm(ihs(stock_maize_abs)~treat2_assigned + fe_vil,data=dta))

prim_maize_tc[1,c(4,7,10,13,16,19)] <- summary(lm(ihs(stock_maize_abs)~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_maize_tc[1,c(4,7,10,13,16,19)+1] <- summary(lm(ihs(stock_maize_abs)~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_maize_tc[1,c(4,7,10,13,16,19)+2] <- summary(lm(ihs(stock_maize_abs)~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_maize_tc[1,22]  <- nobs(lm(ihs(stock_maize_abs)~treat2_assigned,data=dta))

prim_gnuts <- matrix(NA,14,22)
prim_gnuts_tc <- matrix(NA,14,22)
prim_gnuts[1,1] <- mean(dta$stock_gnuts_abs[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts[1,2] <- sd(dta$stock_gnuts_abs[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts_tc[1,1] <- mean(dta$stock_gnuts_abs[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts_tc[1,2] <- sd(dta$stock_gnuts_abs[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts[1,c(4,7,10,13,16,19)]  <- summary(lm(ihs(stock_gnuts_abs)~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_gnuts[1,c(4,7,10,13,16,19)+1]  <- summary(lm(ihs(stock_gnuts_abs)~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_gnuts[1,c(4,7,10,13,16,19)+2]  <- summary(lm(ihs(stock_gnuts_abs)~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,4]
prim_gnuts[1,22]  <- nobs(lm(ihs(stock_gnuts_abs)~treat2_assigned + fe_vil,data=dta))

prim_gnuts_tc[1,c(4,7,10,13,16,19)] <- summary(lm(ihs(stock_gnuts_abs)~treat2_assigned ,data=dta))$coefficients[2:7,1]
prim_gnuts_tc[1,c(4,7,10,13,16,19)+1] <- summary(lm(ihs(stock_gnuts_abs)~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_gnuts_tc[1,c(4,7,10,13,16,19)+2] <- summary(lm(ihs(stock_gnuts_abs)~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_gnuts_tc[1,22]  <- nobs(lm(ihs(stock_gnuts_abs)~treat2_assigned ,data=dta))

prim_soy <- matrix(NA,14,22)
prim_soy_tc <- matrix(NA,14,22)
prim_soy[1,1] <- mean(dta$stock_soy_abs[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy[1,2] <- sd(dta$stock_soy_abs[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy_tc[1,1] <- mean(dta$stock_soy_abs[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy_tc[1,2] <- sd(dta$stock_soy_abs[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy[1,c(4,7,10,13,16,19)] <- summary(lm(ihs(stock_soy_abs)~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_soy[1,c(4,7,10,13,16,19)+1] <- summary(lm(ihs(stock_soy_abs)~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_soy[1,c(4,7,10,13,16,19)+2] <- summary(lm(ihs(stock_soy_abs)~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,4]

prim_soy[1,22] <- nobs(lm(ihs(stock_soy_abs)~treat2_assigned + fe_vil,data=dta))

prim_soy_tc[1,c(4,7,10,13,16,19)] <- summary(lm(ihs(stock_soy_abs)~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_soy_tc[1,c(4,7,10,13,16,19)+1] <- summary(lm(ihs(stock_soy_abs)~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_soy_tc[1,c(4,7,10,13,16,19)+2] <- summary(lm(ihs(stock_soy_abs)~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_soy_tc[1,22] <- nobs(lm(ihs(stock_soy_abs)~treat2_assigned,data=dta))

### second primary outcome - sold? yes or no
prim_maize[3,1] <- mean(dta$sold_maize[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize[3,2] <- sd(dta$sold_maize[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize_tc[3,1] <- mean(dta$sold_maize[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize_tc[3,2] <- sd(dta$sold_maize[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize[3,c(4,7,10,13,16,19)] <- summary(lm(sold_maize~treat2_assigned + sold_maize_b + fe_vil,data=dta))$coefficients[2:7,1]
prim_maize[3,c(4,7,10,13,16,19)+1] <- summary(lm(sold_maize~treat2_assigned + sold_maize_b + fe_vil,data=dta))$coefficients[2:7,2]
prim_maize[3,c(4,7,10,13,16,19)+2] <- summary(lm(sold_maize~treat2_assigned + sold_maize_b + fe_vil,data=dta))$coefficients[2:7,4]

prim_maize[3,22] <- nobs(lm(sold_maize~treat2_assigned + sold_maize_b + fe_vil,data=dta))

prim_maize_tc[3,c(4,7,10,13,16,19)] <- summary(lm(sold_maize~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_maize_tc[3,c(4,7,10,13,16,19)+1] <- summary(lm(sold_maize~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_maize_tc[3,c(4,7,10,13,16,19)+2] <- summary(lm(sold_maize~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_maize_tc[3,22] <- nobs(lm(sold_maize~treat2_assigned,data=dta))

prim_gnuts[3,1] <- mean(dta$sold_gnuts[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts[3,2] <- sd(dta$sold_gnuts[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts_tc[3,1] <- mean(dta$sold_gnuts[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts_tc[3,2] <- sd(dta$sold_gnuts[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts[3,c(4,7,10,13,16,19)] <- summary(lm(sold_gnuts~treat2_assigned + sold_gnuts_b + fe_vil,data=dta))$coefficients[2:7,1]
prim_gnuts[3,c(4,7,10,13,16,19)+1] <- summary(lm(sold_gnuts~treat2_assigned + sold_gnuts_b + fe_vil,data=dta))$coefficients[2:7,2]
prim_gnuts[3,c(4,7,10,13,16,19)+2] <- summary(lm(sold_gnuts~treat2_assigned + sold_gnuts_b + fe_vil,data=dta))$coefficients[2:7,4]
prim_gnuts[3,22] <- nobs(lm(sold_gnuts~treat2_assigned + sold_gnuts_b + fe_vil,data=dta))

prim_gnuts_tc[3,c(4,7,10,13,16,19)] <- summary(lm(sold_gnuts~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_gnuts_tc[3,c(4,7,10,13,16,19)+1] <- summary(lm(sold_gnuts~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_gnuts_tc[3,c(4,7,10,13,16,19)+2] <- summary(lm(sold_gnuts~treat2_assigned,data=dta))$coefficients[2:7,4]

prim_gnuts_tc[3,22] <- nobs(lm(sold_gnuts~treat2_assigned,data=dta))

prim_soy[3,1] <- mean(dta$sold_soy[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy[3,2] <- sd(dta$sold_soy[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy_tc[3,1] <- mean(dta$sold_soy[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy_tc[3,2] <- sd(dta$sold_soy[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy[3,c(4,7,10,13,16,19)] <- summary(lm(sold_soy~treat2_assigned + sold_soy_b + fe_vil,data=dta))$coefficients[2:7,1]
prim_soy[3,c(4,7,10,13,16,19)+1] <- summary(lm(sold_soy~treat2_assigned + sold_soy_b + fe_vil,data=dta))$coefficients[2:7,2]
prim_soy[3,c(4,7,10,13,16,19)+2] <- summary(lm(sold_soy~treat2_assigned + sold_soy_b + fe_vil,data=dta))$coefficients[2:7,4]
prim_soy[3,22] <- nobs(lm(sold_soy~treat2_assigned + sold_soy_b + fe_vil,data=dta))
prim_soy_tc[3,c(4,7,10,13,16,19)] <- summary(lm(sold_soy~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_soy_tc[3,c(4,7,10,13,16,19)+1] <- summary(lm(sold_soy~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_soy_tc[3,c(4,7,10,13,16,19)+2] <- summary(lm(sold_soy~treat2_assigned,data=dta))$coefficients[2:7,4]

prim_soy_tc[3,22] <- nobs(lm(sold_soy~treat2_assigned,data=dta))

#third primary outcome - kg sold

prim_maize[4,1] <- mean(dta$sold_maize_kg[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize[4,2] <- sd(dta$sold_maize_kg[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize_tc[4,1] <- mean(dta$sold_maize_kg[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize_tc[4,2] <- sd(dta$sold_maize_kg[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize[4,c(4,7,10,13,16,19)] <- summary(lm(ihs(sold_maize_kg)~treat2_assigned + sold_maize_kg_b + fe_vil,data=dta))$coefficients[2:7,1]
prim_maize[4,c(4,7,10,13,16,19)+1] <- summary(lm(ihs(sold_maize_kg)~treat2_assigned + sold_maize_kg_b + fe_vil,data=dta))$coefficients[2:7,2]
prim_maize[4,c(4,7,10,13,16,19)+2] <- summary(lm(ihs(sold_maize_kg)~treat2_assigned + sold_maize_kg_b + fe_vil,data=dta))$coefficients[2:7,4]

prim_maize[4,22] <- nobs(lm(ihs(sold_maize_kg)~treat2_assigned + sold_maize_kg_b + fe_vil,data=dta))

prim_maize_tc[4,c(4,7,10,13,16,19)] <- summary(lm(ihs(sold_maize_kg)~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_maize_tc[4,c(4,7,10,13,16,19)+1] <- summary(lm(ihs(sold_maize_kg)~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_maize_tc[4,c(4,7,10,13,16,19)+2] <- summary(lm(ihs(sold_maize_kg)~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_maize_tc[4,22] <- nobs(lm(ihs(sold_maize_kg)~treat2_assigned,data=dta))

prim_gnuts[4,1] <- mean(dta$sold_gnuts_kg[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts[4,2] <- sd(dta$sold_gnuts_kg[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts_tc[4,1] <- mean(dta$sold_gnuts_kg[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts_tc[4,2] <- sd(dta$sold_gnuts_kg[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts[4,c(4,7,10,13,16,19)] <- summary(lm(ihs(sold_gnuts_kg)~treat2_assigned + sold_gnuts_kg_b + fe_vil,data=dta))$coefficients[2:7,1]
prim_gnuts[4,c(4,7,10,13,16,19)+1] <- summary(lm(ihs(sold_gnuts_kg)~treat2_assigned + sold_gnuts_kg_b + fe_vil,data=dta))$coefficients[2:7,2]
prim_gnuts[4,c(4,7,10,13,16,19)+2] <- summary(lm(ihs(sold_gnuts_kg)~treat2_assigned + sold_gnuts_kg_b + fe_vil,data=dta))$coefficients[2:7,4]
prim_gnuts[4,22] <- nobs(lm(ihs(sold_gnuts_kg)~treat2_assigned + sold_gnuts_kg_b + fe_vil,data=dta))

prim_gnuts_tc[4,c(4,7,10,13,16,19)] <- summary(lm(ihs(sold_gnuts_kg)~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_gnuts_tc[4,c(4,7,10,13,16,19)+1] <- summary(lm(ihs(sold_gnuts_kg)~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_gnuts_tc[4,c(4,7,10,13,16,19)+2] <- summary(lm(ihs(sold_gnuts_kg)~treat2_assigned,data=dta))$coefficients[2:7,4]

prim_gnuts_tc[4,22] <- nobs(lm(ihs(sold_gnuts_kg)~treat2_assigned,data=dta))

prim_soy[4,1] <- mean(dta$sold_soy_kg[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy[4,2] <- sd(dta$sold_soy_kg[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy_tc[4,1] <- mean(dta$sold_soy_kg[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy_tc[4,2] <- sd(dta$sold_soy_kg[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy[4,c(4,7,10,13,16,19)] <- summary(lm(ihs(sold_soy_kg)~treat2_assigned + sold_soy_kg_b + fe_vil,data=dta))$coefficients[2:7,1]
prim_soy[4,c(4,7,10,13,16,19)+1] <- summary(lm(ihs(sold_soy_kg)~treat2_assigned + sold_soy_kg_b + fe_vil,data=dta))$coefficients[2:7,2]
prim_soy[4,c(4,7,10,13,16,19)+2] <- summary(lm(ihs(sold_soy_kg)~treat2_assigned + sold_soy_kg_b + fe_vil,data=dta))$coefficients[2:7,4]
prim_soy[4,22] <- nobs(lm(ihs(sold_soy_kg)~treat2_assigned + sold_soy_kg_b + fe_vil,data=dta))

prim_soy_tc[4,c(4,7,10,13,16,19)] <- summary(lm(ihs(sold_soy_kg)~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_soy_tc[4,c(4,7,10,13,16,19)+1] <- summary(lm(ihs(sold_soy_kg)~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_soy_tc[4,c(4,7,10,13,16,19)+2] <- summary(lm(ihs(sold_soy_kg)~treat2_assigned,data=dta))$coefficients[2:7,4]

prim_soy_tc[4,22] <- nobs(lm(ihs(sold_soy_kg)~treat2_assigned,data=dta))

# fourth primary outcome - price 

prim_maize[6,1] <- mean(dta$price_maize[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize[6,2] <- sd(dta$price_maize[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize_tc[6,1] <- mean(dta$price_maize[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize_tc[6,2] <- sd(dta$price_maize[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize[6,c(4,7,10,13,16,19)] <- summary(lm(price_maize~treat2_assigned  +fe_vil,data=dta))$coefficients[2:7,1]
prim_maize[6,c(4,7,10,13,16,19)+1] <- summary(lm(price_maize~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,2]
prim_maize[6,c(4,7,10,13,16,19)+2] <- summary(lm(price_maize~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]
prim_maize[6,22] <- nobs(lm(price_maize~treat2_assigned  + fe_vil,data=dta))

prim_maize_tc[6,c(4,7,10,13,16,19)] <- summary(lm(price_maize~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_maize_tc[6,c(4,7,10,13,16,19)+1] <- summary(lm(price_maize~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_maize_tc[6,c(4,7,10,13,16,19)+2] <- summary(lm(price_maize~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_maize_tc[6,22] <- nobs(lm(price_maize~treat2_assigned,data=dta))

prim_gnuts[6,1] <- mean(dta$price_gnuts[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts[6,2] <- sd(dta$price_gnuts[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts_tc[6,1] <- mean(dta$price_gnuts[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts_tc[6,2] <- sd(dta$price_gnuts[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts[6,c(4,7,10,13,16,19)] <- summary(lm(price_gnuts~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_gnuts[6,c(4,7,10,13,16,19)+1] <- summary(lm(price_gnuts~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_gnuts[6,c(4,7,10,13,16,19)+2] <- summary(lm(price_gnuts~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]
prim_gnuts[6,22] <- nobs(lm(price_gnuts~treat2_assigned  + fe_vil,data=dta))

prim_gnuts_tc[6,c(4,7,10,13,16,19)] <- summary(lm(price_gnuts~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_gnuts_tc[6,c(4,7,10,13,16,19)+1] <- summary(lm(price_gnuts~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_gnuts_tc[6,c(4,7,10,13,16,19)+2] <- summary(lm(price_gnuts~treat2_assigned,data=dta))$coefficients[2:7,4]

prim_gnuts_tc[6,22] <- nobs(lm(price_gnuts~treat2_assigned,data=dta))

prim_soy[6,1] <- mean(dta$price_soy[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy[6,2] <- sd(dta$price_soy[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy_tc[6,1] <- mean(dta$price_soy[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy_tc[6,2] <- sd(dta$price_soy[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy[6,c(4,7,10,13,16,19)] <- summary(lm(price_soy~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_soy[6,c(4,7,10,13,16,19)+1] <- summary(lm(price_soy~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_soy[6,c(4,7,10,13,16,19)+2] <- summary(lm(price_soy~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,4]
prim_soy[6,22] <- nobs(lm(price_soy~treat2_assigned + fe_vil,data=dta))

prim_soy_tc[6,c(4,7,10,13,16,19)] <- summary(lm(price_soy~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_soy_tc[6,c(4,7,10,13,16,19)+1] <- summary(lm(price_soy~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_soy_tc[6,c(4,7,10,13,16,19)+2] <- summary(lm(price_soy~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_soy_tc[6,22] <- nobs(lm(price_soy~treat2_assigned,data=dta))

###fifth primary outcome


prim_maize[7,1] <- mean(dta$bought_maize[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize[7,2] <- sd(dta$bought_maize[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize_tc[7,1] <- mean(dta$bought_maize[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize_tc[7,2] <- sd(dta$bought_maize[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize[7,c(4,7,10,13,16,19)] <- summary(lm(bought_maize~treat2_assigned  +fe_vil,data=dta))$coefficients[2:7,1]
prim_maize[7,c(4,7,10,13,16,19)+1] <- summary(lm(bought_maize~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,2]
prim_maize[7,c(4,7,10,13,16,19)+2] <- summary(lm(bought_maize~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]

prim_maize[7,22] <- nobs(lm(bought_maize~treat2_assigned  + fe_vil,data=dta))

prim_maize_tc[7,c(4,7,10,13,16,19)] <- summary(lm(bought_maize~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_maize_tc[7,c(4,7,10,13,16,19)+1] <- summary(lm(bought_maize~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_maize_tc[7,c(4,7,10,13,16,19)+2] <- summary(lm(bought_maize~treat2_assigned,data=dta))$coefficients[2:7,4]

prim_maize_tc[7,22] <- nobs(lm(bought_maize~treat2_assigned,data=dta))


prim_gnuts[7,1] <- mean(dta$bought_gnuts[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts[7,2] <- sd(dta$bought_gnuts[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts_tc[7,1] <- mean(dta$bought_gnuts[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts_tc[7,2] <- sd(dta$bought_gnuts[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts[7,c(4,7,10,13,16,19)] <- summary(lm(bought_gnuts~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_gnuts[7,c(4,7,10,13,16,19)+1] <- summary(lm(bought_gnuts~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_gnuts[7,c(4,7,10,13,16,19)+2] <- summary(lm(bought_gnuts~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]
prim_gnuts[7,22] <- nobs(lm(bought_gnuts~treat2_assigned  + fe_vil,data=dta))

prim_gnuts_tc[7,c(4,7,10,13,16,19)] <- summary(lm(bought_gnuts~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_gnuts_tc[7,c(4,7,10,13,16,19)+1] <- summary(lm(bought_gnuts~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_gnuts_tc[7,c(4,7,10,13,16,19)+2] <- summary(lm(bought_gnuts~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_gnuts_tc[7,22] <- nobs(lm(bought_gnuts~treat2_assigned,data=dta))


prim_soy[7,1] <- mean(dta$bought_soy[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy[7,2] <- sd(dta$bought_soy[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy_tc[7,1] <- mean(dta$bought_soy[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy_tc[7,2] <- sd(dta$bought_soy[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy[7,c(4,7,10,13,16,19)] <- summary(lm(bought_soy~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_soy[7,c(4,7,10,13,16,19)+1] <- summary(lm(bought_soy~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_soy[7,c(4,7,10,13,16,19)+2] <- summary(lm(bought_soy~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]
prim_soy[7,22] <- nobs(lm(bought_soy~treat2_assigned  + fe_vil,data=dta))

prim_soy_tc[7,c(4,7,10,13,16,19)] <- summary(lm(bought_soy~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_soy_tc[7,c(4,7,10,13,16,19)+1] <- summary(lm(bought_soy~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_soy_tc[7,c(4,7,10,13,16,19)+2] <- summary(lm(bought_soy~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_soy_tc[7,22] <- nobs(lm(bought_soy~treat2_assigned,data=dta))

###sixth primary outcome - ihs(bought_crop_amt)


prim_maize[8,1] <- mean(dta$bought_maize_amt[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize[8,2] <- sd(dta$bought_maize_amt[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize_tc[8,1] <- mean(dta$bought_maize_amt[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize_tc[8,2] <- sd(dta$bought_maize_amt[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize[8,c(4,7,10,13,16,19)] <- summary(lm(ihs(bought_maize_amt)~treat2_assigned  +fe_vil,data=dta))$coefficients[2:7,1]
prim_maize[8,c(4,7,10,13,16,19)+1] <- summary(lm(ihs(bought_maize_amt)~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,2]
prim_maize[8,c(4,7,10,13,16,19)+2] <- summary(lm(ihs(bought_maize_amt)~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]
prim_maize[8,22] <- nobs(lm(ihs(bought_maize_amt)~treat2_assigned  + fe_vil,data=dta))

prim_maize_tc[8,c(4,7,10,13,16,19)] <- summary(lm(ihs(bought_maize_amt)~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_maize_tc[8,c(4,7,10,13,16,19)+1] <- summary(lm(ihs(bought_maize_amt)~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_maize_tc[8,c(4,7,10,13,16,19)+2] <- summary(lm(ihs(bought_maize_amt)~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_maize_tc[8,22] <- nobs(lm(ihs(bought_maize_amt)~treat2_assigned,data=dta))


prim_gnuts[8,1] <- mean(dta$bought_gnuts_amt[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts[8,2] <- sd(dta$bought_gnuts_amt[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts_tc[8,1] <- mean(dta$bought_gnuts_amt[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts_tc[8,2] <- sd(dta$bought_gnuts_amt[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts[8,c(4,7,10,13,16,19)] <- summary(lm(ihs(bought_gnuts_amt)~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_gnuts[8,c(4,7,10,13,16,19)+1] <- summary(lm(ihs(bought_gnuts_amt)~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_gnuts[8,c(4,7,10,13,16,19)+2] <- summary(lm(ihs(bought_gnuts_amt)~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]
prim_gnuts[8,22] <- nobs(lm(ihs(bought_gnuts_amt)~treat2_assigned  + fe_vil,data=dta))

prim_gnuts_tc[8,c(4,7,10,13,16,19)] <- summary(lm(ihs(bought_gnuts_amt)~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_gnuts_tc[8,c(4,7,10,13,16,19)+1] <- summary(lm(ihs(bought_gnuts_amt)~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_gnuts_tc[8,c(4,7,10,13,16,19)+2] <- summary(lm(ihs(bought_gnuts_amt)~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_gnuts_tc[8,22] <- nobs(lm(ihs(bought_gnuts_amt)~treat2_assigned,data=dta))


prim_soy[8,1] <- mean(dta$bought_soy_amt[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy[8,2] <- sd(dta$bought_soy_amt[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy_tc[8,1] <- mean(dta$bought_soy_amt[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy_tc[8,2] <- sd(dta$bought_soy_amt[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy[8,c(4,7,10,13,16,19)] <- summary(lm(ihs(bought_soy_amt)~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_soy[8,c(4,7,10,13,16,19)+1] <- summary(lm(ihs(bought_soy_amt)~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_soy[8,c(4,7,10,13,16,19)+2] <- summary(lm(ihs(bought_soy_amt)~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]
prim_soy[8,22] <- nobs(lm(ihs(bought_soy_amt)~treat2_assigned  + fe_vil,data=dta))

prim_soy_tc[8,c(4,7,10,13,16,19)] <- summary(lm(ihs(bought_soy_amt)~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_soy_tc[8,c(4,7,10,13,16,19)+1] <- summary(lm(ihs(bought_soy_amt)~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_soy_tc[8,c(4,7,10,13,16,19)+2] <- summary(lm(ihs(bought_soy_amt)~treat2_assigned,data=dta))$coefficients[2:7,4]

prim_soy_tc[8,22] <- nobs(lm(ihs(bought_soy_amt)~treat2_assigned,data=dta))

#seventh primary outcome - bought_crop_price


prim_maize[9,1] <- mean(dta$bought_maize_price[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize[9,2] <- sd(dta$bought_maize_price[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize_tc[9,1] <- mean(dta$bought_maize_price[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize_tc[9,2] <- sd(dta$bought_maize_price[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize[9,c(4,7,10,13,16,19)] <- summary(lm(bought_maize_price~treat2_assigned  +fe_vil,data=dta))$coefficients[2:7,1]
prim_maize[9,c(4,7,10,13,16,19)+1] <- summary(lm(bought_maize_price~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,2]
prim_maize[9,c(4,7,10,13,16,19)+2] <- summary(lm(bought_maize_price~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]
prim_maize[9,22] <- nobs(lm(bought_maize_price~treat2_assigned  + fe_vil,data=dta))

prim_maize_tc[9,c(4,7,10,13,16,19)] <- summary(lm(bought_maize_price~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_maize_tc[9,c(4,7,10,13,16,19)+1] <- summary(lm(bought_maize_price~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_maize_tc[9,c(4,7,10,13,16,19)+2] <- summary(lm(bought_maize_price~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_maize_tc[9,22] <- nobs(lm(bought_maize_price~treat2_assigned,data=dta))


prim_gnuts[9,1] <- mean(dta$bought_gnuts_price[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts[9,2] <- sd(dta$bought_gnuts_price[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts_tc[9,1] <- mean(dta$bought_gnuts_price[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts_tc[9,2] <- sd(dta$bought_gnuts_price[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts[9,c(4,7,10,13,16,19)] <- summary(lm(bought_gnuts_price~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_gnuts[9,c(4,7,10,13,16,19)+1] <- summary(lm(bought_gnuts_price~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_gnuts[9,c(4,7,10,13,16,19)+2] <- summary(lm(bought_gnuts_price~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]

prim_gnuts[9,22] <- nobs(lm(bought_gnuts_price~treat2_assigned  + fe_vil,data=dta))

prim_gnuts_tc[9,c(4,7,10,13,16,19)] <- summary(lm(bought_gnuts_price~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_gnuts_tc[9,c(4,7,10,13,16,19)+1] <- summary(lm(bought_gnuts_price~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_gnuts_tc[9,c(4,7,10,13,16,19)+2] <- summary(lm(bought_gnuts_price~treat2_assigned,data=dta))$coefficients[2:7,4]

prim_gnuts_tc[9,22] <- nobs(lm(bought_gnuts_price~treat2_assigned,data=dta))


prim_soy[9,1] <- mean(dta$bought_soy_price[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy[9,2] <- sd(dta$bought_soy_price[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy_tc[9,1] <- mean(dta$bought_soy_price[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy_tc[9,2] <- sd(dta$bought_soy_price[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy[9,c(4,7,10,13,16,19)] <- summary(lm(bought_soy_price~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_soy[9,c(4,7,10,13,16,19)+1] <- summary(lm(bought_soy_price~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_soy[9,c(4,7,10,13,16,19)+2] <- summary(lm(bought_soy_price~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]
prim_soy[9,22] <- nobs(lm(bought_soy_price~treat2_assigned  + fe_vil,data=dta))

prim_soy_tc[9,c(4,7,10,13,16,19)] <- summary(lm(bought_soy_price~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_soy_tc[9,c(4,7,10,13,16,19)+1] <- summary(lm(bought_soy_price~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_soy_tc[9,c(4,7,10,13,16,19)+2] <- summary(lm(bought_soy_price~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_soy_tc[9,22] <- nobs(lm(bought_soy_price~treat2_assigned,data=dta))

#10 th outcome - expected price in april


prim_maize[10,1] <- mean(dta$price_dec_maize[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize[10,2] <- sd(dta$price_dec_maize[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize_tc[10,1] <- mean(dta$price_dec_maize[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize_tc[10,2] <- sd(dta$price_dec_maize[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize[10,c(4,7,10,13,16,19)] <- summary(lm(price_dec_maize~treat2_assigned  +fe_vil,data=dta))$coefficients[2:7,1]
prim_maize[10,c(4,7,10,13,16,19)+1] <- summary(lm(price_dec_maize~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,2]
prim_maize[10,c(4,7,10,13,16,19)+2] <- summary(lm(price_dec_maize~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]

prim_maize[10,22] <- nobs(lm(price_dec_maize~treat2_assigned  + fe_vil,data=dta))

prim_maize_tc[10,c(4,7,10,13,16,19)] <- summary(lm(price_dec_maize~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_maize_tc[10,c(4,7,10,13,16,19)+1] <- summary(lm(price_dec_maize~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_maize_tc[10,c(4,7,10,13,16,19)+2] <- summary(lm(price_dec_maize~treat2_assigned,data=dta))$coefficients[2:7,4]

prim_maize_tc[10,22] <- nobs(lm(price_dec_maize~treat2_assigned,data=dta))


prim_gnuts[10,1] <- mean(dta$price_dec_gnuts[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts[10,2] <- sd(dta$price_dec_gnuts[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts_tc[10,1] <- mean(dta$price_dec_gnuts[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts_tc[10,2] <- sd(dta$price_dec_gnuts[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts[10,c(4,7,10,13,16,19)] <- summary(lm(price_dec_gnuts~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_gnuts[10,c(4,7,10,13,16,19)+1] <- summary(lm(price_dec_gnuts~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_gnuts[10,c(4,7,10,13,16,19)+2] <- summary(lm(price_dec_gnuts~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]

prim_gnuts[10,22] <- nobs(lm(price_dec_gnuts~treat2_assigned  + fe_vil,data=dta))

prim_gnuts_tc[10,c(4,7,10,13,16,19)] <- summary(lm(price_dec_gnuts~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_gnuts_tc[10,c(4,7,10,13,16,19)+1] <- summary(lm(price_dec_gnuts~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_gnuts_tc[10,c(4,7,10,13,16,19)+2] <- summary(lm(price_dec_gnuts~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_gnuts_tc[10,22] <- nobs(lm(price_dec_gnuts~treat2_assigned,data=dta))


prim_soy[10,1] <- mean(dta$price_dec_soy[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy[10,2] <- sd(dta$price_dec_soy[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy_tc[10,1] <- mean(dta$price_dec_soy[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy_tc[10,2] <- sd(dta$price_dec_soy[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy[10,c(4,7,10,13,16,19)] <- summary(lm(price_dec_soy~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_soy[10,c(4,7,10,13,16,19)+1] <- summary(lm(price_dec_soy~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_soy[10,c(4,7,10,13,16,19)+2] <- summary(lm(price_dec_soy~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]
prim_soy[10,22] <- nobs(lm(price_dec_soy~treat2_assigned  + fe_vil,data=dta))

prim_soy_tc[10,c(4,7,10,13,16,19)] <- summary(lm(price_dec_soy~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_soy_tc[10,c(4,7,10,13,16,19)+1] <- summary(lm(price_dec_soy~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_soy_tc[10,c(4,7,10,13,16,19)+2] <- summary(lm(price_dec_soy~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_soy_tc[10,22] <- nobs(lm(price_dec_soy~treat2_assigned,data=dta))

#11 th outcome - one if all decisions to sell were made jointly


prim_maize[11,1] <- mean(dta$joint_maize[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize[11,2] <- sd(dta$joint_maize[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize_tc[11,1] <- mean(dta$joint_maize[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize_tc[11,2] <- sd(dta$joint_maize[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize[11,c(4,7,10,13,16,19)] <- summary(lm(joint_maize~treat2_assigned  +fe_vil,data=dta))$coefficients[2:7,1]
prim_maize[11,c(4,7,10,13,16,19)+1] <- summary(lm(joint_maize~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,2]
prim_maize[11,c(4,7,10,13,16,19)+2] <- summary(lm(joint_maize~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]

prim_maize[11,22] <- nobs(lm(joint_maize~treat2_assigned  + fe_vil,data=dta))

prim_maize_tc[11,c(4,7,10,13,16,19)] <- summary(lm(joint_maize~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_maize_tc[11,c(4,7,10,13,16,19)+1] <- summary(lm(joint_maize~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_maize_tc[11,c(4,7,10,13,16,19)+2] <- summary(lm(joint_maize~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_maize_tc[11,22] <- nobs(lm(joint_maize~treat2_assigned,data=dta))


prim_gnuts[11,1] <- mean(dta$joint_gnuts[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts[11,2] <- sd(dta$joint_gnuts[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts_tc[11,1] <- mean(dta$joint_gnuts[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts_tc[11,2] <- sd(dta$joint_gnuts[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts[11,c(4,7,10,13,16,19)] <- summary(lm(joint_gnuts~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_gnuts[11,c(4,7,10,13,16,19)+1] <- summary(lm(joint_gnuts~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_gnuts[11,c(4,7,10,13,16,19)+2] <- summary(lm(joint_gnuts~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]

prim_gnuts[11,22] <- nobs(lm(joint_gnuts~treat2_assigned  + fe_vil,data=dta))

prim_gnuts_tc[11,c(4,7,10,13,16,19)] <- summary(lm(joint_gnuts~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_gnuts_tc[11,c(4,7,10,13,16,19)+1] <- summary(lm(joint_gnuts~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_gnuts_tc[11,c(4,7,10,13,16,19)+2] <- summary(lm(joint_gnuts~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_gnuts_tc[11,22] <- nobs(lm(joint_gnuts~treat2_assigned,data=dta))


prim_soy[11,1] <- mean(dta$joint_soy[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy[11,2] <- sd(dta$joint_soy[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy_tc[11,1] <- mean(dta$joint_soy[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy_tc[11,2] <- sd(dta$joint_soy[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy[11,c(4,7,10,13,16,19)] <- summary(lm(joint_soy~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_soy[11,c(4,7,10,13,16,19)+1] <- summary(lm(joint_soy~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_soy[11,c(4,7,10,13,16,19)+2] <- summary(lm(joint_soy~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]

prim_soy[11,22] <- nobs(lm(joint_soy~treat2_assigned  + fe_vil,data=dta))

prim_soy_tc[11,c(4,7,10,13,16,19)] <- summary(lm(joint_soy~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_soy_tc[11,c(4,7,10,13,16,19)+1] <- summary(lm(joint_soy~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_soy_tc[11,c(4,7,10,13,16,19)+2] <- summary(lm(joint_soy~treat2_assigned,data=dta))$coefficients[2:7,4]

prim_soy_tc[11,22] <- nobs(lm(joint_soy~treat2_assigned,data=dta))

#12 th outcome - consistently sold to market

prim_maize[12,1] <- mean(dta$market_maize[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize[12,2] <- sd(dta$market_maize[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize_tc[12,1] <- mean(dta$market_maize[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize_tc[12,2] <- sd(dta$market_maize[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize[12,c(4,7,10,13,16,19)] <- summary(lm(market_maize~treat2_assigned  +fe_vil,data=dta))$coefficients[2:7,1]
prim_maize[12,c(4,7,10,13,16,19)+1] <- summary(lm(market_maize~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,2]
prim_maize[12,c(4,7,10,13,16,19)+2] <- summary(lm(market_maize~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]
prim_maize[12,22] <- nobs(lm(market_maize~treat2_assigned  + fe_vil,data=dta))

prim_maize_tc[12,c(4,7,10,13,16,19)] <- summary(lm(market_maize~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_maize_tc[12,c(4,7,10,13,16,19)+1] <- summary(lm(market_maize~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_maize_tc[12,c(4,7,10,13,16,19)+2] <- summary(lm(market_maize~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_maize_tc[12,22] <- nobs(lm(market_maize~treat2_assigned,data=dta))


prim_gnuts[12,1] <- mean(dta$market_gnuts[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts[12,2] <- sd(dta$market_gnuts[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts_tc[12,1] <- mean(dta$market_gnuts[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts_tc[12,2] <- sd(dta$market_gnuts[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts[12,c(4,7,10,13,16,19)] <- summary(lm(market_gnuts~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_gnuts[12,c(4,7,10,13,16,19)+1] <- summary(lm(market_gnuts~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_gnuts[12,c(4,7,10,13,16,19)+2] <- summary(lm(market_gnuts~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]
prim_gnuts[12,22] <- nobs(lm(market_gnuts~treat2_assigned  + fe_vil,data=dta))

prim_gnuts_tc[12,c(4,7,10,13,16,19)] <- summary(lm(market_gnuts~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_gnuts_tc[12,c(4,7,10,13,16,19)+1] <- summary(lm(market_gnuts~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_gnuts_tc[12,c(4,7,10,13,16,19)+2] <- summary(lm(market_gnuts~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_gnuts_tc[12,22] <- nobs(lm(market_gnuts~treat2_assigned,data=dta))


prim_soy[12,1] <- mean(dta$market_soy[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy[12,2] <- sd(dta$market_soy[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy_tc[12,1] <- mean(dta$market_soy[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy_tc[12,2] <- sd(dta$market_soy[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy[12,c(4,7,10,13,16,19)] <- summary(lm(market_soy~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_soy[12,c(4,7,10,13,16,19)+1] <- summary(lm(market_soy~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_soy[12,c(4,7,10,13,16,19)+2] <- summary(lm(market_soy~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]

prim_soy[12,22] <- nobs(lm(market_soy~treat2_assigned  + fe_vil,data=dta))

prim_soy_tc[12,c(4,7,10,13,16,19)] <- summary(lm(market_soy~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_soy_tc[12,c(4,7,10,13,16,19)+1] <- summary(lm(market_soy~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_soy_tc[12,c(4,7,10,13,16,19)+2] <- summary(lm(market_soy~treat2_assigned,data=dta))$coefficients[2:7,4]

prim_soy_tc[12,22] <- nobs(lm(market_soy~treat2_assigned,data=dta))

#13 th outcome - crop used to pay for education

prim_maize[13,1] <- mean(dta$edu_maize[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize[13,2] <- sd(dta$edu_maize[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize_tc[13,1] <- mean(dta$edu_maize[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize_tc[13,2] <- sd(dta$edu_maize[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize[13,c(4,7,10,13,16,19)] <- summary(lm(edu_maize~treat2_assigned  +fe_vil,data=dta))$coefficients[2:7,1]
prim_maize[13,c(4,7,10,13,16,19)+1] <- summary(lm(edu_maize~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,2]
prim_maize[13,c(4,7,10,13,16,19)+2] <- summary(lm(edu_maize~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]

prim_maize[13,22] <- nobs(lm(edu_maize~treat2_assigned  + fe_vil,data=dta))

prim_maize_tc[13,c(4,7,10,13,16,19)] <- summary(lm(edu_maize~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_maize_tc[13,c(4,7,10,13,16,19)+1] <- summary(lm(edu_maize~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_maize_tc[13,c(4,7,10,13,16,19)+2] <- summary(lm(edu_maize~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_maize_tc[13,22] <- nobs(lm(edu_maize~treat2_assigned,data=dta))


prim_gnuts[13,1] <- mean(dta$edu_gnuts[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts[13,2] <- sd(dta$edu_gnuts[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts_tc[13,1] <- mean(dta$edu_gnuts[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts_tc[13,2] <- sd(dta$edu_gnuts[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts[13,c(4,7,10,13,16,19)] <- summary(lm(edu_gnuts~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_gnuts[13,c(4,7,10,13,16,19)+1] <- summary(lm(edu_gnuts~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_gnuts[13,c(4,7,10,13,16,19)+2] <- summary(lm(edu_gnuts~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]

prim_gnuts[13,22] <- nobs(lm(edu_gnuts~treat2_assigned  + fe_vil,data=dta))

prim_gnuts_tc[13,c(4,7,10,13,16,19)] <- summary(lm(edu_gnuts~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_gnuts_tc[13,c(4,7,10,13,16,19)+1] <- summary(lm(edu_gnuts~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_gnuts_tc[13,c(4,7,10,13,16,19)+2] <- summary(lm(edu_gnuts~treat2_assigned,data=dta))$coefficients[2:7,4]

prim_gnuts_tc[13,22] <- nobs(lm(edu_gnuts~treat2_assigned,data=dta))


prim_soy[13,1] <- mean(dta$edu_soy[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy[13,2] <- sd(dta$edu_soy[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy_tc[13,1] <- mean(dta$edu_soy[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy_tc[13,2] <- sd(dta$edu_soy[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy[13,c(4,7,10,13,16,19)] <- summary(lm(edu_soy~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_soy[13,c(4,7,10,13,16,19)+1] <- summary(lm(edu_soy~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_soy[13,c(4,7,10,13,16,19)+2] <- summary(lm(edu_soy~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]
prim_soy[13,22] <- nobs(lm(edu_soy~treat2_assigned  + fe_vil,data=dta))

prim_soy_tc[13,c(4,7,10,13,16,19)] <- summary(lm(edu_soy~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_soy_tc[13,c(4,7,10,13,16,19)+1] <- summary(lm(edu_soy~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_soy_tc[13,c(4,7,10,13,16,19)+2] <- summary(lm(edu_soy~treat2_assigned,data=dta))$coefficients[2:7,4]
prim_soy_tc[13,22] <- nobs(lm(edu_soy~treat2_assigned,data=dta))

#14 th outcome - crop used to pay for health

prim_maize[14,1] <- mean(dta$health_maize[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize[14,2] <- sd(dta$health_maize[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize_tc[14,1] <- mean(dta$health_maize[dta$treat2_assigned=="CC"], na.rm=T)
prim_maize_tc[14,2] <- sd(dta$health_maize[dta$treat2_assigned=="CC"], na.rm=T)

prim_maize[14,c(4,7,10,13,16,19)] <- summary(lm(health_maize~treat2_assigned  +fe_vil,data=dta))$coefficients[2:7,1]
prim_maize[14,c(4,7,10,13,16,19)+1] <- summary(lm(health_maize~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,2]
prim_maize[14,c(4,7,10,13,16,19)+2] <- summary(lm(health_maize~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]

prim_maize[14,22] <- nobs(lm(health_maize~treat2_assigned  + fe_vil,data=dta))

prim_maize_tc[14,c(4,7,10,13,16,19)] <- summary(lm(health_maize~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_maize_tc[14,c(4,7,10,13,16,19)+1] <- summary(lm(health_maize~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_maize_tc[14,c(4,7,10,13,16,19)+2] <- summary(lm(health_maize~treat2_assigned,data=dta))$coefficients[2:7,4]

prim_maize_tc[14,22] <- nobs(lm(health_maize~treat2_assigned,data=dta))


prim_gnuts[14,1] <- mean(dta$health_gnuts[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts[14,2] <- sd(dta$health_gnuts[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts_tc[14,1] <- mean(dta$health_gnuts[dta$treat2_assigned=="CC"], na.rm=T)
prim_gnuts_tc[14,2] <- sd(dta$health_gnuts[dta$treat2_assigned=="CC"], na.rm=T)

prim_gnuts[14,c(4,7,10,13,16,19)] <- summary(lm(health_gnuts~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_gnuts[14,c(4,7,10,13,16,19)+1] <- summary(lm(health_gnuts~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_gnuts[14,c(4,7,10,13,16,19)+2] <- summary(lm(health_gnuts~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]
prim_gnuts[14,22] <- nobs(lm(health_gnuts~treat2_assigned  + fe_vil,data=dta))

prim_gnuts_tc[14,c(4,7,10,13,16,19)] <- summary(lm(health_gnuts~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_gnuts_tc[14,c(4,7,10,13,16,19)+1] <- summary(lm(health_gnuts~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_gnuts_tc[14,c(4,7,10,13,16,19)+2] <- summary(lm(health_gnuts~treat2_assigned,data=dta))$coefficients[2:7,4]

prim_gnuts_tc[14,22] <- nobs(lm(health_gnuts~treat2_assigned,data=dta))


prim_soy[14,1] <- mean(dta$health_soy[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy[14,2] <- sd(dta$health_soy[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy_tc[14,1] <- mean(dta$health_soy[dta$treat2_assigned=="CC"], na.rm=T)
prim_soy_tc[14,2] <- sd(dta$health_soy[dta$treat2_assigned=="CC"], na.rm=T)

prim_soy[14,c(4,7,10,13,16,19)] <- summary(lm(health_soy~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,1]
prim_soy[14,c(4,7,10,13,16,19)+1] <- summary(lm(health_soy~treat2_assigned + fe_vil,data=dta))$coefficients[2:7,2]
prim_soy[14,c(4,7,10,13,16,19)+2] <- summary(lm(health_soy~treat2_assigned  + fe_vil,data=dta))$coefficients[2:7,4]
prim_soy[14,22] <- nobs(lm(health_soy~treat2_assigned  + fe_vil,data=dta))
prim_soy_tc[14,c(4,7,10,13,16,19)] <- summary(lm(health_soy~treat2_assigned,data=dta))$coefficients[2:7,1]
prim_soy_tc[14,c(4,7,10,13,16,19)+1] <- summary(lm(health_soy~treat2_assigned,data=dta))$coefficients[2:7,2]
prim_soy_tc[14,c(4,7,10,13,16,19)+2] <- summary(lm(health_soy~treat2_assigned,data=dta))$coefficients[2:7,4]

prim_soy_tc[14,22] <- nobs(lm(health_soy~treat2_assigned,data=dta))


prim_maize <-round(prim_maize,digits=3)
prim_gnuts <-round(prim_gnuts,digits=3)
prim_soy <-round(prim_soy,digits=3)

prim_maize_tc <-round(prim_maize_tc,digits=3)
prim_gnuts_tc <-round(prim_gnuts_tc,digits=3)
prim_soy_tc <-round(prim_soy_tc,digits=3)


saveRDS(prim_maize, paste(path,"endline_2024/results/prim_maize.RData", sep="/"))
saveRDS(prim_gnuts, paste(path,"endline_2024/results/prim_gnuts.RData", sep="/"))
saveRDS(prim_soy, paste(path,"endline_2024/results/prim_soy.RData", sep="/"))

saveRDS(prim_maize_tc, paste(path,"endline_2024/results/prim_maize_tc.RData", sep="/"))
saveRDS(prim_gnuts_tc, paste(path,"endline_2024/results/prim_gnuts_tc.RData", sep="/"))
saveRDS(prim_soy_tc, paste(path,"endline_2024/results/prim_soy_tc.RData", sep="/"))

###write data for merging with midline 2
write.csv(dta,paste(path,"all_rounds/data/endline_2024.csv", sep="/"))
# 
# 
# #### control for FWER using simulation
# 
# library(randomizr)
# 
# #we need to create an ID within block for merging
# #do this in a loop
# dta$id_in_blocks <- NA
# for (i in levels(dta$fe_vil)) {
#   dta$id_in_blocks[dta$fe_vil==i] <- 1:length(dta$id_in_blocks[dta$fe_vil==i])
# 
# }
# to_drop <- "treatment"
# dta <- dta[ , !(names(dta) %in% to_drop)]
# 
# crit_val <- 0.05
# 
# blocks <- rep(1:114, times = 31)
# id_in_blocks <- rep(1:31, times = 114)
# block_m_each <- matrix(NA,114,3)
# block_m_each[,1] <- 13
# block_m_each[,2] <- 9 
# block_m_each[,3] <- 9 
# f_s <- c("ihs(stock_maize_abs)~treatment + hhsize+ironroof+tot_acre+hired_labour+ fe_vil","stock_maize_pct~treatment + hhsize+ironroof+tot_acre+hired_labour+ fe_vil","sold_maize~treatment + sold_maize_b +fe_vil" ,"ihs(sold_maize_kg)~treatment + sold_maize_kg_b +fe_vil","sold_maize_pct~treatment + sold_maize_pct_b +fe_vil","price_maize~treatment + price_maize_b + hhsize+ironroof+tot_acre+hired_labour+ fe_vil" , "ihs(stock_gnuts_abs)~treatment + hhsize+ironroof+tot_acre+hired_labour+ fe_vil","stock_gnuts_pct~treatment + hhsize+ironroof+tot_acre+hired_labour+ fe_vil","sold_gnuts~treatment + sold_gnuts_b +fe_vil" ,"ihs(sold_gnuts_kg)~treatment + sold_gnuts_kg_b +fe_vil","sold_gnuts_pct~treatment + sold_gnuts_pct_b +fe_vil","price_gnuts~treatment + price_gnuts_b + hhsize+ironroof+tot_acre+hired_labour+ fe_vil", "ihs(stock_soy_abs)~treatment + hhsize+ironroof+tot_acre+hired_labour+ fe_vil","stock_soy_pct~treatment + hhsize+ironroof+tot_acre+hired_labour+ fe_vil","sold_soy~treatment + sold_gnuts_b +fe_vil" ,"ihs(sold_soy_kg)~treatment + sold_soy_kg_b +fe_vil","sold_soy_pct~treatment + sold_soy_pct_b +fe_vil","price_soy~treatment + price_soy_b + hhsize+ironroof+tot_acre+hired_labour+ fe_vil")
# 
# # Helper functions
# do_reg_T1 <- function(data_set, f_s){
#   summary(lm(as.formula(f_s),data=data_set))$coefficients[2,4]
# }
# 
# do_reg_T2 <- function(data_set, f_s){
#   summary(lm(as.formula(f_s),data=data_set))$coefficients[3,4]
# }
# 
# permute_treat <- function(dta_fun, f_s){
#   dta_fun$blocks <- as.numeric(dta_fun$fe_vil)
#   assignment <-    data.frame(blocks,id_in_blocks,block_ra(blocks = blocks, block_m_each = block_m_each,conditions = c("control", "T1", "T2")))
#   names(assignment) <- c("blocks","id_in_blocks", "treatment")
#   dta_sim <- merge(dta_fun, assignment, by.x=c("blocks", "id_in_blocks"), by.y=c("blocks", "id_in_blocks"))
#     
#   ps_sim <- matrix(NA,1,2*length(f_s))               
#   for (i in 1:length(f_s)) {            
#   ps_sim[i] <- do_reg_T1(dta_sim, f_s[i])
#    ps_sim[i+(length(f_s))] <- do_reg_T2(dta_sim, f_s[i])
#   }
#   return(ps_sim)
# }
# threshold_finder<- function(threshold){
#   mean(apply(many_ps, 2, x <- function(x) sum(x <= threshold) > 0 ))
# }
# startTime <- Sys.time()
# #we can get the p_obs from the prim_ matrices
# p_obs <- c(prim_maize[1:6,6],prim_gnuts[1:6,6],prim_soy[1:6,6],prim_maize[1:6,9],prim_gnuts[1:6,9],prim_soy[1:6,9])
# # Simulate under the sharp null
# many_ps <- replicate(1000, permute_treat(dta,f_s), simplify = TRUE)
# # Obtain the Type I error rate for a series of thresholds
# thresholds <- seq(0, crit_val, length.out = 1000)
# type_I_rate <- sapply(thresholds, threshold_finder)
# # Find the largest threshold that yields an alpha type I error rate
# target_p_value <- thresholds[max(which(type_I_rate <=crit_val))]
# # Apply target p_value to observed p_values
# sig_simulated <- p_obs <= target_p_value
# # Compare to raw p-values
# sig <- p_obs <= crit_val
# 
# endTime <- Sys.time()
# 
# # prints recorded time
# print(endTime - startTime)
# 
# print("the end")




