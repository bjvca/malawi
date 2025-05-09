##### Balance tables for the malawi study ####################
#run in malawi/RCT/mock_report/analysis
library(nnet)
library(lmtest)
library(car)

path <- getwd()
path <- strsplit(path, "/mock_report/analysis")[[1]]

dta <- read.csv(paste(path,"baseline/data/public/baseline_data.csv", sep="/"))

###create unique dist_ta_vil variable to use for fixed effects
dta$fe_vil <- as.factor(paste(paste(dta$distID, dta$taID, sep="_"),dta$vilID, sep ="_"))

dta$treatment[dta$treatment=="n/a"] <- NA

dta$femhead <-dta$q13

dta$femhead[dta$q12!="1"] <-dta$q16[dta$q12!="1"]
dta$femhead[dta$femhead=="n/a"] <- NA
dta$femhead <- dta$femhead =="Female"

dta$hhsize <- as.numeric(as.character(dta$q19))

dta$agehead <-as.numeric(as.character(dta$q14))
dta$agehead[dta$q12!="1"] <-as.numeric(as.character(dta$q17[dta$q12!="1"]))
dta$agehead[dta$agehead==999] <- NA

dta$eduhead <-as.numeric(as.character(dta$q15))
dta$eduhead[dta$q12!="1"] <-as.numeric(as.character(dta$q18[dta$q12!="1"]))
dta$eduhead[dta$eduhead==999] <- NA
dta$eduhead[dta$eduhead>20] <- NA

dta$ironroof <- as.numeric(as.character(dta$q20)) == 2
dta$nr_rooms <- as.numeric(as.character(dta$q21))

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

dta$distance_road <- as.numeric(as.character(dta$q22))
dta$distance_road[dta$distance_road==999] <- NA
dta$distance_market <- as.numeric(as.character(dta$q23))
dta$distance_market[dta$distance_market==999] <- NA

basevars <- c("femhead","hhsize","agehead","eduhead","ironroof","nr_rooms","tot_acre","hired_labour","distance_road","distance_market")

allModelsList <- lapply(paste(basevars,"treatment + fe_vil", sep="~"), as.formula)
allModelsResults <- lapply(allModelsList, function(x) lm(x, data = dta)) 
allModelsSummaries = lapply(allModelsResults, summary) 

##some checks

#summary(lm(tot_acre~treatment, data=dta))

#summary(lm(tot_acre~treatment=="T1", data=dta[dta$treatment%in%c("C","T1"),]))
#summary(lm(tot_acre~treatment=="T2", data=dta[dta$treatment%in%c("C","T2"),]))



###prepare matrix that can be easily used 
res_bal <- matrix(NA,13,10)
### ctrl mean
res_bal[1:10,1] <- round(colMeans(dta[dta$treatment=="C",basevars], na.rm=T), digits=3)

res_bal[1:10,2] <- round(apply(dta[dta$treatment=="C",basevars], 2, sd, na.rm=T), digits=3)


for (i in 1:10) {


res_bal[i,4] <- round(allModelsSummaries[[i]]$coefficients[2,1],digits=3)
res_bal[i,5] <- round(allModelsSummaries[[i]]$coefficients[2,2],digits=3)
res_bal[i,6] <- round(allModelsSummaries[[i]]$coefficients[2,4],digits=3)

res_bal[i,7] <- round(allModelsSummaries[[i]]$coefficients[3,1],digits=3)
res_bal[i,8] <- round(allModelsSummaries[[i]]$coefficients[3,2],digits=3)
res_bal[i,9] <- round(allModelsSummaries[[i]]$coefficients[3,4],digits=3)

res_bal[i,10] <- nrow(model.frame(allModelsResults[[i]]))

}

##joint orthogonality

### do this with a multinomial model and a Likelihood ration test : -2*L(null model) – (-2*L(fitted model)) = 365.736 – 332.641 = 33.095

nullMod <- multinom(treatment ~1 + fe_vil, data=na.omit(dta[ , all.vars(formula(treatment~femhead+hhsize+agehead+eduhead+ironroof+nr_rooms+tot_acre+hired_labour+distance_road+distance_market + fe_vil))]))
altMod <- multinom(treatment~femhead+hhsize+agehead+eduhead+ironroof+nr_rooms+tot_acre+hired_labour+distance_road+distance_market + fe_vil, data = dta)
lrtest(altMod, nullMod)

res_bal[13,1] <-round(lrtest(altMod, nullMod)[2,4],digits=3)
res_bal[13,2] <-round(lrtest(altMod, nullMod)[2,5],digits=3)

##simple F-tests
 
mod1<- lm((treatment=="T1")~femhead+hhsize+agehead+eduhead+ironroof+nr_rooms+tot_acre+hired_labour+distance_road+distance_market + fe_vil, data = dta[dta$treatment%in%c("T1","C"),])
test_res <- linearHypothesis(mod1, c("femheadTRUE=0","hhsize=0", "agehead=0","eduhead=0","ironroofTRUE=0","nr_rooms=0","tot_acre=0","hired_labourTRUE=0","distance_road=0","distance_market=0"))


res_bal[11,1] <-round(test_res[2,5],digits=3)
res_bal[11,2] <-round(test_res[2,6],digits=3)
 
mod1<- lm((treatment=="T2")~femhead+hhsize+agehead+eduhead+ironroof+nr_rooms+tot_acre+hired_labour+distance_road+distance_market + fe_vil, data = dta[dta$treatment%in%c("T2","C"),])
test_res <- linearHypothesis(mod1, c("femheadTRUE=0","hhsize=0", "agehead=0","eduhead=0","ironroofTRUE=0","nr_rooms=0","tot_acre=0","hired_labourTRUE=0","distance_road=0","distance_market=0"))


res_bal[12,1] <-round(test_res[2,5],digits=3)
res_bal[12,2] <-round(test_res[2,6],digits=3)

saveRDS(res_bal, paste(path,"mock_report/results/balace.RData", sep="/"))

## we also promis balance table for variables that will be used for treatment heterogeneity
##access to credit, access to storage facility, membership of (marketing related) cooperative, livestock asset ownership, whether the household already makes a budget. 
dta$credit_access <- dta$exp.q73=="Yes" 
dta$storage_access <- dta$exp.q75=="Yes"
dta$coop_member <- dta$exp.q77=="Yes" 
dta$budget <- dta$exp.q71=="Yes" 

dta$livestock_index <- dta$livestock.q38a + 0.75*dta$livestock.q38b + 0.25*dta$livestock.q38c + 0.2*dta$livestock.q38d + 0.13*(dta$livestock.q38e + dta$livestock.q38f) + 0.013*(dta$livestock.q38g + dta$livestock.q38h)
dta$livestock_index[dta$livestock_index> 90] <- NA 
dta$has_livestock <- (dta$livestock_index > .13)


basevars <- c("credit_access","storage_access","coop_member","has_livestock","budget")

allModelsList <- lapply(paste(basevars,"treatment + fe_vil", sep="~"), as.formula)
allModelsResults <- lapply(allModelsList, function(x) lm(x, data = dta)) 
allModelsSummaries = lapply(allModelsResults, summary) 

##prepare matrix that can be easily used 
het_bal <- matrix(NA,13,10)
### ctrl mean
het_bal[1:5,1] <- round(colMeans(dta[dta$treatment=="C",basevars], na.rm=T), digits=3)

het_bal[1:5,2] <- round(apply(dta[dta$treatment=="C",basevars], 2, sd, na.rm=T), digits=3)


for (i in 1:5) {
  
  
  het_bal[i,4] <- round(allModelsSummaries[[i]]$coefficients[2,1],digits=3)
  het_bal[i,5] <- round(allModelsSummaries[[i]]$coefficients[2,2],digits=3)
  het_bal[i,6] <- round(allModelsSummaries[[i]]$coefficients[2,4],digits=3)
  
  het_bal[i,7] <- round(allModelsSummaries[[i]]$coefficients[3,1],digits=3)
  het_bal[i,8] <- round(allModelsSummaries[[i]]$coefficients[3,2],digits=3)
  het_bal[i,9] <- round(allModelsSummaries[[i]]$coefficients[3,4],digits=3)
  
  het_bal[i,10] <- nrow(model.frame(allModelsResults[[i]]))
  
}

##joint orthogonality

### do this with a multinomial model and a Likelihood ration test : -2*L(null model) – (-2*L(fitted model)) = 365.736 – 332.641 = 33.095

nullMod <- multinom(treatment ~1 + fe_vil, data=na.omit(dta[ , all.vars(formula(treatment~credit_access+storage_access+coop_member+has_livestock+budget + fe_vil))]))
altMod <- multinom(treatment~credit_access+storage_access+coop_member+has_livestock+budget + fe_vil, data = dta)
lrtest(altMod, nullMod)

het_bal[13,1] <-round(lrtest(altMod, nullMod)[2,4],digits=3)
het_bal[13,2] <-round(lrtest(altMod, nullMod)[2,5],digits=3)

##simple F-tests

mod1<- lm((treatment=="T1")~credit_access+storage_access+coop_member+has_livestock+budget + fe_vil, data = dta[dta$treatment%in%c("T1","C"),])
test_res <- linearHypothesis(mod1, c("credit_accessTRUE=0","storage_accessTRUE=0","coop_memberTRUE=0","has_livestockTRUE=0","budgetTRUE=0"))


het_bal[11,1] <-round(test_res[2,5],digits=3)
het_bal[11,2] <-round(test_res[2,6],digits=3)

mod1<- lm((treatment=="T2")~credit_access+storage_access+coop_member+has_livestock+budget + fe_vil, data = dta[dta$treatment%in%c("T2","C"),])
test_res <- linearHypothesis(mod1, c("credit_accessTRUE=0","storage_accessTRUE=0","coop_memberTRUE=0","has_livestockTRUE=0","budgetTRUE=0"))


het_bal[12,1] <-round(test_res[2,5],digits=3)
het_bal[12,2] <-round(test_res[2,6],digits=3)

saveRDS(het_bal, paste(path,"mock_report/results/balace_het.RData", sep="/"))

