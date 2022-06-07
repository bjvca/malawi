
#run in malawi/RCT/mock_report
library(nnet)
library(lmtest)



path <- getwd()
path <- strsplit(path, "/mock_report")[[1]]

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





###prepare matrix that can be easily used 
res_bal <- matrix(NA,10,10)
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

nullMod <- multinom(treatment ~1, data=na.omit(dta[ , all.vars(formula(treatment~femhead+hhsize+agehead+eduhead+ironroof+nr_rooms+tot_acre+hired_labour+distance_road+distance_market))]))
altMod <- multinom(treatment~femhead+hhsize+agehead+eduhead+ironroof+nr_rooms+tot_acre+hired_labour+distance_road+distance_market, data = dta)
lrtest(altMod, nullMod)

#What if we use F-tests
x <-  summary(lm((treatment=="T1")~femhead+hhsize+agehead+eduhead+ironroof+nr_rooms+tot_acre+hired_labour+distance_road+distance_market, data = dta[dta$treatment%in%c("T1","C"),]))

f_test_p_T1 <-  pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
 
x <-  summary(lm((treatment=="T2")~femhead+hhsize+agehead+eduhead+ironroof+nr_rooms+tot_acre+hired_labour+distance_road+distance_market, data = dta[dta$treatment%in%c("T2","C"),]))

f_test_p_T2 <-  pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)


saveRDS(res_bal, paste(path,"mock_report/results/balace.RData", sep="/"))







