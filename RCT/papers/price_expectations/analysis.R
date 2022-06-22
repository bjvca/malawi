#run in papers/price_expectations

library(reshape2)
library(ggplot2)
library(ggpubr)
library(multiwayvcov)
library(clubSandwich)
library(car)

path <- getwd()
path <- strsplit(path, "papers/price_expectations")[[1]]

dta <- read.csv(paste(path,"baseline/data/public/baseline_data.csv", sep="/"))
dta[dta=="n/a"] <- NA
dta$fe_vil <- as.factor(paste(paste(dta$distID, dta$taID, sep="_"),dta$vilID, sep ="_"))

### create a graph of price expectations


### create a graph of prices received in previous years
prices <- c("q24","q25","q26","q27","q28","q29","q30","q31","q32","q32a")
dta[prices] <- lapply(dta[prices],  function(x) as.numeric(as.character(x)))
dta$q30[dta$q30 > 30000] <- NA
dta$q31[dta$q31 > 30000] <- NA

dta$q30[dta$q30 == 0] <- NA
dta$q31[dta$q31 == 0] <- NA

temp_dta <- dta[c("q24","q25")]
names(temp_dta) <-c("September","December")
temp_dta <- melt(temp_dta)
names(temp_dta) <- c("time", "price")

graph <- temp_dta
graph$crop <- "maize"

temp_dta <- dta[c("q27","q28")]
names(temp_dta) <-c("September","December")
temp_dta <- melt(temp_dta)
names(temp_dta) <- c("time", "price")
temp_dta$crop <- "soybean"

graph <- rbind(graph,temp_dta)

temp_dta <- dta[c("q30","q31")]
names(temp_dta) <-c("September","December")
temp_dta <- melt(temp_dta)
names(temp_dta) <- c("time", "price")
temp_dta$crop <- "groundnuts"

graph <- rbind(graph,temp_dta)
graph$price[graph$crop=="groundnuts"] <- graph$price[graph$crop=="groundnuts"]/20


png(paste(path,"papers/price_expectations/results/fig1.png",sep = ""), units="px", height=3200, width= 3800, res=600)
ggplot(graph, aes(crop, price,fill=time)) + 
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0,1500))
  dev.off()
  
### create a time series graph with average prices 
### first stack data of different transactions
### start with maize  
sel <- c( "trans.1..q43b", "trans.1..q43d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.1..q43a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans1 <- data.frame(dta$farmer_ID,dta$trans.1..q43a,dta$trans.1..q43b,dta$trans.1..q43d)
names(trans1) <- c("farmer_ID", "date","form","price")

sel <- c( "trans.2..q43b", "trans.2..q43d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.2..q43a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans2 <- data.frame(dta$farmer_ID,dta$trans.2..q43a,dta$trans.2..q43b,dta$trans.2..q43d) 
names(trans2) <- c("farmer_ID", "date","form","price")

sel <- c( "trans.3..q43b", "trans.3..q43d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.3..q43a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans3 <- data.frame(dta$farmer_ID,dta$trans.3..q43a,dta$trans.3..q43b,dta$trans.3..q43d) 
names(trans3) <- c("farmer_ID", "date","form","price")

sel <- c( "trans.4..q43b", "trans.4..q43d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.4..q43a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans4 <- data.frame(dta$farmer_ID,dta$trans.4..q43a,dta$trans.4..q43b,dta$trans.4..q43d) 
names(trans4) <- c("farmer_ID", "date","form","price")

sel <- c( "trans.5..q43b", "trans.5..q43d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.5..q43a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans5 <- data.frame(dta$farmer_ID,dta$trans.5..q43a,dta$trans.5..q43b,dta$trans.5..q43d) 
names(trans5) <- c("farmer_ID", "date","form","price")

sel <- c( "trans.6..q43b", "trans.6..q43d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.6..q43a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans6 <- data.frame(dta$farmer_ID,dta$trans.6..q43a,dta$trans.6..q43b,dta$trans.6..q43d) 
names(trans6) <- c("farmer_ID", "date","form","price")

sel <- c( "trans.7..q43b", "trans.7..q43d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.7..q43a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans7 <- data.frame(dta$farmer_ID,dta$trans.7..q43a,dta$trans.7..q43b,dta$trans.7..q43d) 
names(trans7) <- c("farmer_ID", "date","form","price")

sel <- c( "trans.8..q43b", "trans.8..q43d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.8..q43a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans8 <- data.frame(dta$farmer_ID,dta$trans.8..q43a,dta$trans.8..q43b,dta$trans.8..q43d) 
names(trans8) <- c("farmer_ID", "date","form","price")

all <- rbind(trans1, trans2, trans3, trans4, trans5, trans6, trans7,trans8)
names(all) <-  c("farmer_ID", "date","form","price")
all <- subset(all, form ==2)
all$price[all$price > 999] <- NA
all$price[all$price < 25] <- NA
to_plot_maize <-data.frame(tapply(all$price, all$date, FUN=median, na.rm=T)[6:16])
to_plot_maize$date <- rownames(to_plot_maize)
names(to_plot_maize) <- c("price","date")

all_maize <- all

#now for gnuts

sel <- c( "trans1.1..q47b", "trans1.1..q47d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans1.1..q47a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans11 <- data.frame(dta$farmer_ID,dta$trans1.1..q47a,dta$trans1.1..q47b,dta$trans1.1..q47d)
names(trans11) <- c("farmer_ID", "date","form","price")

sel <- c( "trans1.2..q47b", "trans1.2..q47d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans1.2..q47a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans12 <- data.frame(dta$farmer_ID,dta$trans1.2..q47a,dta$trans1.2..q47b,dta$trans1.2..q47d) 
names(trans12) <- c("farmer_ID", "date","form","price")

sel <- c( "trans1.3..q47b", "trans1.3..q47d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans1.3..q47a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans13 <- data.frame(dta$farmer_ID,dta$trans1.3..q47a,dta$trans1.3..q47b,dta$trans1.3..q47d) 
names(trans13) <- c("farmer_ID", "date","form","price")

sel <- c( "trans1.4..q47b", "trans1.4..q47d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans1.4..q47a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans14 <- data.frame(dta$farmer_ID,dta$trans1.4..q47a,dta$trans1.4..q47b,dta$trans1.4..q47d) 
names(trans14) <- c("farmer_ID", "date","form","price")

sel <- c( "trans1.5..q47b", "trans1.5..q47d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans1.5..q47a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans15 <- data.frame(dta$farmer_ID,dta$trans1.5..q47a,dta$trans1.5..q47b,dta$trans1.5..q47d) 
names(trans15) <- c("farmer_ID", "date","form","price")

all <- rbind(trans11, trans12, trans13, trans14, trans15)
names(all) <-  c("farmer_ID", "date","form","price")
all <- subset(all, form ==2)
all$price[all$price > 19999] <- NA
all$price[all$price < 999] <- NA
all$price <- all$price/20 
to_plot_gnuts <-data.frame(tapply(all$price, all$date, FUN=median, na.rm=T)[4:14])
to_plot_gnuts$date <- rownames(to_plot_gnuts)
names(to_plot_gnuts) <- c("price","date")

all_gnuts <- all

### for soybean

sel <- c( "trans2.1..q52b", "trans2.1..q52d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans2.1..q52a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans21 <- data.frame(dta$farmer_ID,dta$trans2.1..q52a,dta$trans2.1..q52b,dta$trans2.1..q52d)
names(trans21) <- c("farmer_ID", "date","form","price")

sel <- c( "trans2.2..q52b", "trans2.2..q52d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans2.2..q52a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans22 <- data.frame(dta$farmer_ID,dta$trans2.2..q52a,dta$trans2.2..q52b,dta$trans2.2..q52d) 
names(trans22) <- c("farmer_ID", "date","form","price")

sel <- c( "trans2.3..q52b", "trans2.3..q52d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans2.3..q52a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans23 <- data.frame(dta$farmer_ID,dta$trans2.3..q52a,dta$trans2.3..q52b,dta$trans2.3..q52d) 
names(trans23) <- c("farmer_ID", "date","form","price")

sel <- c( "trans2.4..q52b", "trans2.4..q52d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans2.4..q52a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans24 <- data.frame(dta$farmer_ID,dta$trans2.4..q52a,dta$trans2.4..q52b,dta$trans2.4..q52d) 
names(trans24) <- c("farmer_ID", "date","form","price")

all <- rbind(trans21, trans22, trans23, trans24)
names(all) <-  c("farmer_ID", "date","form","price")
all <- subset(all, form ==1)
all$price[all$price > 1999] <- NA
all$price[all$price < 99] <- NA
to_plot_soy <-data.frame(tapply(all$price, all$date, FUN=median, na.rm=T)[5:15])
to_plot_soy$date <- rownames(to_plot_soy)
names(to_plot_soy) <- c("price","date")

all_soy <- all

to_plot_soy_b <- to_plot_soy
to_plot_maize_b <- to_plot_maize
to_plot_gnuts_b <- to_plot_gnuts

to_plot_soy$price <- to_plot_soy$price / 4

to_plot_soy$crop <- "soy"
to_plot_maize$crop <- "maize"
to_plot_gnuts$crop <- "gnuts"
all_1 <- rbind(to_plot_soy,to_plot_maize,to_plot_gnuts) 
names(all_1) <- c("price","month","crop")
plot_1 <- ggplot(all_1 ,aes(x=as.Date(month),y=price,colour=crop,group=crop)) + geom_line(size=1.2) + scale_x_date(date_labels = "%b") 

to_plot_soy <- to_plot_soy_b
to_plot_maize <- to_plot_maize_b
to_plot_gnuts <- to_plot_gnuts_b


to_plot_soy$price <- to_plot_soy$price / to_plot_soy$price[1]*100  
to_plot_maize$price <- to_plot_maize$price / to_plot_maize$price[1]*100  
to_plot_gnuts$price <- to_plot_gnuts$price / to_plot_gnuts$price[1]*100  


to_plot_soy$crop <- "soy"
to_plot_maize$crop <- "maize"
to_plot_gnuts$crop <- "gnuts"
all_2 <- rbind(to_plot_soy,to_plot_maize,to_plot_gnuts) 

names(all_2) <- c("price_change","month","crop")
plot_2 <- ggplot(all_2 ,aes(x=as.Date(month),y=price_change,colour=crop,group=crop)) + geom_line(size=1.2) + scale_x_date(date_labels = "%b")  + geom_hline(yintercept=100, linetype="dashed")

to_plot_soy <- to_plot_soy_b
to_plot_maize <- to_plot_maize_b
to_plot_gnuts <- to_plot_gnuts_b


to_plot_soy$price <- to_plot_soy$price / to_plot_soy$price[5]*100  
to_plot_maize$price <- to_plot_maize$price / to_plot_maize$price[5]*100  
to_plot_gnuts$price <- to_plot_gnuts$price / to_plot_gnuts$price[5]*100  


to_plot_soy$crop <- "soy"
to_plot_maize$crop <- "maize"
to_plot_gnuts$crop <- "gnuts"
all_3 <- rbind(to_plot_soy,to_plot_maize,to_plot_gnuts) 

names(all_3) <- c("price_change","month","crop")
plot_3 <- ggplot(all_3 ,aes(x=as.Date(month),y=price_change,colour=crop,group=crop)) + geom_line(size=1.2) + scale_x_date(date_labels = "%b")  + geom_hline(yintercept=100, linetype="dashed") + geom_vline(xintercept = as.numeric(as.Date(all_3$month[5])), linetype="dotted")+ geom_vline(xintercept = as.numeric(as.Date(all_3$month[9])), linetype="dotted")
 



png(paste(path,"papers/price_expectations/results/fig2.png",sep = ""), units="px", height=3200, width= 3800, res=600)
ggarrange(plot_1, plot_2,plot_3, heights = c(2, 2,2), ncol = 1, nrow = 3, align = "v")
dev.off()

all_soy$crop <- "soy"
all_maize$crop <- "maize"
all_gnuts$crop <- "gnuts"

all_transactions <- rbind(all_maize, all_soy, all_gnuts) 

### do farmers who sell at least once have higher price expectations?

#sold any maize

summary(lm((dta$q24)~q41=="Yes",data=dta))
summary(lm((dta$q25)~q41=="Yes",data=dta))
summary(lm((dta$q25 - dta$q24)~q41=="Yes",data=dta))

summary(lm((dta$q27)~q50=="Yes",data=dta))
summary(lm((dta$q28)~q50=="Yes",data=dta))
summary(lm((dta$q28 - dta$q27)~q50=="Yes",data=dta))

summary(lm((dta$q30)~q46=="Yes",data=dta))
summary(lm((dta$q31)~q46=="Yes",data=dta))
summary(lm((dta$q31 - dta$q30)~q46=="Yes",data=dta))

# if number of sales transactions > 1 how does that affect price expectations
as.numeric(as.character(dta$q42)>1)

summary(lm((dta$q24)~as.numeric(as.character(dta$q42)>1),data=dta))
summary(lm((dta$q25)~as.numeric(as.character(dta$q42)>1),data=dta))
summary(lm((dta$q25 - dta$q24)~as.numeric(as.character(dta$q42)>1),data=dta))


summary(lm((dta$q27)~as.numeric(as.character(dta$q51)>1),data=dta))
summary(lm((dta$q28)~as.numeric(as.character(dta$q51)>1),data=dta))
summary(lm((dta$q28 - dta$q27)~as.numeric(as.character(dta$q51)>1),data=dta))


summary(lm((dta$q30)~as.numeric(as.character(dta$q47)>1),data=dta))
summary(lm((dta$q31)~as.numeric(as.character(dta$q47)>1),data=dta))
summary(lm((dta$q31 - dta$q30)~as.numeric(as.character(dta$q47)>1),data=dta))

#does the timing of first sale affect price expectations (the earlier they sold, the lower price expectations)

# does average price affect expectations
#q: what price to use if more than one transaction?
#start with average price

## collect results in resmat
resmat <- matrix(NA,14,3)

inter <- aggregate(all_transactions[all_transactions$crop=="maize",]$price, list(all_transactions[all_transactions$crop=="maize",]$farmer_ID),mean, na.rm=T)
names(inter) <- c("farmer_ID","price")
inter <- merge(dta, inter, by="farmer_ID")
summary(lm(log(q24)~log(price),data=inter))
summary(lm(log(q25)~log(price),data=inter))

resmat[1,1] <- summary(lm(log((q24+q25)/2)~log(price),data=inter))$coefficients[2,1]
resmat[2,1] <- summary(lm(log((q24+q25)/2)~log(price),data=inter))$coefficients[2,2]
resmat[3,1] <- summary(lm(log((q24+q25)/2)~log(price),data=inter))$coefficients[2,4]

resmat[1,2] <- summary(lm(log(q24)~log(price),data=inter))$coefficients[2,1]
resmat[2,2] <- summary(lm(log(q24)~log(price),data=inter))$coefficients[2,2]
resmat[3,2] <- summary(lm(log(q24)~log(price),data=inter))$coefficients[2,4]

resmat[1,3] <- summary(lm(log(q25)~log(price),data=inter))$coefficients[2,1]
resmat[2,3] <- summary(lm(log(q25)~log(price),data=inter))$coefficients[2,2]
resmat[3,3] <- summary(lm(log(q25)~log(price),data=inter))$coefficients[2,4]

resmat[10,1] <- nobs(lm(log((q24+q25)/2)~log(price),data=inter))
summary(lm((q25-q24)~price,data=inter))

#gnuts
inter <- aggregate(all_transactions[all_transactions$crop=="gnuts",]$price, list(all_transactions[all_transactions$crop=="gnuts",]$farmer_ID),mean, na.rm=T)
names(inter) <- c("farmer_ID","price")
inter <- merge(dta, inter, by="farmer_ID")
summary(lm(log(q30)~log(price),data=inter))
summary(lm(log(q31)~log(price),data=inter))

summary(lm(log((q30+q31)/2)~log(price),data=inter))
resmat[4,1] <- summary(lm(log((q30+q31)/2)~log(price),data=inter))$coefficients[2,1]
resmat[5,1] <- summary(lm(log((q30+q31)/2)~log(price),data=inter))$coefficients[2,2]
resmat[6,1] <- summary(lm(log((q30+q31)/2)~log(price),data=inter))$coefficients[2,4]


resmat[4,2] <- summary(lm(log(q30)~log(price),data=inter))$coefficients[2,1]
resmat[5,2] <- summary(lm(log(q30)~log(price),data=inter))$coefficients[2,2]
resmat[6,2] <- summary(lm(log(q30)~log(price),data=inter))$coefficients[2,4]

resmat[4,3] <- summary(lm(log(q31)~log(price),data=inter))$coefficients[2,1]
resmat[5,3] <- summary(lm(log(q31)~log(price),data=inter))$coefficients[2,2]
resmat[6,3] <- summary(lm(log(q31)~log(price),data=inter))$coefficients[2,4]

resmat[10,2] <- nobs(lm(log((q31+q30)/2)~log(price),data=inter))
summary(lm((q31-q30)~price,data=inter))

##soy
inter <- aggregate(all_transactions[all_transactions$crop=="soy",]$price, list(all_transactions[all_transactions$crop=="soy",]$farmer_ID),mean, na.rm=T)
names(inter) <- c("farmer_ID","price")
inter <- merge(dta, inter, by="farmer_ID")
summary(lm(log(q27)~log(price),data=inter))
summary(lm(log(q28)~log(price),data=inter))

summary(lm(log((q27+q28)/2)~log(price),data=inter))
resmat[7,1] <- summary(lm(log((q27+q28)/2)~log(price),data=inter))$coefficients[2,1]
resmat[8,1] <- summary(lm(log((q27+q28)/2)~log(price),data=inter))$coefficients[2,2]
resmat[9,1] <- summary(lm(log((q27+q28)/2)~log(price),data=inter))$coefficients[2,4]

resmat[7,2] <- summary(lm(log(q27)~log(price),data=inter))$coefficients[2,1]
resmat[8,2] <- summary(lm(log(q27)~log(price),data=inter))$coefficients[2,2]
resmat[9,2] <- summary(lm(log(q27)~log(price),data=inter))$coefficients[2,4]

resmat[7,3] <- summary(lm(log(q28)~log(price),data=inter))$coefficients[2,1]
resmat[8,3] <- summary(lm(log(q28)~log(price),data=inter))$coefficients[2,2]
resmat[9,3] <- summary(lm(log(q28)~log(price),data=inter))$coefficients[2,4]

resmat[10,3] <- nobs(lm(log((q27+q28)/2)~log(price),data=inter))

#can we do a pooled regression as well?
inter <- aggregate(all_transactions[all_transactions$crop=="maize",]$price, list(all_transactions[all_transactions$crop=="maize",]$farmer_ID),mean, na.rm=T)
names(inter) <- c("farmer_ID","price")
inter_maize <- merge(dta, inter, by="farmer_ID")
inter_maize$price_sept <- inter_maize$q24
inter_maize$price_dec <- inter_maize$q25
inter_maize$crop <- "maize"

inter <- aggregate(all_transactions[all_transactions$crop=="gnuts",]$price, list(all_transactions[all_transactions$crop=="gnuts",]$farmer_ID),mean, na.rm=T)
names(inter) <- c("farmer_ID","price")
inter_gnuts <- merge(dta, inter, by="farmer_ID")
inter_gnuts$price_sept <- inter_gnuts$q30
inter_gnuts$price_dec <- inter_gnuts$q31
inter_gnuts$crop <- "gnuts"

inter <- aggregate(all_transactions[all_transactions$crop=="soy",]$price, list(all_transactions[all_transactions$crop=="soy",]$farmer_ID),mean, na.rm=T)
names(inter) <- c("farmer_ID","price")
inter_soy <- merge(dta, inter, by="farmer_ID")

inter_soy$price_sept <- inter_soy$q27
inter_soy$price_dec <- inter_soy$q28
inter_soy$crop <- "soy"

all <- rbind(inter_maize,inter_soy,inter_gnuts)

mod <- lm(log((price_sept+price_dec)/2)~log(price) + crop,data=all)
vcov_cluster <- vcovCR(mod, cluster=all$farmer_ID, type = "CR2")
res <- coef_test(mod, vcov = vcov_cluster)

resmat[14,1] <- nobs(mod)
resmat[11,1] <- res[2,2]
resmat[12,1] <- res[2,3]
resmat[13,1] <- res[2,6]


mod <- lm(log(price_sept)~log(price) + crop,data=all)
vcov_cluster <- vcovCR(mod, cluster=all$farmer_ID, type = "CR2")
res <- coef_test(mod, vcov = vcov_cluster)


resmat[11,2] <- res[2,2]
resmat[12,2] <- res[2,3]
resmat[13,2] <- res[2,6]


mod <- lm(log(price_dec)~log(price) + crop,data=all)
vcov_cluster <- vcovCR(mod, cluster=all$farmer_ID, type = "CR2")
res <- coef_test(mod, vcov = vcov_cluster)

resmat[11,3] <- res[2,2]
resmat[12,3] <- res[2,3]
resmat[13,3] <- res[2,6]

### increase over time
### restrict sample to farmers

#first define groups early, normal, late

all_transactions$early <- as.numeric(as.Date(all_transactions$date)) %in% c(18748,18779,18809)
all_transactions$average <- as.numeric(as.Date(all_transactions$date)) %in% c(18840, 18871, 18901)
all_transactions$late <- as.numeric(as.Date(all_transactions$date)) %in% c(18932,18962,18993)

#then calculate average prices per farmer - maize
maize_early <- aggregate(all_transactions$price[all_transactions$early & all_transactions$crop=="maize"], list(all_transactions$farmer_ID[all_transactions$early & all_transactions$crop=="maize"]),mean,na.rm=T)
names(maize_early) <- c("farmer_ID","price_early")
maize_average <- aggregate(all_transactions$price[all_transactions$average & all_transactions$crop=="maize"], list(all_transactions$farmer_ID[all_transactions$average & all_transactions$crop=="maize"]),mean,na.rm=T)
names(maize_average) <- c("farmer_ID","price_average")
maize_late <- aggregate(all_transactions$price[all_transactions$late & all_transactions$crop=="maize"], list(all_transactions$farmer_ID[all_transactions$late & all_transactions$crop=="maize"]),mean,na.rm=T)
names(maize_late) <- c("farmer_ID","price_late")

diff_maize <- merge(merge(maize_average, maize_late, by="farmer_ID", all=T), maize_early, by="farmer_ID", all=T)

#then calculate average prices per farmer - gnuts
gnuts_early <- aggregate(all_transactions$price[all_transactions$early & all_transactions$crop=="gnuts"], list(all_transactions$farmer_ID[all_transactions$early & all_transactions$crop=="gnuts"]),mean,na.rm=T)
names(gnuts_early) <- c("farmer_ID","price_early")
gnuts_average <- aggregate(all_transactions$price[all_transactions$average & all_transactions$crop=="gnuts"], list(all_transactions$farmer_ID[all_transactions$average & all_transactions$crop=="gnuts"]),mean,na.rm=T)
names(gnuts_average) <- c("farmer_ID","price_average")
gnuts_late <- aggregate(all_transactions$price[all_transactions$late & all_transactions$crop=="gnuts"], list(all_transactions$farmer_ID[all_transactions$late & all_transactions$crop=="gnuts"]),mean,na.rm=T)
names(gnuts_late) <- c("farmer_ID","price_late")

diff_gnuts <- merge(merge(gnuts_average, gnuts_late, by="farmer_ID", all=T), gnuts_early, by="farmer_ID", all=T)

#then calculate average prices per farmer - soy
soy_early <- aggregate(all_transactions$price[all_transactions$early & all_transactions$crop=="soy"], list(all_transactions$farmer_ID[all_transactions$early & all_transactions$crop=="soy"]),mean,na.rm=T)
names(soy_early) <- c("farmer_ID","price_early")
soy_average <- aggregate(all_transactions$price[all_transactions$average & all_transactions$crop=="soy"], list(all_transactions$farmer_ID[all_transactions$average & all_transactions$crop=="soy"]),mean,na.rm=T)
names(soy_average) <- c("farmer_ID","price_average")
soy_late <- aggregate(all_transactions$price[all_transactions$late & all_transactions$crop=="soy"], list(all_transactions$farmer_ID[all_transactions$late & all_transactions$crop=="soy"]),mean,na.rm=T)
names(soy_late) <- c("farmer_ID","price_late")

diff_soy <- merge(merge(soy_average, soy_late, by="farmer_ID", all=T), soy_early, by="farmer_ID", all=T)

dyn_maize <- merge(dta,diff_maize, by = "farmer_ID",all=T)
dyn_gnuts <- merge(dta,diff_gnuts, by = "farmer_ID",all=T)
dyn_soy <- merge(dta,diff_soy, by = "farmer_ID",all=T)

## collect results in resmat
resmat_fe <- matrix(NA,12,4)


###this is a first difference model
#maize
resmat_fe[1,2] <- summary(lm((q25-q24)~I(price_late - price_average)-1 ,data=dyn_maize))$coefficients[1,1]
resmat_fe[2,2] <- summary(lm((q25-q24)~I(price_late - price_average)-1 ,data=dyn_maize))$coefficients[1,2]
resmat_fe[3,2] <- summary(lm((q25-q24)~I(price_late - price_average)-1 ,data=dyn_maize))$coefficients[1,4]
resmat_fe[4,2] <- nobs(lm((q25-q24)~I(price_late - price_average)-1 ,data=dyn_maize))

resmat_fe[5,2] <- summary(lm((q25-q24)~I(price_late - price_early)-1 ,data=dyn_maize))$coefficients[1,1]
resmat_fe[6,2] <- summary(lm((q25-q24)~I(price_late - price_early)-1 ,data=dyn_maize))$coefficients[1,2]
resmat_fe[7,2] <- summary(lm((q25-q24)~I(price_late - price_early)-1 ,data=dyn_maize))$coefficients[1,4]
resmat_fe[8,2] <- nobs(lm((q25-q24)~I(price_late - price_early)-1 ,data=dyn_maize))

resmat_fe[9,2] <- summary(lm((q25-q24)~I(price_average - price_early)-1 ,data=dyn_maize))$coefficients[1,1]
resmat_fe[10,2] <- summary(lm((q25-q24)~I(price_average - price_early)-1 ,data=dyn_maize))$coefficients[1,2]
resmat_fe[11,2] <- summary(lm((q25-q24)~I(price_average - price_early)-1 ,data=dyn_maize))$coefficients[1,4]
resmat_fe[12,2] <- nobs(lm((q25-q24)~I(price_average - price_early)-1 ,data=dyn_maize))

#gnuts
resmat_fe[1,3] <- summary(lm((q31-q30)~I(price_late - price_average)-1 ,data=dyn_gnuts))$coefficients[1,1]
resmat_fe[2,3] <- summary(lm((q31-q30)~I(price_late - price_average)-1 ,data=dyn_gnuts))$coefficients[1,2]
resmat_fe[3,3] <- summary(lm((q31-q30)~I(price_late - price_average)-1 ,data=dyn_gnuts))$coefficients[1,4]
resmat_fe[4,3] <- nobs(lm((q31-q30)~I(price_late - price_average)-1 ,data=dyn_gnuts))

resmat_fe[5,3] <- summary(lm((q31-q30)~I(price_late - price_early)-1 ,data=dyn_gnuts))$coefficients[1,1]
resmat_fe[6,3] <- summary(lm((q31-q30)~I(price_late - price_early)-1 ,data=dyn_gnuts))$coefficients[1,2]
resmat_fe[7,3] <- summary(lm((q31-q30)~I(price_late - price_early)-1 ,data=dyn_gnuts))$coefficients[1,4]
resmat_fe[8,3] <- nobs(lm((q31-q30)~I(price_late - price_early)-1 ,data=dyn_gnuts))

resmat_fe[9,3] <- summary(lm((q31-q30)~I(price_average - price_early)-1 ,data=dyn_gnuts))$coefficients[1,1]
resmat_fe[10,3] <- summary(lm((q31-q30)~I(price_average - price_early)-1 ,data=dyn_gnuts))$coefficients[1,2]
resmat_fe[11,3] <- summary(lm((q31-q30)~I(price_average - price_early)-1 ,data=dyn_gnuts))$coefficients[1,4]
resmat_fe[12,3] <- nobs(lm((q31-q30)~I(price_average - price_early)-1 ,data=dyn_gnuts))

#soy
resmat_fe[1,4] <- summary(lm((q28-q27)~I(price_late - price_average)-1 ,data=dyn_soy))$coefficients[1,1]
resmat_fe[2,4] <- summary(lm((q28-q27)~I(price_late - price_average)-1 ,data=dyn_soy))$coefficients[1,2]
resmat_fe[3,4] <- summary(lm((q28-q27)~I(price_late - price_average)-1 ,data=dyn_soy))$coefficients[1,4]
resmat_fe[4,4] <- nobs(lm((q28-q27)~I(price_late - price_average)-1 ,data=dyn_soy))

resmat_fe[5,4] <- summary(lm((q28-q27)~I(price_late - price_early)-1 ,data=dyn_soy))$coefficients[1,1]
resmat_fe[6,4] <- summary(lm((q28-q27)~I(price_late - price_early)-1 ,data=dyn_soy))$coefficients[1,2]
resmat_fe[7,4] <- summary(lm((q28-q27)~I(price_late - price_early)-1 ,data=dyn_soy))$coefficients[1,4]
resmat_fe[8,4] <- nobs(lm((q28-q27)~I(price_late - price_early)-1 ,data=dyn_soy))

resmat_fe[9,4] <- summary(lm((q28-q27)~I(price_average - price_early)-1 ,data=dyn_soy))$coefficients[1,1]
resmat_fe[10,4] <- summary(lm((q28-q27)~I(price_average - price_early)-1 ,data=dyn_soy))$coefficients[1,2]
resmat_fe[11,4] <- summary(lm((q28-q27)~I(price_average - price_early)-1 ,data=dyn_soy))$coefficients[1,4]
resmat_fe[12,4] <- nobs(lm((q28-q27)~I(price_average - price_early)-1 ,data=dyn_soy))

### stack and run pooled

stack_maize <- dyn_maize[c("farmer_ID","price_early","price_late","price_average","q24","q25")]
names(stack_maize) <- c("farmer_ID","price_early","price_late","price_average","p_sep","p_dec")
stack_maize$crop <- "maize"

stack_gnuts <- dyn_gnuts[c("farmer_ID","price_early","price_late","price_average","q30","q31")]
names(stack_gnuts) <- c("farmer_ID","price_early","price_late","price_average","p_sep","p_dec")
stack_gnuts$crop <- "gnuts"

stack_soy <- dyn_soy[c("farmer_ID","price_early","price_late","price_average","q27","q28")]
names(stack_soy) <- c("farmer_ID","price_early","price_late","price_average","p_sep","p_dec")
stack_soy$crop <- "soy"

stack <- rbind(stack_maize, stack_gnuts, stack_soy)

mod <- lm((p_dec-p_sep)~(price_late - price_average)-1,data=stack)
vcov_cluster <- vcovCR(mod, cluster=stack$farmer_ID, type = "CR2")
res <- coef_test(mod, vcov = vcov_cluster)

resmat_fe[1,1] <- res[1,2]
resmat_fe[2,1] <- res[1,3]
resmat_fe[3,1] <- res[1,6]
resmat_fe[4,1] <- nobs(mod)

mod <- lm((p_dec-p_sep)~(price_late - price_early)-1,data=stack)
vcov_cluster <- vcovCR(mod, cluster=stack$farmer_ID, type = "CR2")
res <- coef_test(mod, vcov = vcov_cluster)

resmat_fe[5,1] <- res[1,2]
resmat_fe[6,1] <- res[1,3]
resmat_fe[7,1] <- res[1,6]
resmat_fe[8,1] <- nobs(mod)

mod <- lm((p_dec-p_sep)~(price_average - price_early)-1,data=stack)
vcov_cluster <- vcovCR(mod, cluster=stack$farmer_ID, type = "CR2")
res <- coef_test(mod, vcov = vcov_cluster)

resmat_fe[9,1] <- res[1,2]
resmat_fe[10,1] <- res[1,3]
resmat_fe[11,1] <- res[1,6]
resmat_fe[12,1] <- nobs(mod)


dta$conversion_soy_harvest <- as.numeric(as.character(dta$group13.q63b2))
dta$harvest_soy <- as.numeric(as.character(dta$group13.q63b))
dta$harvest_soy[!is.na(dta$conversion_soy_harvest )] <- dta$harvest_soy[!is.na(dta$conversion_soy_harvest )] * dta$conversion_soy_harvest[!is.na(dta$conversion_soy_harvest )]

dta$conversion_soy_sold <- as.numeric(as.character(dta$group15.q65b))
dta$sold_soy <- as.numeric(as.character(dta$group15.q65))
dta$sold_soy[!is.na(dta$conversion_soy_sold)] <- dta$sold_soy[!is.na(dta$conversion_soy_sold)] * dta$conversion_soy_sold[!is.na(dta$conversion_soy_sold)]
dta$sold_soy[is.na(dta$sold_soy) & !is.na(dta$harvest_soy)]  <- 0

dta$soy_sold_share <- dta$sold_soy/dta$harvest_soy
dta$soy_sold_share[dta$soy_sold_share>1] <- NA

prop.table(table(dta$soy_sold_share>.8))
dta$q66 <- as.numeric(as.character(dta$q66))
dta$q66[dta$q66>1600] <- NA
median(dta$q66)

median(dta$q66[dta$soy_sold_share>.8], na.rm=T)
