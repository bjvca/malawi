#run in papers/price_expectations

library(reshape2)
library(ggplot2)
library(ggpubr)

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

trans1 <- data.frame(dta$trans.1..q43a,dta$trans.1..q43b,dta$trans.1..q43d)
names(trans1) <- c("date","form","price")

sel <- c( "trans.2..q43b", "trans.2..q43d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.2..q43a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans2 <- data.frame(dta$trans.2..q43a,dta$trans.2..q43b,dta$trans.2..q43d) 
names(trans2) <- c("date","form","price")

sel <- c( "trans.3..q43b", "trans.3..q43d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.3..q43a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans3 <- data.frame(dta$trans.3..q43a,dta$trans.3..q43b,dta$trans.3..q43d) 
names(trans3) <- c("date","form","price")

sel <- c( "trans.4..q43b", "trans.4..q43d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.4..q43a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans4 <- data.frame(dta$trans.4..q43a,dta$trans.4..q43b,dta$trans.4..q43d) 
names(trans4) <- c("date","form","price")

sel <- c( "trans.5..q43b", "trans.5..q43d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.5..q43a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans5 <- data.frame(dta$trans.5..q43a,dta$trans.5..q43b,dta$trans.5..q43d) 
names(trans5) <- c("date","form","price")

sel <- c( "trans.6..q43b", "trans.6..q43d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.6..q43a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans6 <- data.frame(dta$trans.6..q43a,dta$trans.6..q43b,dta$trans.6..q43d) 
names(trans6) <- c("date","form","price")

sel <- c( "trans.7..q43b", "trans.7..q43d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.7..q43a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans7 <- data.frame(dta$trans.7..q43a,dta$trans.7..q43b,dta$trans.7..q43d) 
names(trans7) <- c("date","form","price")

sel <- c( "trans.8..q43b", "trans.8..q43d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans.8..q43a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans8 <- data.frame(dta$trans.8..q43a,dta$trans.8..q43b,dta$trans.8..q43d) 
names(trans8) <- c("date","form","price")

all <- rbind(trans1, trans2, trans3, trans4, trans5, trans6, trans7,trans8)
names(all) <-  c("date","form","price")
all <- subset(all, form ==2)
all$price[all$price > 999] <- NA
all$price[all$price < 25] <- NA
to_plot_maize <-data.frame(tapply(all$price, all$date, FUN=median, na.rm=T)[6:16])
to_plot_maize$date <- rownames(to_plot_maize)
names(to_plot_maize) <- c("price","date")

#now for gnuts

sel <- c( "trans1.1..q47b", "trans1.1..q47d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans1.1..q47a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans11 <- data.frame(dta$trans1.1..q47a,dta$trans1.1..q47b,dta$trans1.1..q47d)
names(trans11) <- c("date","form","price")

sel <- c( "trans1.2..q47b", "trans1.2..q47d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans1.2..q47a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans12 <- data.frame(dta$trans1.2..q47a,dta$trans1.2..q47b,dta$trans1.2..q47d) 
names(trans12) <- c("date","form","price")

sel <- c( "trans1.3..q47b", "trans1.3..q47d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans1.3..q47a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans13 <- data.frame(dta$trans1.3..q47a,dta$trans1.3..q47b,dta$trans1.3..q47d) 
names(trans13) <- c("date","form","price")

sel <- c( "trans1.4..q47b", "trans1.4..q47d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans1.4..q47a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans14 <- data.frame(dta$trans1.4..q47a,dta$trans1.4..q47b,dta$trans1.4..q47d) 
names(trans14) <- c("date","form","price")

sel <- c( "trans1.5..q47b", "trans1.5..q47d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans1.5..q47a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans15 <- data.frame(dta$trans1.5..q47a,dta$trans1.5..q47b,dta$trans1.5..q47d) 
names(trans15) <- c("date","form","price")

all <- rbind(trans11, trans12, trans13, trans14, trans15)
names(all) <-  c("date","form","price")
all <- subset(all, form ==2)
all$price[all$price > 19999] <- NA
all$price[all$price < 999] <- NA
all$price <- all$price/20 
to_plot_gnuts <-data.frame(tapply(all$price, all$date, FUN=median, na.rm=T)[4:14])
to_plot_gnuts$date <- rownames(to_plot_gnuts)
names(to_plot_gnuts) <- c("price","date")

### for soybean

sel <- c( "trans2.1..q52b", "trans2.1..q52d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans2.1..q52a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans21 <- data.frame(dta$trans2.1..q52a,dta$trans2.1..q52b,dta$trans2.1..q52d)
names(trans21) <- c("date","form","price")

sel <- c( "trans2.2..q52b", "trans2.2..q52d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans2.2..q52a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans22 <- data.frame(dta$trans2.2..q52a,dta$trans2.2..q52b,dta$trans2.2..q52d) 
names(trans22) <- c("date","form","price")

sel <- c( "trans2.3..q52b", "trans2.3..q52d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans2.3..q52a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans23 <- data.frame(dta$trans2.3..q52a,dta$trans2.3..q52b,dta$trans2.3..q52d) 
names(trans23) <- c("date","form","price")

sel <- c( "trans2.4..q52b", "trans2.4..q52d")
dta[sel] <- lapply(dta[sel],  function(x) as.numeric(as.character(x)))
sel <- c( "trans2.4..q52a")
dta[sel] <- lapply(dta[sel],  function(x) as.character(x))

trans24 <- data.frame(dta$trans2.4..q52a,dta$trans2.4..q52b,dta$trans2.4..q52d) 
names(trans24) <- c("date","form","price")

all <- rbind(trans21, trans22, trans23, trans24)
names(all) <-  c("date","form","price")
all <- subset(all, form ==1)
all$price[all$price > 1999] <- NA
all$price[all$price < 99] <- NA
to_plot_soy <-data.frame(tapply(all$price, all$date, FUN=median, na.rm=T)[5:15])
to_plot_soy$date <- rownames(to_plot_soy)
names(to_plot_soy) <- c("price","date")

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


