#run in papers/price_expectations

library(reshape2)
library(ggplot2)
library(ggpubr)
library(dplyr)

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


###### Market Participation
# 1. Share of farmers that sold part of their harvest.
# 2. Share of the harvest that was sold-- commercialisation index
# 3. No. of transactions (mean & Median)
# 4. Timing of sales
# 5. Main buyer--- ongoing


### Share of farmers that sold part of their harvest
sold <- data.frame(dta$q41)  # sold part of the maize from previous season
sold$crop <- "Maize"
names(sold) <- c("Sold","Crop")
sold_table <- sold

sold <- data.frame(dta$q46)  #sold part of the gnuts from previous season
sold$crop <- "Groundnuts"
names(sold) <- c("Sold","Crop")
sold_table <- rbind(sold_table,sold)

sold <- data.frame(dta$q50)   # sold part of the soybean from previous season
sold$crop <- "Soybeans"
names(sold) <- c("Sold","Crop")
sold_table <- rbind(sold_table,sold)

sold_crop <- table(sold_table)
sold_crop_table <- round(proportions(sold_crop, 2), digits = 2)
sold_crop_table
names(sold_crop_table) <- c("variable", "Groundnuts", "Maize", "Soyabeans")

market_1 <- sold_crop_table
remove(sold_crop_table, sold, sold_crop, sold_table)
market_1

### Share of harvest that is sold- commercialisation index
# Maize
harvested_trans <- c("group1.q40", "group1.q40b",
                     "trans.1..group2.q43c",
                     "trans.1..group2.q43c2",
                     "trans.2..group2.q43c",
                     "trans.2..group2.q43c2",
                     "trans.3..group2.q43c",
                     "trans.3..group2.q43c2",
                     "trans.4..group2.q43c",
                     "trans.4..group2.q43c2",
                     "trans.5..group2.q43c",
                     "trans.5..group2.q43c2",
                     "trans.6..group2.q43c",
                     "trans.6..group2.q43c2",
                     "trans.7..group2.q43c",
                     "trans.7..group2.q43c2",
                     "trans.8..group2.q43c",
                     "trans.8..group2.q43c2")


dta[harvested_trans] <- lapply(dta[harvested_trans],  function(x) as.numeric(as.character(x)))

harvested_trans <- c("farmer_ID", "q39", "group1.q40a", "q41",
                      "trans.1..group2.q43c1",
                      "trans.2..group2.q43c1",
                      "trans.3..group2.q43c1",
                      "trans.4..group2.q43c1",
                      "trans.5..group2.q43c1",
                      "trans.6..group2.q43c1",
                      "trans.7..group2.q43c1",
                      "trans.8..group2.q43c1",
                      "trans.1..q43a",
                      "trans.2..q43a",
                      "trans.3..q43a",
                      "trans.4..q43a",
                      "trans.5..q43a",
                      "trans.6..q43a",
                      "trans.7..q43a",
                      "trans.8..q43a")

dta[harvested_trans] <- lapply(dta[harvested_trans],  function(x) as.character(x))

harvested_trans <- data.frame(dta$farmer_ID,
                              dta$group1.q40, dta$q41, 
                              dta$group1.q40a, 
                              dta$group1.q40b, dta$q39,
                              dta$trans.1..group2.q43c, 
                              dta$trans.1..group2.q43c2,
                              dta$trans.2..group2.q43c, 
                              dta$trans.2..group2.q43c2,
                              dta$trans.3..group2.q43c, 
                              dta$trans.3..group2.q43c2,
                              dta$trans.4..group2.q43c, 
                              dta$trans.4..group2.q43c2,
                              dta$trans.5..group2.q43c, 
                              dta$trans.5..group2.q43c2,
                              dta$trans.6..group2.q43c, 
                              dta$trans.6..group2.q43c2,
                              dta$trans.7..group2.q43c, 
                              dta$trans.7..group2.q43c2,
                              dta$trans.8..group2.q43c, 
                              dta$trans.8..group2.q43c2,
                              dta$trans.1..group2.q43c1,
                              dta$trans.2..group2.q43c1,
                              dta$trans.3..group2.q43c1,
                              dta$trans.4..group2.q43c1,
                              dta$trans.5..group2.q43c1,
                              dta$trans.6..group2.q43c1,
                              dta$trans.7..group2.q43c1,
                              dta$trans.8..group2.q43c1,
                              dta$trans.1..q43a,
                              dta$trans.2..q43a,
                              dta$trans.3..q43a,
                              dta$trans.4..q43a,
                              dta$trans.5..q43a,
                              dta$trans.6..q43a,
                              dta$trans.7..q43a,
                              dta$trans.8..q43a)

# standardizing amounts solds to kgs
harvested_trans$dta.trans.1..group2.q43c2[harvested_trans$dta.trans.1..group2.q43c1 == "kg"] <- 1
harvested_trans$dta.trans.2..group2.q43c2[harvested_trans$dta.trans.2..group2.q43c1 == "kg"] <- 1
harvested_trans$dta.trans.3..group2.q43c2[harvested_trans$dta.trans.3..group2.q43c1 == "kg"] <- 1
harvested_trans$dta.trans.4..group2.q43c2[harvested_trans$dta.trans.4..group2.q43c1 == "kg"] <- 1
harvested_trans$dta.trans.5..group2.q43c2[harvested_trans$dta.trans.5..group2.q43c1 == "kg"] <- 1
harvested_trans$dta.trans.6..group2.q43c2[harvested_trans$dta.trans.6..group2.q43c1 == "kg"] <- 1
harvested_trans$dta.trans.7..group2.q43c2[harvested_trans$dta.trans.7..group2.q43c1 == "kg"] <- 1
harvested_trans$dta.trans.8..group2.q43c2[harvested_trans$dta.trans.8..group2.q43c1 == "kg"] <- 1


harvested_trans2 <- data.frame(harvested_trans)

# multiplying to obtain total kgs
harvested_trans2$kgs.1 <- harvested_trans2$dta.trans.1..group2.q43c*harvested_trans2$dta.trans.1..group2.q43c2
harvested_trans2$kgs.2 <- harvested_trans2$dta.trans.2..group2.q43c*harvested_trans2$dta.trans.2..group2.q43c2
harvested_trans2$kgs.3 <- harvested_trans2$dta.trans.3..group2.q43c*harvested_trans2$dta.trans.3..group2.q43c2
harvested_trans2$kgs.4 <- harvested_trans2$dta.trans.4..group2.q43c*harvested_trans2$dta.trans.4..group2.q43c2
harvested_trans2$kgs.5 <- harvested_trans2$dta.trans.5..group2.q43c*harvested_trans2$dta.trans.5..group2.q43c2
harvested_trans2$kgs.6 <- harvested_trans2$dta.trans.6..group2.q43c*harvested_trans2$dta.trans.6..group2.q43c2
harvested_trans2$kgs.7 <- harvested_trans2$dta.trans.7..group2.q43c*harvested_trans2$dta.trans.7..group2.q43c2
harvested_trans2$kgs.8 <- harvested_trans2$dta.trans.8..group2.q43c*harvested_trans2$dta.trans.8..group2.q43c2

# replacing NAs in amount of transactions with 0
harvested_trans2$kgs.1[which(is.na(harvested_trans2$kgs.1))] <- 0
harvested_trans2$kgs.2[which(is.na(harvested_trans2$kgs.2))] <- 0
harvested_trans2$kgs.3[which(is.na(harvested_trans2$kgs.3))] <- 0
harvested_trans2$kgs.4[which(is.na(harvested_trans2$kgs.4))] <- 0
harvested_trans2$kgs.5[which(is.na(harvested_trans2$kgs.5))] <- 0
harvested_trans2$kgs.6[which(is.na(harvested_trans2$kgs.6))] <- 0
harvested_trans2$kgs.7[which(is.na(harvested_trans2$kgs.7))] <- 0
harvested_trans2$kgs.8[which(is.na(harvested_trans2$kgs.8))] <- 0

# summing up cumulative maize sales in kgs by transaction
harvested_trans2$sold_kgs <- harvested_trans2$kgs.1+harvested_trans2$kgs.2+harvested_trans2$kgs.3+harvested_trans2$kgs.4+harvested_trans2$kgs.5+harvested_trans2$kgs.6+harvested_trans2$kgs.7+harvested_trans2$kgs.8
harvested_trans2$t8 <- harvested_trans2$kgs.1+harvested_trans2$kgs.2+harvested_trans2$kgs.3+harvested_trans2$kgs.4+harvested_trans2$kgs.5+harvested_trans2$kgs.6+harvested_trans2$kgs.7+harvested_trans2$kgs.8
harvested_trans2$t7 <- harvested_trans2$kgs.1+harvested_trans2$kgs.2+harvested_trans2$kgs.3+harvested_trans2$kgs.4+harvested_trans2$kgs.5+harvested_trans2$kgs.6+harvested_trans2$kgs.7
harvested_trans2$t6 <- harvested_trans2$kgs.1+harvested_trans2$kgs.2+harvested_trans2$kgs.3+harvested_trans2$kgs.4+harvested_trans2$kgs.5+harvested_trans2$kgs.6
harvested_trans2$t5 <- harvested_trans2$kgs.1+harvested_trans2$kgs.2+harvested_trans2$kgs.3+harvested_trans2$kgs.4+harvested_trans2$kgs.5
harvested_trans2$t4 <- harvested_trans2$kgs.1+harvested_trans2$kgs.2+harvested_trans2$kgs.3+harvested_trans2$kgs.4
harvested_trans2$t3 <- harvested_trans2$kgs.1+harvested_trans2$kgs.2+harvested_trans2$kgs.3
harvested_trans2$t2 <- harvested_trans2$kgs.1+harvested_trans2$kgs.2
harvested_trans2$t1 <- harvested_trans2$kgs.1

harvested_trans <- data.frame(harvested_trans2$dta.farmer_ID,
                              harvested_trans2$dta.group1.q40, 
                              harvested_trans2$dta.q41, 
                              harvested_trans2$dta.group1.q40a, 
                              harvested_trans2$dta.group1.q40b, 
                              harvested_trans2$sold_kgs,
                              harvested_trans2$dta.q39,
                              harvested_trans2$t1, 
                              harvested_trans2$dta.trans.1..q43a,
                              harvested_trans2$t2, 
                              harvested_trans2$dta.trans.2..q43a,
                              harvested_trans2$t3, 
                              harvested_trans2$dta.trans.3..q43a,
                              harvested_trans2$t4, 
                              harvested_trans2$dta.trans.4..q43a,
                              harvested_trans2$t5, 
                              harvested_trans2$dta.trans.5..q43a,
                              harvested_trans2$t6, 
                              harvested_trans2$dta.trans.6..q43a,
                              harvested_trans2$t7, 
                              harvested_trans2$dta.trans.7..q43a,
                              harvested_trans2$t8, 
                              harvested_trans2$dta.trans.8..q43a)


names(harvested_trans) <- c("farmerID", "harvest", "Sold", "harvest_unit", 
                            "Kgs_harvest_unit", "sold_kgs",
                            "date_harvest", "t1", "date1",
                            "t2", "date2",
                            "t3", "date3",
                            "t4", "date4",
                            "t5", "date5",
                            "t6", "date6",
                            "t7", "date7",
                            "t8", "date8")

harvested_trans <- subset(harvested_trans, Sold == "Yes")

## amount harvested--- maize- used to compute share of sales

harvested_trans$harvest[harvested_trans$harvest == 0] <- NA

table(harvested_trans$harvest_unit)
harvested_trans$harvest_unit[harvested_trans$harvest_unit == 96] <- NA

harvested_trans$Kgs_harvest_unit[harvested_trans$harvest_unit == "kg"] <- 1

harvested_trans$harvest_kgs <- harvested_trans$harvest*harvested_trans$Kgs_harvest_unit

harvest_sold <- data.frame(harvested_trans$harvest_kgs, harvested_trans$sold_kgs)

harvest_sold$comm_index <- round((harvest_sold$harvested_trans.sold_kgs/harvest_sold$harvested_trans.harvest_kgs), digits = 3)

harvest_sold$crop <- "maize"

harvest_sold <- subset(harvest_sold, comm_index <= 1)

harvest_sold <- subset(harvest_sold, harvest_sold$harvested_trans.harvest_kgs < 8000000)

harvest_sold <- data.frame(harvest_sold$comm_index, harvest_sold$crop)

names(harvest_sold) <- c("Commer_index", "Crop")

commer_level <- data.frame(harvest_sold)

timing_maize_trans <- data.frame(harvested_trans$farmerID,
                                 harvested_trans$harvest_kgs,
                                 harvested_trans$date_harvest,
                                 harvested_trans$t1,
                                 harvested_trans$date1,
                                 harvested_trans$t2,
                                 harvested_trans$date2,
                                 harvested_trans$t3,
                                 harvested_trans$date3,
                                 harvested_trans$t4,
                                 harvested_trans$date4,
                                 harvested_trans$t5,
                                 harvested_trans$date5,
                                 harvested_trans$t6,
                                 harvested_trans$date6,
                                 harvested_trans$t7,
                                 harvested_trans$date7,
                                 harvested_trans$t8,
                                 harvested_trans$date8)
remove(harvest_sold, harvested_trans, harvested_trans2)


# G.nut--- share of harvest that is sold.
### Share of harvest that is sold- commercialisation index
# Groundnuts
harvested_trans1 <- c("group3.q45", "group3.q45b",
                      "trans1.1..group4.q47c",
                      "trans1.1..group4.q47c2",
                      "trans1.2..group4.q47c",
                      "trans1.2..group4.q47c2",
                      "trans1.3..group4.q47c",
                      "trans1.3..group4.q47c2",
                      "trans1.4..group4.q47c",
                      "trans1.4..group4.q47c2",
                      "trans1.5..group4.q47c",
                      "trans1.5..group4.q47c2")


dta[harvested_trans1] <- lapply(dta[harvested_trans1],  function(x) as.numeric(as.character(x)))

harvested_trans1 <- c("q44", "group3.q45a", "q46",
                       "trans1.1..group4.q47c1",
                       "trans1.2..group4.q47c1",
                       "trans1.3..group4.q47c1",
                       "trans1.4..group4.q47c1",
                       "trans1.5..group4.q47c1",
                       "trans1.1..q47a",
                       "trans1.2..q47a",
                       "trans1.3..q47a",
                       "trans1.4..q47a",
                       "trans1.5..q47a")

dta[harvested_trans1] <- lapply(dta[harvested_trans1],  function(x) as.character(x))

harvested_trans1 <- data.frame(dta$group3.q45, dta$q46, 
                               dta$group3.q45a, 
                               dta$group3.q45b, dta$q44,
                               dta$trans1.1..group4.q47c, 
                               dta$trans1.1..group4.q47c2,
                               dta$trans1.2..group4.q47c, 
                               dta$trans1.2..group4.q47c2,
                               dta$trans1.3..group4.q47c, 
                               dta$trans1.3..group4.q47c2,
                               dta$trans1.4..group4.q47c, 
                               dta$trans1.4..group4.q47c2,
                               dta$trans1.5..group4.q47c, 
                               dta$trans1.5..group4.q47c2,
                               dta$trans1.1..group4.q47c1,
                               dta$trans1.2..group4.q47c1,
                               dta$trans1.3..group4.q47c1,
                               dta$trans1.4..group4.q47c1,
                               dta$trans1.5..group4.q47c1,
                               dta$trans1.1..q47a,
                               dta$trans1.2..q47a,
                               dta$trans1.3..q47a,
                               dta$trans1.4..q47a,
                               dta$trans1.5..q47a)

# standardizing amounts solds to kgs
harvested_trans1$dta.trans1.1..group4.q47c2[harvested_trans1$dta.trans1.1..group4.q47c1 == "kg"] <- 1
harvested_trans1$dta.trans1.2..group4.q47c2[harvested_trans1$dta.trans1.2..group4.q47c1 == "kg"] <- 1
harvested_trans1$dta.trans1.3..group4.q47c2[harvested_trans1$dta.trans1.3..group4.q47c1 == "kg"] <- 1
harvested_trans1$dta.trans1.4..group4.q47c2[harvested_trans1$dta.trans1.4..group4.q47c1 == "kg"] <- 1
harvested_trans1$dta.trans1.5..group4.q47c2[harvested_trans1$dta.trans1.5..group4.q47c1 == "kg"] <- 1

harvested_trans12 <- data.frame(harvested_trans1)

# multiplying to obtain total kgs
harvested_trans12$kgs.1 <- harvested_trans12$dta.trans1.1..group4.q47c*harvested_trans12$dta.trans1.1..group4.q47c2
harvested_trans12$kgs.2 <- harvested_trans12$dta.trans1.2..group4.q47c*harvested_trans12$dta.trans1.2..group4.q47c2
harvested_trans12$kgs.3 <- harvested_trans12$dta.trans1.3..group4.q47c*harvested_trans12$dta.trans1.3..group4.q47c2
harvested_trans12$kgs.4 <- harvested_trans12$dta.trans1.4..group4.q47c*harvested_trans12$dta.trans1.4..group4.q47c2
harvested_trans12$kgs.5 <- harvested_trans12$dta.trans1.5..group4.q47c*harvested_trans12$dta.trans1.5..group4.q47c2

# replacing NAs in amount of trans1actions with 0
harvested_trans12$kgs.1[which(is.na(harvested_trans12$kgs.1))] <- 0
harvested_trans12$kgs.2[which(is.na(harvested_trans12$kgs.2))] <- 0
harvested_trans12$kgs.3[which(is.na(harvested_trans12$kgs.3))] <- 0
harvested_trans12$kgs.4[which(is.na(harvested_trans12$kgs.4))] <- 0
harvested_trans12$kgs.5[which(is.na(harvested_trans12$kgs.5))] <- 0

# summing up cumulative gnuts sales in kgs by trans1action
harvested_trans12$sold_kgs <- harvested_trans12$kgs.1+harvested_trans12$kgs.2+harvested_trans12$kgs.3+harvested_trans12$kgs.4+harvested_trans12$kgs.5
harvested_trans12$t5 <- harvested_trans12$kgs.1+harvested_trans12$kgs.2+harvested_trans12$kgs.3+harvested_trans12$kgs.4+harvested_trans12$kgs.5
harvested_trans12$t4 <- harvested_trans12$kgs.1+harvested_trans12$kgs.2+harvested_trans12$kgs.3+harvested_trans12$kgs.4
harvested_trans12$t3 <- harvested_trans12$kgs.1+harvested_trans12$kgs.2+harvested_trans12$kgs.3
harvested_trans12$t2 <- harvested_trans12$kgs.1+harvested_trans12$kgs.2
harvested_trans12$t1 <- harvested_trans12$kgs.1

harvested_trans1 <- data.frame(harvested_trans12$dta.group3.q45, 
                               harvested_trans12$dta.q46, 
                               harvested_trans12$dta.group3.q45a, 
                               harvested_trans12$dta.group3.q45b, 
                               harvested_trans12$sold_kgs,
                               harvested_trans12$dta.q44,
                               harvested_trans12$t1, 
                               harvested_trans12$dta.trans1.1..q47a,
                               harvested_trans12$t2, 
                               harvested_trans12$dta.trans1.2..q47a,
                               harvested_trans12$t3, 
                               harvested_trans12$dta.trans1.3..q47a,
                               harvested_trans12$t4, 
                               harvested_trans12$dta.trans1.4..q47a,
                               harvested_trans12$t5, 
                               harvested_trans12$dta.trans1.5..q47a)
          

names(harvested_trans1) <- c("harvest", "Sold", "harvest_unit", 
                             "Kgs_harvest_unit", "sold_kgs",
                             "date_harvest", "t1", "date1",
                             "t2", "date2",
                             "t3", "date3",
                             "t4", "date4",
                             "t5", "date5")

harvested_trans1 <- subset(harvested_trans1, Sold == "Yes")


## amount harvested--- gnuts- used to compute share of sales

harvested_trans1$harvest[harvested_trans1$harvest == 0] <- NA

table(harvested_trans1$harvest_unit)
harvested_trans1$harvest_unit[harvested_trans1$harvest_unit == 96] <- NA

harvested_trans1$Kgs_harvest_unit[harvested_trans1$harvest_unit == "kg"] <- 1

harvested_trans1$harvest_kgs <- harvested_trans1$harvest*harvested_trans1$Kgs_harvest_unit

harvest_soldg <- data.frame(harvested_trans1$harvest_kgs, harvested_trans1$sold_kgs)
names(harvest_soldg) <- c("harvest_kgs", "sold_kgs")
harvest_soldg$comm_index <- round((harvest_soldg$sold_kgs/harvest_soldg$harvest_kgs))

harvest_soldg$crop <- "Groundnuts"

harvest_soldg <- subset(harvest_soldg, comm_index <= 1)

harvest_soldg <- data.frame(harvest_soldg$comm_index, harvest_soldg$crop)

names(harvest_soldg) <- c("Commer_index", "Crop")

commer_level <- rbind(commer_level, harvest_soldg)

timing_gnuts_trans <- data.frame(harvested_trans1$harvest_kgs,
                                 harvested_trans1$date_harvest,
                                 harvested_trans1$t1,
                                 harvested_trans1$date1,
                                 harvested_trans1$t2,
                                 harvested_trans1$date2,
                                 harvested_trans1$t3,
                                 harvested_trans1$date3,
                                 harvested_trans1$t4,
                                 harvested_trans1$date4,
                                 harvested_trans1$t5,
                                 harvested_trans1$date5)

remove(harvested_trans2, harvested_trans1, harvested_trans12, harvest_soldg)

# Soybeans--- share of harvest that is sold.

harvested_trans2 <- c("group5.q49", "group5.q49b",
                      "trans2.1..group6.q52c",
                      "trans2.1..group6.q52c2",
                      "trans2.2..group6.q52c",
                      "trans2.2..group6.q52c2",
                      "trans2.3..group6.q52c",
                      "trans2.3..group6.q52c2",
                      "trans2.4..group6.q52c",
                      "trans2.4..group6.q52c2")


dta[harvested_trans2] <- lapply(dta[harvested_trans2],  function(x) as.numeric(as.character(x)))

harvested_trans2 <- c("q48", "group5.q49a", "q50",
                      "trans2.1..group6.q52c1",
                      "trans2.2..group6.q52c1",
                      "trans2.3..group6.q52c1",
                      "trans2.4..group6.q52c1",
                      "trans2.1..q52a",
                      "trans2.2..q52a",
                      "trans2.3..q52a",
                      "trans2.4..q52a")

dta[harvested_trans2] <- lapply(dta[harvested_trans2],  function(x) as.character(x))

harvested_trans2 <- data.frame(dta$group5.q49, dta$q50, 
                               dta$group5.q49a, 
                               dta$group5.q49b, dta$q48,
                               dta$trans2.1..group6.q52c, 
                               dta$trans2.1..group6.q52c2,
                               dta$trans2.2..group6.q52c, 
                               dta$trans2.2..group6.q52c2,
                               dta$trans2.3..group6.q52c, 
                               dta$trans2.3..group6.q52c2,
                               dta$trans2.4..group6.q52c, 
                               dta$trans2.4..group6.q52c2,
                               dta$trans2.1..group6.q52c1,
                               dta$trans2.2..group6.q52c1,
                               dta$trans2.3..group6.q52c1,
                               dta$trans2.4..group6.q52c1,
                               dta$trans2.1..q52a,
                               dta$trans2.2..q52a,
                               dta$trans2.3..q52a,
                               dta$trans2.4..q52a)

# standardizing amounts solds to kgs
harvested_trans2$dta.trans2.1..group6.q52c2[harvested_trans2$dta.trans2.1..group6.q52c1 == "kg"] <- 1
harvested_trans2$dta.trans2.2..group6.q52c2[harvested_trans2$dta.trans2.2..group6.q52c1 == "kg"] <- 1
harvested_trans2$dta.trans2.3..group6.q52c2[harvested_trans2$dta.trans2.3..group6.q52c1 == "kg"] <- 1
harvested_trans2$dta.trans2.4..group6.q52c2[harvested_trans2$dta.trans2.4..group6.q52c1 == "kg"] <- 1

harvested_trans22 <- data.frame(harvested_trans2)

# multiplying to obtain total kgs
harvested_trans22$kgs.1 <- harvested_trans22$dta.trans2.1..group6.q52c*harvested_trans22$dta.trans2.1..group6.q52c2
harvested_trans22$kgs.2 <- harvested_trans22$dta.trans2.2..group6.q52c*harvested_trans22$dta.trans2.2..group6.q52c2
harvested_trans22$kgs.3 <- harvested_trans22$dta.trans2.3..group6.q52c*harvested_trans22$dta.trans2.3..group6.q52c2
harvested_trans22$kgs.4 <- harvested_trans22$dta.trans2.4..group6.q52c*harvested_trans22$dta.trans2.4..group6.q52c2

# replacing NAs in amount of trans2actions with 0
harvested_trans22$kgs.1[which(is.na(harvested_trans22$kgs.1))] <- 0
harvested_trans22$kgs.2[which(is.na(harvested_trans22$kgs.2))] <- 0
harvested_trans22$kgs.3[which(is.na(harvested_trans22$kgs.3))] <- 0
harvested_trans22$kgs.4[which(is.na(harvested_trans22$kgs.4))] <- 0

# summing up cumulative soy sales in kgs by trans2action
harvested_trans22$sold_kgs <- harvested_trans22$kgs.1+harvested_trans22$kgs.2+harvested_trans22$kgs.3+harvested_trans22$kgs.4
harvested_trans22$t4 <- harvested_trans22$kgs.1+harvested_trans22$kgs.2+harvested_trans22$kgs.3+harvested_trans22$kgs.4
harvested_trans22$t3 <- harvested_trans22$kgs.1+harvested_trans22$kgs.2+harvested_trans22$kgs.3
harvested_trans22$t2 <- harvested_trans22$kgs.1+harvested_trans22$kgs.2
harvested_trans22$t1 <- harvested_trans22$kgs.1

names(harvested_trans22)
harvested_trans2 <- data.frame(harvested_trans22$dta.group5.q49,
                               harvested_trans22$dta.q50,
                               harvested_trans22$dta.group5.q49a,
                               harvested_trans22$dta.group5.q49b,
                               harvested_trans22$sold_kgs,
                               harvested_trans22$dta.q48,
                               harvested_trans22$t1,
                               harvested_trans22$dta.trans2.1..q52a,
                               harvested_trans22$t2,
                               harvested_trans22$dta.trans2.2..q52a,
                               harvested_trans22$t3,
                               harvested_trans22$dta.trans2.3..q52a,
                               harvested_trans22$t4,
                               harvested_trans22$dta.trans2.4..q52a)
 
names(harvested_trans2) <- c("harvest", "Sold", "harvest_unit", 
                             "Kgs_harvest_unit", "sold_kgs",
                             "date_harvest", "t1", "date1",
                             "t2", "date2",
                             "t3", "date3",
                             "t4", "date4")
harvested_trans2 <- subset(harvested_trans2, Sold == "Yes")


## amount harvested--- soy- used to compute share of sales

harvested_trans2$harvest[harvested_trans2$harvest == 0] <- NA

table(harvested_trans2$harvest_unit)
harvested_trans2$harvest_unit[harvested_trans2$harvest_unit == 96] <- NA

harvested_trans2$Kgs_harvest_unit[harvested_trans2$harvest_unit == "kg"] <- 1

harvested_trans2$harvest_kgs <- harvested_trans2$harvest*harvested_trans2$Kgs_harvest_unit

harvest_soldg <- data.frame(harvested_trans2$harvest_kgs, harvested_trans2$sold_kgs)
names(harvest_soldg) <- c("harvest_kgs", "sold_kgs")
harvest_soldg$comm_index <- round((harvest_soldg$sold_kgs/harvest_soldg$harvest_kgs))

harvest_soldg$crop <- "Soybeans"

harvest_soldg <- subset(harvest_soldg, comm_index <= 1)

harvest_soldg <- data.frame(harvest_soldg$comm_index, harvest_soldg$crop)

names(harvest_soldg) <- c("Commer_index", "Crop")

commer_level <- rbind(commer_level, harvest_soldg)

timing_soy_trans <- data.frame(harvested_trans2$harvest_kgs,
                               harvested_trans2$date_harvest,
                               harvested_trans2$t1,
                               harvested_trans2$date1,
                               harvested_trans2$t2,
                               harvested_trans2$date2,
                               harvested_trans2$t3,
                               harvested_trans2$date3,
                               harvested_trans2$t4,
                               harvested_trans2$date4)

remove(harvested_trans2, harvested_trans22)


# all commercialization index
tapply(commer_level$Commer_index, INDEX = commer_level$Crop, FUN = mean)
market_2 <- round(tapply(commer_level$Commer_index, INDEX = commer_level$Crop, FUN = mean), digits = 2)
market_2

### Number of transactions
transactions <- c("q42", "q47", "q51")
dta[transactions] <- lapply(dta[transactions],  function(x) as.numeric(as.character(x)))

trans_maize <- data.frame(dta$q42)
trans_maize$crop <- "Maize"
names(trans_maize) <- c("Sep_trans","Crop")
transactions <- trans_maize

trans_gnuts <- data.frame(dta$q47)
trans_gnuts$crop <- "Groundnuts"
names(trans_gnuts) <- c("Sep_trans","Crop")
transactions <- rbind(transactions, trans_gnuts)

trans_soy <- data.frame(dta$q51)
trans_soy$crop <- "Soybeans"
names(trans_soy) <- c("Sep_trans","Crop")
transactions <- rbind(transactions, trans_soy)
transactions <- subset(transactions, Sep_trans >= 1)

trans_sum <- summarise(group_by(transactions, Crop),
                 mean_transactions = mean(transactions$Sep_trans),
                 median_transaction = median(transactions$Sep_trans))
market_3 <- t(trans_sum)
market_3



# timing of transactions & cummulative amount sold 
# as a share of total harvest
#maize
names(timing_maize_trans)
names(timing_maize_trans) <- c("farmerID", 
                               "harvestkgs", 
                               "harvestdate",
                               "cumsale1", "date1", 
                               "cumsale2", "date2",
                               "cumsale3", "date3",
                               "cumsale4", "date4",
                               "cumsale5", "date5",
                               "cumsale6", "date6",
                               "cumsale7", "date7",
                               "cumsale8", "date8")

maize_trans1 <- data.frame(timing_maize_trans$farmerID,
                           timing_maize_trans$harvestkgs,
                           timing_maize_trans$harvestdate,
                           timing_maize_trans$cumsale1,
                           timing_maize_trans$date1)
names(maize_trans1) <- c("farmerID", "harvestkgs", 
                         "harvestdate", "total_kgs_sold", "date_sold")
timimg_sales <- data.frame(maize_trans1)

maize_trans2 <- data.frame(timing_maize_trans$farmerID,
                           timing_maize_trans$harvestkgs,
                           timing_maize_trans$harvestdate,
                           timing_maize_trans$cumsale2,
                           timing_maize_trans$date2)
names(maize_trans2) <- c("farmerID", "harvestkgs", 
                         "harvestdate", "total_kgs_sold", "date_sold")

timimg_sales <- rbind(timimg_sales, maize_trans2)

maize_trans3 <- data.frame(timing_maize_trans$farmerID,
                           timing_maize_trans$harvestkgs,
                           timing_maize_trans$harvestdate,
                           timing_maize_trans$cumsale3,
                           timing_maize_trans$date3)
names(maize_trans3) <- c("farmerID", "harvestkgs", 
                         "harvestdate", "total_kgs_sold", "date_sold")

timimg_sales <- rbind(timimg_sales, maize_trans3)

maize_trans4 <- data.frame(timing_maize_trans$farmerID,
                           timing_maize_trans$harvestkgs,
                           timing_maize_trans$harvestdate,
                           timing_maize_trans$cumsale4,
                           timing_maize_trans$date4)
names(maize_trans4) <- c("farmerID", "harvestkgs", 
                         "harvestdate", "total_kgs_sold", "date_sold")

timimg_sales <- rbind(timimg_sales, maize_trans4)

maize_trans5 <- data.frame(timing_maize_trans$farmerID,
                           timing_maize_trans$harvestkgs,
                           timing_maize_trans$harvestdate,
                           timing_maize_trans$cumsale5,
                           timing_maize_trans$date5)
names(maize_trans5) <- c("farmerID", "harvestkgs", 
                         "harvestdate", "total_kgs_sold", "date_sold")

timimg_sales <- rbind(timimg_sales, maize_trans5)

maize_trans6 <- data.frame(timing_maize_trans$farmerID,
                           timing_maize_trans$harvestkgs,
                           timing_maize_trans$harvestdate,
                           timing_maize_trans$cumsale6,
                           timing_maize_trans$date6)
names(maize_trans6) <- c("farmerID", "harvestkgs", 
                         "harvestdate", "total_kgs_sold", "date_sold")

timimg_sales <- rbind(timimg_sales, maize_trans6)

maize_trans7 <- data.frame(timing_maize_trans$farmerID,
                           timing_maize_trans$harvestkgs,
                           timing_maize_trans$harvestdate,
                           timing_maize_trans$cumsale7,
                           timing_maize_trans$date7)
names(maize_trans7) <- c("farmerID", "harvestkgs", 
                         "harvestdate", "total_kgs_sold", "date_sold")

timimg_sales <- rbind(timimg_sales, maize_trans7)

maize_trans8 <- data.frame(timing_maize_trans$farmerID,
                           timing_maize_trans$harvestkgs,
                           timing_maize_trans$harvestdate,
                           timing_maize_trans$cumsale8,
                           timing_maize_trans$date8)
names(maize_trans8) <- c("farmerID", "harvestkgs", 
                         "harvestdate", "total_kgs_sold", "date_sold")

timimg_sales <- rbind(timimg_sales, maize_trans8)
timimg_sales$Crop <- "Maize"
maize_timing_sales_ <- timimg_sales


# timing of sales--- Gnuts
# timing of transactions & cummulative amount sold as a share of total harvest
#gnuts
names(timing_gnuts_trans)
names(timing_gnuts_trans) <- c("harvestkgs", 
                               "harvestdate",
                               "cumsale1", "date1", 
                               "cumsale2", "date2",
                               "cumsale3", "date3",
                               "cumsale4", "date4",
                               "cumsale5", "date5")

gnuts_trans1 <- data.frame(timing_gnuts_trans$harvestkgs,
                           timing_gnuts_trans$harvestdate,
                           timing_gnuts_trans$cumsale1,
                           timing_gnuts_trans$date1)
names(gnuts_trans1) <- c("harvestkgs", 
                         "harvestdate", "total_kgs_sold", "date_sold")
timimg_sales <- data.frame(gnuts_trans1)

gnuts_trans2 <- data.frame(timing_gnuts_trans$harvestkgs,
                           timing_gnuts_trans$harvestdate,
                           timing_gnuts_trans$cumsale2,
                           timing_gnuts_trans$date2)
names(gnuts_trans2) <- c("harvestkgs", 
                         "harvestdate", "total_kgs_sold", "date_sold")

timimg_sales <- rbind(timimg_sales, gnuts_trans2)

gnuts_trans3 <- data.frame(timing_gnuts_trans$harvestkgs,
                           timing_gnuts_trans$harvestdate,
                           timing_gnuts_trans$cumsale3,
                           timing_gnuts_trans$date3)
names(gnuts_trans3) <- c("harvestkgs", 
                         "harvestdate", "total_kgs_sold", "date_sold")

timimg_sales <- rbind(timimg_sales, gnuts_trans3)

gnuts_trans4 <- data.frame(timing_gnuts_trans$harvestkgs,
                           timing_gnuts_trans$harvestdate,
                           timing_gnuts_trans$cumsale4,
                           timing_gnuts_trans$date4)
names(gnuts_trans4) <- c("harvestkgs", 
                         "harvestdate", "total_kgs_sold", "date_sold")

timimg_sales <- rbind(timimg_sales, gnuts_trans4)

gnuts_trans5 <- data.frame(timing_gnuts_trans$harvestkgs,
                           timing_gnuts_trans$harvestdate,
                           timing_gnuts_trans$cumsale5,
                           timing_gnuts_trans$date5)
names(gnuts_trans5) <- c("harvestkgs", 
                         "harvestdate", "total_kgs_sold", "date_sold")

timimg_sales <- rbind(timimg_sales, gnuts_trans5)
timimg_sales$Crop <- "Groundnuts"
gnuts_timing_sales_ <- timimg_sales

# timing of sales--- soy
# timing of transactions & cummulative amount sold as a share of total harvest
#soy
names(timing_soy_trans)
names(timing_soy_trans) <- c("harvestkgs", 
                             "harvestdate",
                             "cumsale1", "date1", 
                             "cumsale2", "date2",
                             "cumsale3", "date3",
                             "cumsale4", "date4")

soy_trans1 <- data.frame(timing_soy_trans$harvestkgs,
                         timing_soy_trans$harvestdate,
                         timing_soy_trans$cumsale1,
                         timing_soy_trans$date1)
names(soy_trans1) <- c("harvestkgs", 
                       "harvestdate", "total_kgs_sold", "date_sold")
timimg_sales <- data.frame(soy_trans1)

soy_trans2 <- data.frame(timing_soy_trans$harvestkgs,
                         timing_soy_trans$harvestdate,
                         timing_soy_trans$cumsale2,
                         timing_soy_trans$date2)
names(soy_trans2) <- c("harvestkgs", 
                       "harvestdate", "total_kgs_sold", "date_sold")

timimg_sales <- rbind(timimg_sales, soy_trans2)

soy_trans3 <- data.frame(timing_soy_trans$harvestkgs,
                         timing_soy_trans$harvestdate,
                         timing_soy_trans$cumsale3,
                         timing_soy_trans$date3)
names(soy_trans3) <- c("harvestkgs", 
                       "harvestdate", "total_kgs_sold", "date_sold")

timimg_sales <- rbind(timimg_sales, soy_trans3)

soy_trans4 <- data.frame(timing_soy_trans$harvestkgs,
                         timing_soy_trans$harvestdate,
                         timing_soy_trans$cumsale4,
                         timing_soy_trans$date4)
names(soy_trans4) <- c("harvestkgs", 
                       "harvestdate", "total_kgs_sold", "date_sold")

timimg_sales <- rbind(timimg_sales, soy_trans4)
timimg_sales$Crop <- "Soybeans"
soy_timing_sales_ <- timimg_sales

maize_timing_sales_ <- data.frame(maize_timing_sales_$harvestkgs,
                                  maize_timing_sales_$harvestdate,
                                  maize_timing_sales_$total_kgs_sold,
                                  maize_timing_sales_$date_sold,
                                  maize_timing_sales_$Crop)

names(maize_timing_sales_) <- c("harvestkgs", "harvestdate", 
                                "total_kgs_sold",
                                "date_sold", "Crop")
names(soy_timing_sales_)
All_sales_timing <- data.frame(maize_timing_sales_)
All_sales_timing <- rbind(All_sales_timing, gnuts_timing_sales_)
All_sales_timing <- rbind(All_sales_timing, soy_timing_sales_)

sales_timing <- data.frame(All_sales_timing)

remove(gnuts_timing_sales_, maize_timing_sales_,
       soy_timing_sales_, All_sales_timing, 
       All_timing_sales,
       timimg_sales, timing_gnuts_trans, timing_maize_trans,
       timing_soy_trans)

# share of total harvest sold over time

sales_timing$share_of_harvest <- sales_timing$total_kgs_sold / sales_timing$harvestkgs
# this divion is giving very weird results. will get back to it later.
sapply(sales_timing, class)

# time of harvest

sales_timing$date_sold <- as.Date(sales_timing$date_sold, 
                                  format = "%Y-%m-%d")

sales_timing$trans_date <- format(sales_timing$date_sold, "%b")

harvest_t <- table(sales_timing$harvestdate, sales_timing$Crop)
sales_t <- table(sales_timing$trans_date, sales_timing$Crop)

sales_time <- round(proportions(sales_t, 2), digits = 2)
sales_time
names(sales_time) <- c("sales_Month", "Groundnuts", "Maize", "Soyabeans")

harvest_time <- round(proportions(harvest_t, 2), digits = 2)
harvest_time
names(harvest_time) <- c("harvest_Month", "Groundnuts", "Maize", "Soyabeans")

harvest_time <- harvest_time*100
sales_time <- sales_time*100

harvest_time
sales_time

#### Tables, but yet to be exported
market_1 # proportion of farmers that sold part of 2021 harvest
market_2 # share of the 2021 harvest that was sold.
market_3 # Number of transactions 
harvest_time # percentage of harvests over the year
sales_time # percentage of sales over the year
#average cumulative sales over time # still working on it

