#run in papers/price_expectations
rm(list=ls())
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

dta$q32[dta$q32 > 80000] <- NA
dta$q32a[dta$q32a > 80000] <- NA

dta$q32[dta$q32 == 0] <- NA
dta$q32a[dta$q32a == 0] <- NA

dta$q29[dta$q29 > 100000] <- NA
dta$q29[dta$q29 ==0] <- NA
dta$q26[dta$q26 ==0] <- NA

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
  
### create graph for production
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

dta$q39[dta$q39 == "88"] <- NA
dta$q39 <- factor(dta$q39, levels=month.name)
maize_prod <- data.frame(tapply(dta$maize_harv,dta$q39, sum, na.rm=T))
maize_prod$month <- rownames(maize_prod)
maize_prod$crop <- "maize"
names(maize_prod) <- c("prod","month","crop")

dta$q44[dta$q44 == "88"] <- NA
dta$q44 <- factor(dta$q44, levels=month.name)
gnuts_prod <- data.frame(tapply(dta$gnuts_harv,dta$q44, sum, na.rm=T))
gnuts_prod$month <- rownames(gnuts_prod)
gnuts_prod$crop <- "gnuts"
names(gnuts_prod) <- c("prod","month","crop")

dta$q48[dta$q48 == "88"] <- NA
dta$q48 <- factor(dta$q48, levels=month.name)
soy_prod <- data.frame(tapply(dta$soy_harv,dta$q48, sum, na.rm=T))
soy_prod$month <- rownames(soy_prod)
soy_prod$crop <- "soy"
names(soy_prod) <- c("prod","month","crop")

all_prod <- rbind(maize_prod, gnuts_prod, soy_prod)
all_prod$month <- factor(all_prod$month, levels=month.name)
all_prod$crop <- factor(all_prod$crop)
  

# Plot
## quantities    
plot_1 <- ggplot(all_prod[all_prod$month%in%c("March","April","May","June","July","Auguts"),], aes(fill=crop, y=prod, x=month)) + 
    geom_bar(position="stack", stat="identity")
all_prod$price[all_prod$crop == "gnuts"] <- 187
all_prod$price[all_prod$crop == "maize"] <- 114
all_prod$price[all_prod$crop == "soy"] <- 89

all_prod$value <- all_prod$price*all_prod$prod

plot_2 <- ggplot(all_prod[all_prod$month%in%c("March","April","May","June","July","Auguts"),], aes(fill=crop, y=value, x=month)) + 
  geom_bar(position="stack", stat="identity")

png(paste(path,"papers/price_expectations/results/fig_prod.png",sep = ""), units="px", height=3200, width= 3800, res=600)
ggarrange(plot_1, plot_2, heights = c(2, 2,2), ncol = 1, nrow = 2, align = "v")
dev.off()
 
### create a time series graph with average prices 
### first stack data of different transactions
### start with maize  
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

to_plot_maize <-data.frame(tapply(all$price, all$date, FUN=median, na.rm=T)[7:17])
to_plot_maize$date <- rownames(to_plot_maize)
names(to_plot_maize) <- c("price","date")

all_maize <- all

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
all$price <- all$price/20 

to_plot_gnuts <-data.frame(tapply(all$price, all$date, FUN=median, na.rm=T)[4:14])
to_plot_gnuts$date <- rownames(to_plot_gnuts)
names(to_plot_gnuts) <- c("price","date")
to_plot_gnuts[11,2] <- "2022-03-01"
to_plot_gnuts[11,1] <- NA


all_gnuts <- all

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

to_plot_soy <-data.frame(tapply(all$price, all$date, FUN=median, na.rm=T)[5:15])
to_plot_soy$date <- rownames(to_plot_soy)
names(to_plot_soy) <- c("price","date")

to_plot_soy[10,2] <- "2022-02-01"
to_plot_soy[10,1] <- NA

to_plot_soy[11,2] <- "2022-03-01"
to_plot_soy[11,1] <- NA

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

###interlude - now that we have prices, create production in value

tapply(all_1[all_1$month%in%c("2021-03-01","2021-04-01","2021-05-01","2021-06-01","2021-07-01","2021-08-01"),]$price,all_1[all_1$month%in%c("2021-03-01","2021-04-01","2021-05-01","2021-06-01","2021-07-01","2021-08-01"),]$crop, mean, na.rm=T)


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

##% of all farmers sold at least once
dim(aggregate(all_transactions$quant>0, list(all_transactions$farmer_ID), FUN=sum))[1] / dim(dta)[1] *100


sum(aggregate(all_transactions$quant>0, list(all_transactions$farmer_ID), FUN=sum)==1)/dim(aggregate(all_transactions$quant>0, list(all_transactions$farmer_ID), FUN=sum))[1]*100
### do farmers who sell at least once have higher price expectations?

round(dim(aggregate(all_transactions[all_transactions$crop=="maize",]$quant>0, list(all_transactions[all_transactions$crop=="maize",]$farmer_ID), FUN=sum))[1] /sum(!is.na(dta$q39))*100, digits=0)

round(dim(aggregate(all_transactions[all_transactions$crop=="gnuts",]$quant>0, list(all_transactions[all_transactions$crop=="gnuts",]$farmer_ID), FUN=sum))[1] /sum(!is.na(dta$q44))*100, digits=0)

round(dim(aggregate(all_transactions[all_transactions$crop=="soy",]$quant>0, list(all_transactions[all_transactions$crop=="soy",]$farmer_ID), FUN=sum))[1] /sum(!is.na(dta$q48))*100, digits=0)

sum(aggregate(all_transactions[all_transactions$crop=="maize",]$quant>0, list(all_transactions[all_transactions$crop=="maize",]$farmer_ID), FUN=sum)==1)/dim(aggregate(all_transactions[all_transactions$crop=="maize",]$quant>0, list(all_transactions[all_transactions$crop=="maize",]$farmer_ID), FUN=sum))[1] *100

sum(aggregate(all_transactions[all_transactions$crop=="gnuts",]$quant>0, list(all_transactions[all_transactions$crop=="gnuts",]$farmer_ID), FUN=sum)==1)/dim(aggregate(all_transactions[all_transactions$crop=="gnuts",]$quant>0, list(all_transactions[all_transactions$crop=="gnuts",]$farmer_ID), FUN=sum))[1] *100

sum(aggregate(all_transactions[all_transactions$crop=="soy",]$quant>0, list(all_transactions[all_transactions$crop=="soy",]$farmer_ID), FUN=sum)==1)/dim(aggregate(all_transactions[all_transactions$crop=="soy",]$quant>0, list(all_transactions[all_transactions$crop=="soy",]$farmer_ID), FUN=sum))[1] *100


### sold to?
sold_to_res <- matrix(NA,4,6)

sold_to_res[,2] <- tapply(all_transactions[all_transactions$crop=="maize",]$price,all_transactions[all_transactions$crop=="maize",]$buyer, mean, na.rm=T)[1:4]
sold_to_res[,4] <- tapply(all_transactions[all_transactions$crop=="soy",]$price,all_transactions[all_transactions$crop=="soy",]$buyer, mean, na.rm=T)[1:4]
sold_to_res[,6] <- tapply(all_transactions[all_transactions$crop=="gnuts",]$price,all_transactions[all_transactions$crop=="gnuts",]$buyer, mean, na.rm=T)[1:4]

all_transactions$ones <- 1

sold_to_res[,1] <- tapply(all_transactions[all_transactions$crop=="maize",]$one,all_transactions[all_transactions$crop=="maize",]$buyer, sum, na.rm=T)[1:4]/dim(all_transactions[all_transactions$crop=="maize",])[1]*100
sold_to_res[,3] <- tapply(all_transactions[all_transactions$crop=="soy",]$one,all_transactions[all_transactions$crop=="soy",]$buyer, sum, na.rm=T)[1:4]/dim(all_transactions[all_transactions$crop=="soy",])[1]*100
sold_to_res[,5] <- tapply(all_transactions[all_transactions$crop=="gnuts",]$one,all_transactions[all_transactions$crop=="gnuts",]$buyer, sum, na.rm=T)[1:4]/dim(all_transactions[all_transactions$crop=="gnuts",])[1]*100
sold_to_res <- round(sold_to_res, digits=0)

### sold by whom?



sold_by_res_maize <- data.frame(tapply(all_transactions[all_transactions$crop=="maize",]$one,all_transactions[all_transactions$crop=="maize",]$decision, sum, na.rm=T)[1:5]/sum(tapply(all_transactions[all_transactions$crop=="maize",]$one,all_transactions[all_transactions$crop=="maize",]$decision, sum, na.rm=T)[1:5])*100)
sold_by_res_soy <- data.frame(tapply(all_transactions[all_transactions$crop=="soy",]$one,all_transactions[all_transactions$crop=="soy",]$decision, sum, na.rm=T)[1:5]/sum(tapply(all_transactions[all_transactions$crop=="soy",]$one,all_transactions[all_transactions$crop=="soy",]$decision, sum, na.rm=T)[1:5])*100)
sold_by_res_gnuts <- data.frame(tapply(all_transactions[all_transactions$crop=="gnuts",]$one,all_transactions[all_transactions$crop=="gnuts",]$decision, sum, na.rm=T)[1:5]/sum(tapply(all_transactions[all_transactions$crop=="gnuts",]$one,all_transactions[all_transactions$crop=="gnuts",]$decision, sum, na.rm=T)[1:5])*100)
names(sold_by_res_maize) <- "percent"
names(sold_by_res_soy) <- "percent"
names(sold_by_res_gnuts) <- "percent"
sold_by_res_maize$crop <- "maize" 
sold_by_res_soy$crop <- "soy" 
sold_by_res_gnuts$crop <- "gnuts" 
sold_by_res_maize$dec <- c("woman alone","man alone","woman +", "man +", "joint")
sold_by_res_soy$dec <- c("woman alone","man alone","woman +", "man +", "joint")
sold_by_res_gnuts$dec <- c("woman alone","man alone","woman +", "man +", "joint")
sold_by_all <- rbind(sold_by_res_maize,sold_by_res_soy,sold_by_res_gnuts)

sold_by_all$percent <- round(sold_by_all$percent, digits=0)

png(paste(path,"papers/price_expectations/results/fig_dec.png",sep = ""), units="px", height=1200, width= 3800, res=600)
# Stacked + percent
ggplot(sold_by_all, aes(fill=dec, y=percent, x=crop)) + 
    geom_bar(position="fill", stat="identity", width = 0.3) +coord_flip()
dev.off()

### merge in access to credit
library(dplyr)
all_trans_cred <- merge(all_transactions,dta[c("farmer_ID","exp.q73")], by="farmer_ID")

use_cred_y <- data.frame(tapply(all_trans_cred[all_trans_cred$exp.q73=="Yes",]$one,all_trans_cred[all_trans_cred$exp.q73=="Yes",]$use, sum, na.rm=T)[1:14]/sum(tapply(all_trans_cred[all_trans_cred$exp.q73=="Yes",]$one,all_trans_cred[all_trans_cred$exp.q73=="Yes",]$use, sum, na.rm=T)[1:14])*100)

use_cred_n <-data.frame(tapply(all_trans_cred[all_trans_cred$exp.q73=="No",]$one,all_trans_cred[all_trans_cred$exp.q73=="No",]$use, sum, na.rm=T)[1:14]/sum(tapply(all_trans_cred[all_trans_cred$exp.q73=="No",]$one,all_trans_cred[all_trans_cred$exp.q73=="No",]$use, sum, na.rm=T)[1:14])*100)

all_cred <- cbind(use_cred_y, use_cred_n  )
names(all_cred) <- c("access_to_credit","no_access_to_credit")
all_cred$categories <- c("Clothing and footware",
"Food and non-alcoholic beverages (home consumption)",
"Alcoholic beverages, tobacco and narcotics",
"Health expenditure",
"Transport expenditures",
"Communications expenditures",
"Education expenditures",
"Housing (incl rent), water, electricity, gas and other fuels",
"Agricultural investment (land rental, livestock, seed, fertilizer,...)",
"To pay back a loan",
"Ceremonial expenses (burial, marriage, ...)",
"Restaurants and hotels",
"Furnishings, household equipment and routing maintenance",
"Miscellaneous goods and services"
)

all_cred <- all_cred  %>%  rowwise() %>% 
  mutate( mymean = mean(c(access_to_credit,no_access_to_credit) )) %>% 
  arrange(mymean) %>% 
  mutate(categories=factor(categories,categories))
  
  all_cred <- melt(all_cred[,1:3])
  names(all_cred) <- c("categories","variable","percentage")

png(paste(path,"papers/price_expectations/results/fig_exp.png",sep = ""), units="px", height=1200, width= 3800, res=400)

ggplot(all_cred, aes(percentage,categories )) +
        geom_line(aes(group = categories)) +
        geom_point(aes(color = variable))
dev.off()
##identify transactions that happen less than 3 months after harvest
### merge in production dates from dta into all_transactions
trans_date <- merge(all_transactions,dta[c("farmer_ID","q39","q44","q48")],by="farmer_ID")



trans_date[trans_date$crop == "maize",]
trans_date$pdate_num_maize <- NA
trans_date$pdate_num_maize[trans_date$q39=="April"] <- 18718 
trans_date$pdate_num_maize[trans_date$q39=="May"] <- 18748 
trans_date$pdate_num_maize[trans_date$q39=="June"] <- 18779 
trans_date$pdate_num_maize[trans_date$q39=="July"] <- 18809 

trans_date$pdate_num_gnuts <- NA
trans_date$pdate_num_gnuts[trans_date$q44=="April"] <- 18718 
trans_date$pdate_num_gnuts[trans_date$q44=="May"] <- 18748 
trans_date$pdate_num_gnuts[trans_date$q44=="June"] <- 18779 
trans_date$pdate_num_gnuts[trans_date$q44=="July"] <- 18809 

trans_date$pdate_num_soy <- NA
trans_date$pdate_num_soy[trans_date$q48=="April"] <- 18718 
trans_date$pdate_num_soy[trans_date$q48=="May"] <- 18748 
trans_date$pdate_num_soy[trans_date$q48=="June"] <- 18779 
trans_date$pdate_num_soy[trans_date$q48=="July"] <- 18809 

sum((as.numeric(as.Date(trans_date[trans_date$crop == "maize",]$date))- trans_date[trans_date$crop == "maize",]$pdate_num_maize >=0) & (as.numeric(as.Date(trans_date[trans_date$crop == "maize",]$date))- trans_date[trans_date$crop == "maize",]$pdate_num_maize < 90), na.rm=T)/(sum(as.numeric(as.Date(trans_date[trans_date$crop == "maize",]$date))- trans_date[trans_date$crop == "maize",]$pdate_num_maize >=0, na.rm=T))*100 


sum((as.numeric(as.Date(trans_date[trans_date$crop == "gnuts",]$date))- trans_date[trans_date$crop == "gnuts",]$pdate_num_gnuts >=0) & (as.numeric(as.Date(trans_date[trans_date$crop == "gnuts",]$date))- trans_date[trans_date$crop == "gnuts",]$pdate_num_gnuts < 90), na.rm=T)/(sum(as.numeric(as.Date(trans_date[trans_date$crop == "gnuts",]$date))- trans_date[trans_date$crop == "gnuts",]$pdate_num_gnuts >=0, na.rm=T))*100


sum((as.numeric(as.Date(trans_date[trans_date$crop == "soy",]$date))- trans_date[trans_date$crop == "soy",]$pdate_num_soy >=0) & (as.numeric(as.Date(trans_date[trans_date$crop == "soy",]$date))- trans_date[trans_date$crop == "soy",]$pdate_num_soy < 90), na.rm=T)/(sum(as.numeric(as.Date(trans_date[trans_date$crop == "soy",]$date))- trans_date[trans_date$crop == "soy",]$pdate_num_soy >=0, na.rm=T))*100


#### needs to be aggreagated at farmer level -  sells at at least on
trans_date$sell_early <- NA
trans_date[trans_date$crop == "maize",]$sell_early <-  (as.numeric(as.Date(trans_date[trans_date$crop == "maize",]$date))- trans_date[trans_date$crop == "maize",]$pdate_num_maize >=0) & (as.numeric(as.Date(trans_date[trans_date$crop == "maize",]$date))- trans_date[trans_date$crop == "maize",]$pdate_num_maize < 90)

nom_maize <- sum((aggregate(trans_date[trans_date$crop == "maize",]$sell_early,list(trans_date[trans_date$crop == "maize",]$farmer_ID), FUN=sum, na.rm=T)[,2]  >0), na.rm=T)

trans_date$sell_corr <- NA
trans_date[trans_date$crop == "maize",]$sell_corr <- (as.numeric(as.Date(trans_date[trans_date$crop == "maize",]$date))- trans_date[trans_date$crop == "maize",]$pdate_num_maize) >= 0

denom_maize <-sum(aggregate(trans_date$sell_corr,list(trans_date$farmer_ID),FUN=sum, na.rm=T)[,2]>0, na.rm=T)
#gnuts
trans_date$sell_early <- NA
trans_date[trans_date$crop == "gnuts",]$sell_early <-  (as.numeric(as.Date(trans_date[trans_date$crop == "gnuts",]$date))- trans_date[trans_date$crop == "gnuts",]$pdate_num_gnuts >=0) & (as.numeric(as.Date(trans_date[trans_date$crop == "gnuts",]$date))- trans_date[trans_date$crop == "gnuts",]$pdate_num_gnuts < 90)

nom_gnuts  <- sum((aggregate(trans_date[trans_date$crop == "gnuts",]$sell_early,list(trans_date[trans_date$crop == "gnuts",]$farmer_ID), FUN=sum, na.rm=T)[,2]  >0), na.rm=T)

trans_date$sell_corr <- NA
trans_date[trans_date$crop == "gnuts",]$sell_corr <- (as.numeric(as.Date(trans_date[trans_date$crop == "gnuts",]$date))- trans_date[trans_date$crop == "gnuts",]$pdate_num_gnuts) >= 0

denom_gnuts <-sum(aggregate(trans_date$sell_corr,list(trans_date$farmer_ID),FUN=sum, na.rm=T)[,2]>0, na.rm=T)

#soy
trans_date$sell_early <- NA
trans_date[trans_date$crop == "soy",]$sell_early <-  (as.numeric(as.Date(trans_date[trans_date$crop == "soy",]$date))- trans_date[trans_date$crop == "soy",]$pdate_num_soy >=0) & (as.numeric(as.Date(trans_date[trans_date$crop == "soy",]$date))- trans_date[trans_date$crop == "soy",]$pdate_num_soy < 90)

nom_soy  <- sum((aggregate(trans_date[trans_date$crop == "soy",]$sell_early,list(trans_date[trans_date$crop == "soy",]$farmer_ID), FUN=sum, na.rm=T)[,2]  >0), na.rm=T)

trans_date$sell_corr <- NA
trans_date[trans_date$crop == "soy",]$sell_corr <- (as.numeric(as.Date(trans_date[trans_date$crop == "soy",]$date))- trans_date[trans_date$crop == "soy",]$pdate_num_soy) >= 0

denom_soy <-sum(aggregate(trans_date$sell_corr,list(trans_date$farmer_ID),FUN=sum, na.rm=T)[,2]>0, na.rm=T)

###now interaction between sold only once and sold 
#for maize

trans_date$sell_early <- NA
trans_date[trans_date$crop == "maize",]$sell_early <-  (as.numeric(as.Date(trans_date[trans_date$crop == "maize",]$date))- trans_date[trans_date$crop == "maize",]$pdate_num_maize >=0) & (as.numeric(as.Date(trans_date[trans_date$crop == "maize",]$date))- trans_date[trans_date$crop == "maize",]$pdate_num_maize < 90)
trans_date$ones <- 1
pt1 <- aggregate(trans_date[trans_date$crop == "maize",]$sell_early,list(trans_date[trans_date$crop == "maize",]$farmer_ID),FUN=sum, na.rm=T)
names(pt1) <- c("farmer_ID","sold_early")

pt2 <- aggregate(all_transactions[all_transactions$crop=="maize",]$quant>0, list(all_transactions[all_transactions$crop=="maize",]$farmer_ID), FUN=sum)
names(pt2) <- c("farmer_ID","nr_sales")
mergepts <- merge(pt1,pt2, by="farmer_ID")
nomoXe_maize <- sum((mergepts$sold_early>0) & (mergepts$nr_sales == 1))/dim(pt2)[1]*100

#for gnuts

trans_date$sell_early <- NA
trans_date[trans_date$crop == "gnuts",]$sell_early <-  (as.numeric(as.Date(trans_date[trans_date$crop == "gnuts",]$date))- trans_date[trans_date$crop == "gnuts",]$pdate_num_gnuts >=0) & (as.numeric(as.Date(trans_date[trans_date$crop == "gnuts",]$date))- trans_date[trans_date$crop == "gnuts",]$pdate_num_gnuts < 90)
trans_date$ones <- 1
pt1 <- aggregate(trans_date[trans_date$crop == "gnuts",]$sell_early,list(trans_date[trans_date$crop == "gnuts",]$farmer_ID),FUN=sum, na.rm=T)
names(pt1) <- c("farmer_ID","sold_early")

pt2 <- aggregate(all_transactions[all_transactions$crop=="gnuts",]$quant>0, list(all_transactions[all_transactions$crop=="gnuts",]$farmer_ID), FUN=sum)
names(pt2) <- c("farmer_ID","nr_sales")
mergepts <- merge(pt1,pt2, by="farmer_ID")
nomoXe_gnuts <- sum((mergepts$sold_early>0) & (mergepts$nr_sales == 1))/dim(pt2)[1]*100

#for soy

trans_date$sell_early <- NA
trans_date[trans_date$crop == "soy",]$sell_early <-  (as.numeric(as.Date(trans_date[trans_date$crop == "soy",]$date))- trans_date[trans_date$crop == "soy",]$pdate_num_soy >=0) & (as.numeric(as.Date(trans_date[trans_date$crop == "soy",]$date))- trans_date[trans_date$crop == "soy",]$pdate_num_soy < 90)
trans_date$ones <- 1
pt1 <- aggregate(trans_date[trans_date$crop == "soy",]$sell_early,list(trans_date[trans_date$crop == "soy",]$farmer_ID),FUN=sum, na.rm=T)
names(pt1) <- c("farmer_ID","sold_early")

pt2 <- aggregate(all_transactions[all_transactions$crop=="soy",]$quant>0, list(all_transactions[all_transactions$crop=="soy",]$farmer_ID), FUN=sum)
names(pt2) <- c("farmer_ID","nr_sales")
mergepts <- merge(pt1,pt2, by="farmer_ID")
nomoXe_soy <- sum((mergepts$sold_early>0) & (mergepts$nr_sales == 1))/dim(pt2)[1]*100


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

set <- c("q55g", "q55d","q55c4" )
table(dta[set])

dta[set] <- lapply(dta[set],  function(x) replace(x,x=="March",3))
dta[set] <- lapply(dta[set],  function(x) replace(x,x=="April",4))
dta[set] <- lapply(dta[set],  function(x) replace(x,x=="May",5))
dta[set] <- lapply(dta[set],  function(x) replace(x,x=="June",6))
dta[set] <- lapply(dta[set],  function(x) replace(x,x=="July",7))
dta[set] <- lapply(dta[set],  function(x) as.numeric(as.character(x)))
dta$prod_maize_month <- rowMeans(dta[set], na.rm=T)

current_prod_maize <- data.frame(tapply(dta$prod_maize, dta$prod_maize_month, sum, na.rm=T))
current_prod_maize$month <- rownames(current_prod_maize)
current_prod_maize$crop <- "maize" 
names(current_prod_maize) <- c("prod","month","crop") 
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

set <- c("q59g", "q59d","q59c4" )
table(dta[set])

dta[set] <- lapply(dta[set],  function(x) replace(x,x=="March",3))
dta[set] <- lapply(dta[set],  function(x) replace(x,x=="April",4))
dta[set] <- lapply(dta[set],  function(x) replace(x,x=="May",5))
dta[set] <- lapply(dta[set],  function(x) replace(x,x=="June",6))
dta[set] <- lapply(dta[set],  function(x) replace(x,x=="July",7))
dta[set] <- lapply(dta[set],  function(x) as.numeric(as.character(x)))
dta$prod_gnuts_month <- rowMeans(dta[set], na.rm=T)

current_prod_gnuts <- data.frame(tapply(dta$prod_gnuts, dta$prod_gnuts_month, sum, na.rm=T))
current_prod_gnuts$month <- rownames(current_prod_gnuts)
current_prod_gnuts$crop <- "gnuts" 
names(current_prod_gnuts) <- c("prod","month","crop") 
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

set <- c("q63g", "q63d","q63c4" )
table(dta[set])

dta[set] <- lapply(dta[set],  function(x) replace(x,x=="March",3))
dta[set] <- lapply(dta[set],  function(x) replace(x,x=="April",4))
dta[set] <- lapply(dta[set],  function(x) replace(x,x=="May",5))
dta[set] <- lapply(dta[set],  function(x) replace(x,x=="June",6))
dta[set] <- lapply(dta[set],  function(x) replace(x,x=="July",7))
dta[set] <- lapply(dta[set],  function(x) as.numeric(as.character(x)))
dta$prod_soy_month <- rowMeans(dta[set], na.rm=T)

current_prod_soy <- data.frame(tapply(dta$prod_soy, dta$prod_soy_month, sum, na.rm=T))
current_prod_soy$month <- rownames(current_prod_soy)
current_prod_soy$crop <- "soy" 
names(current_prod_soy) <- c("prod","month","crop") 

##stack
current_prod_all <- rbind(current_prod_maize, current_prod_gnuts, current_prod_soy)
current_prod_all$month_num <- current_prod_all$month
current_prod_all$month <- NA
current_prod_all$month[current_prod_all$month_num==3] <- "March"
current_prod_all$month[current_prod_all$month_num==4] <- "April"
current_prod_all$month[current_prod_all$month_num==5] <- "May"
current_prod_all$month[current_prod_all$month_num==6] <- "June"
current_prod_all$month[current_prod_all$month_num==7] <- "July"


current_prod_all$month <-  factor(current_prod_all$month, levels=month.name)
# Plot
## quantities    
plot_1 <- ggplot(current_prod_all[current_prod_all$month%in%c("March","April","May","June","July"),], aes(fill=crop, y=prod, x=month)) + 
    geom_bar(position="stack", stat="identity")
current_prod_all$price[current_prod_all$crop == "gnuts"] <- 720
current_prod_all$price[current_prod_all$crop == "maize"] <- 220
current_prod_all$price[current_prod_all$crop == "soy"] <- 650

current_prod_all$value <- current_prod_all$price*current_prod_all$prod

plot_2 <- ggplot(current_prod_all[current_prod_all$month%in%c("March","April","May","June","July"),], aes(fill=crop, y=value, x=month)) + 
  geom_bar(position="stack", stat="identity")

png(paste(path,"papers/price_expectations/results/fig_current_prod.png",sep = ""), units="px", height=3200, width= 3800, res=600)
ggarrange(plot_1, plot_2, heights = c(2, 2,2), ncol = 1, nrow = 2, align = "v")
dev.off()

### income from cash cropping


### analyis
all_transactions$quant_kg <- NA
all_transactions$measure[is.na(all_transactions$measure)] <- "XX"
all_transactions[(all_transactions$crop %in% c("maize","soy")) & all_transactions$measure=="Bags_50kg",]$quant_kg <- all_transactions[(all_transactions$crop %in% c("maize","soy")) & all_transactions$measure=="Bags_50kg",]$quant*50
all_transactions[(all_transactions$crop=="gnuts") & all_transactions$measure=="Bags_50kg",]$quant_kg <- all_transactions[(all_transactions$crop=="gnuts") & all_transactions$measure=="Bags_50kg",]$quant*13
all_transactions[all_transactions$measure=="kg",]$quant_kg <- all_transactions[ all_transactions$measure=="kg",]$quant

all_transactions[(all_transactions$crop=="gnuts") & all_transactions$measure=="Debbe_Ndowa",]$quant_kg <- all_transactions[(all_transactions$crop=="gnuts") & all_transactions$measure=="Debbe_Ndowa",]$quant*5
all_transactions[(all_transactions$crop=="soy") & all_transactions$measure=="Debbe_Ndowa",]$quant_kg <- all_transactions[(all_transactions$crop=="soy") & all_transactions$measure=="Debbe_Ndowa",]$quant*20

all_transactions[(all_transactions$crop=="soy") & all_transactions$measure=="OX cart",]$quant_kg <- all_transactions[(all_transactions$crop=="soy") & all_transactions$measure=="OX cart",]$quant*500
all_transactions$revenue <- all_transactions$price * all_transactions$quant_kg 
all_transactions$revenue[all_transactions$revenue>1000000 ] <- NA

revenue_maize <- aggregate(all_transactions[all_transactions$crop == "maize",]$revenue,list(all_transactions[all_transactions$crop=="maize",]$date),FUN=median, na.rm=T)
revenue_gnuts <- aggregate(all_transactions[all_transactions$crop == "gnuts",]$revenue,list(all_transactions[all_transactions$crop=="gnuts",]$date),FUN=median, na.rm=T)
revenue_soy <- aggregate(all_transactions[all_transactions$crop == "soy",]$revenue,list(all_transactions[all_transactions$crop=="soy",]$date),FUN=median, na.rm=T)

names(revenue_maize) <- c("date","revenue")
names(revenue_soy) <- c("date","revenue")
names(revenue_gnuts) <- c("date","revenue")

revenue_maize$crop <- "maize"
revenue_soy$crop <- "soy"
revenue_gnuts$crop <- "gnuts"

revenue_all <- rbind(revenue_maize,revenue_gnuts,revenue_soy)
revenue_all$month <- NA
revenue_all$month[revenue_all$date=="2021-04-01"] <- "Apr 21"
revenue_all$month[revenue_all$date=="2021-05-01"] <- "May 21"
revenue_all$month[revenue_all$date=="2021-06-01"] <- "Jun 21"
revenue_all$month[revenue_all$date=="2021-07-01"] <- "Jul 21"
revenue_all$month[revenue_all$date=="2021-08-01"] <- "Aug 21"
revenue_all$month[revenue_all$date=="2021-09-01"] <- "Sep 21"
revenue_all$month[revenue_all$date=="2021-10-01"] <- "Oct 21"
revenue_all$month[revenue_all$date=="2021-11-01"] <- "Nov 21"
revenue_all$month[revenue_all$date=="2021-12-01"] <- "Dec 21"
revenue_all$month[revenue_all$date=="2022-01-01"] <- "Jan 22"
revenue_all$month[revenue_all$date=="2022-02-01"] <- "Feb 22"
revenue_all$month[revenue_all$date=="2022-03-01"] <- "Mar 22"
revenue_all_plot  <-revenue_all[!is.na(revenue_all$month),]
revenue_all_plot$month <- factor(revenue_all_plot$month, levels=c("Apr 21","May 21","Jun 21","Jul 21","Aug 21","Sep 21","Oct 21","Nov 21","Dec 21","Jan 22","Feb 22","Mar 22"))

plot_res_2 <- ggplot(revenue_all_plot, aes(fill=crop, y=revenue, x=month)) + 
    geom_bar(position="stack", stat="identity")
    
quant_kg_maize <- aggregate(all_transactions[all_transactions$crop == "maize",]$quant_kg,list(all_transactions[all_transactions$crop=="maize",]$date),FUN=median, na.rm=T)
quant_kg_gnuts <- aggregate(all_transactions[all_transactions$crop == "gnuts",]$quant_kg,list(all_transactions[all_transactions$crop=="gnuts",]$date),FUN=median, na.rm=T)
quant_kg_soy <- aggregate(all_transactions[all_transactions$crop == "soy",]$quant_kg,list(all_transactions[all_transactions$crop=="soy",]$date),FUN=median, na.rm=T)

names(quant_kg_maize) <- c("date","quant_kg")
names(quant_kg_gnuts) <- c("date","quant_kg")
names(quant_kg_soy) <- c("date","quant_kg")

quant_kg_maize$crop <- "maize"
quant_kg_soy$crop <- "soy"
quant_kg_gnuts$crop <- "gnuts"

quant_kg_all <- rbind(quant_kg_maize,quant_kg_soy,quant_kg_gnuts)
quant_kg_all$month <- NA
quant_kg_all$month[quant_kg_all$date=="2021-04-01"] <- "Apr 21"
quant_kg_all$month[quant_kg_all$date=="2021-05-01"] <- "May 21"
quant_kg_all$month[quant_kg_all$date=="2021-06-01"] <- "Jun 21"
quant_kg_all$month[quant_kg_all$date=="2021-07-01"] <- "Jul 21"
quant_kg_all$month[quant_kg_all$date=="2021-08-01"] <- "Aug 21"
quant_kg_all$month[quant_kg_all$date=="2021-09-01"] <- "Sep 21"
quant_kg_all$month[quant_kg_all$date=="2021-10-01"] <- "Oct 21"
quant_kg_all$month[quant_kg_all$date=="2021-11-01"] <- "Nov 21"
quant_kg_all$month[quant_kg_all$date=="2021-12-01"] <- "Dec 21"
quant_kg_all$month[quant_kg_all$date=="2022-01-01"] <- "Jan 22"
quant_kg_all$month[quant_kg_all$date=="2022-02-01"] <- "Feb 22"
quant_kg_all$month[quant_kg_all$date=="2022-03-01"] <- "Mar 22"
quant_kg_all_plot  <-quant_kg_all[!is.na(quant_kg_all$month),]
quant_kg_all_plot$month <- factor(quant_kg_all_plot$month, levels=c("Apr 21","May 21","Jun 21","Jul 21","Aug 21","Sep 21","Oct 21","Nov 21","Dec 21","Jan 22","Feb 22","Mar 22"))

plot_res_1 <- ggplot(quant_kg_all_plot, aes(fill=crop, y=quant_kg, x=month)) + 
    geom_bar(position="stack", stat="identity")

png(paste(path,"papers/price_expectations/results/fig_revenue.png",sep = ""), units="px", height=3200, width= 3800, res=600)
ggarrange(plot_res_1, plot_res_2, heights = c(2, 2,2), ncol = 1, nrow = 2, align = "v")
dev.off()




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

