#run in crowdsourcing/paper/
rm(list=ls())
library(ggplot2)
library(reshape2)
library(KernSmooth)
set.seed(28052021)

path <- getwd()
path <- strsplit(path, "/paper")[[1]]


dta <- read.csv(paste(path,"data/public/crowd_sourced_all_baseline.csv", sep="/"), stringsAsFactors = FALSE)

dta$pricesold[dta$pricesold > 400] <- NA
dta$pricesold[(dta$pricesold > 300 & dta$crop=="maize")] <- NA
#cut at 95 percentile
dta$quantitysold[dta$quantitysold > 2000] <- NA

### time to market - recast in minutes

dta$Time2Market[dta$Time2Market %in% c(" 1 hour","1", "1 hour","1 Hour", "1 HOUR","1 hour ","1 Hour ", "1hour", "one hour")] <- "60"  

dta$Time2Market[dta$Time2Market %in% c( "2  hours" ,"2 hour" , "2 hour " , "2 hours"  ,"2 HOURS"  ,"two hours", "2hours" )] <- "120"  

dta$Time2Market[dta$Time2Market %in% c("1 hour 30 minutes", "1 houir & 30 minutes","1 hour & 30 minutes"  , "1hour 30minutes"  ,    "1hour30minutes" ,   "1 hour & half","1 hour and  30 minutes", "1 hour and 30 minutes" ,"90 minutes" )] <- "90" 
dta$Time2Market[dta$Time2Market %in% c( "5 minutes" ,"5minutes")] <- "5"   
dta$Time2Market[dta$Time2Market %in% c( "10 minutes" ,"10minutes")] <- "10"  
dta$Time2Market[dta$Time2Market %in% c( "15 minutes" ,"15minutes")] <- "15"
dta$Time2Market[dta$Time2Market %in% c( "20 minutes" ,"20minutes")] <- "20"  
dta$Time2Market[dta$Time2Market %in% c( "25 minutes" ,"25minutes")] <- "25"  
dta$Time2Market[dta$Time2Market %in% c( "35 minutes" ,"35minutes")] <- "35"  
dta$Time2Market[dta$Time2Market %in% c( "40 minutes" , "40 MINUTES" ,"40minutes")] <- "40" 
dta$Time2Market[dta$Time2Market %in% c( "45 minutes" ,"45minutes")] <- "45" 
dta$Time2Market[dta$Time2Market %in% c( "50 minutes" ,"50minutes")] <- "50" 
dta$Time2Market[dta$Time2Market %in% c( "115 minutes")] <- "115" 
dta$Time2Market[dta$Time2Market %in% c( "1hour 10minutes")] <- "70"
dta$Time2Market[dta$Time2Market %in% c( "1 hour & 45 minutes")] <- "105"
dta$Time2Market[dta$Time2Market %in% c( "2 hours & 30 minutes", "2 hours & half", "2hours30minutes")] <- "150"
dta$Time2Market[dta$Time2Market %in% c( "4 hour","4 hours" )] <- "240"  
dta$Time2Market[dta$Time2Market %in% c( "4 hour","4 hours" )] <- "180"   
 

dta$Time2Market[dta$Time2Market %in% c("30  minutes" ,"30 minutes","30 Minutes","30 MINUTES" , "30minutes", "30MINUTES"    )] <- "30"       


dta$Time2Market <- as.numeric(dta$Time2Market)

dta$loc_sold <- NA
dta$loc_sold[dta$market] <- "market" 
dta$loc_sold[dta$admarc] <- "admarc" 
dta$loc_sold[dta$farmgate] <- "farmgate" 

############## start analysis



png(paste(path,"paper/figures/fig1.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
pl <- ggplot(dta, aes(x=quantitysold, y=as.numeric(farmgate), color = crop)) + 
        geom_smooth(method="loess") +
        coord_cartesian(
  xlim =c(100,1500), ylim=c(.2,.7))

print(pl)
dev.off()



png(paste(path,"paper/figures/fig2.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
pl <- ggplot(dta, aes(x=Time2Market, y=as.numeric(farmgate), color = crop)) + 
        geom_smooth(method="loess") +
        coord_cartesian(
  xlim =c(0,120), ylim=c(.25,.6))
  print(pl)
dev.off()


#how does competition affect location of sales?

png(paste(path,"paper/figures/fig3.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
pl <- ggplot(dta, aes(x=NumBuyers, y=as.numeric(farmgate), color = crop)) + 
        geom_smooth(method="loess") +
        coord_cartesian(
  xlim =c(0,5), ylim=c(.25,.6))
  print(pl)
dev.off()


#we do not have wealth, but what is we use farm size as an indicator of wealth?
#we get the reverse of what Fafchamps and Hill find... (fig 1)
png(paste(path,"paper/figures/fig4.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
pl <- ggplot(dta, aes(x=farmsize_acres, y=as.numeric(farmgate), color = crop)) + 
        geom_smooth(method="loess") +
        coord_cartesian(
  xlim =c(0,5), ylim=c(.25,.6))
  print(pl)
dev.off()

png(paste(path,"paper/figures/fig5.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
pl <- ggplot(dta, aes(x=crop_acres, y=as.numeric(farmgate), color = crop)) + 
        geom_smooth(method="loess") +
        coord_cartesian(
  xlim =c(0,3), ylim=c(.25,.6))
  print(pl)
dev.off()
## fig 2 in Fafchamps and Hill
png(paste(path,"paper/figures/fig6.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
pl <- ggplot(dta, aes(x=farmsize_acres, y=Time2Market, color = crop)) + 
        geom_smooth(method="loess") +
        coord_cartesian(
  xlim =c(0,5), ylim=c(20,75))
  print(pl)
dev.off()
