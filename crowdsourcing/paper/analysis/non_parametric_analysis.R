#run in crowdsourcing/paper/analysis

rm(list=ls())
library(ggplot2)

library(KernSmooth)
set.seed(28052021)

path <- getwd()
path <- strsplit(path, "/paper/analysis")[[1]]

dta <- read.csv(paste(path,"data/public/crowd_sourced_all.csv", sep="/"), stringsAsFactors = FALSE)

table(dta$Location)
dta$Location[dta$Location == "0. Farmgate"] <- "Farmgate"
dta$Location[dta$Location == "1. Market"] <- "Market"

# keep only farmgate and market
dta <- dta[dta$Location %in% c("Farmgate","Market"),]

dta$Price[dta$Price > 400] <- NA
dta$Price[(dta$Price > 300 & dta$crop=="maize")] <- NA
#cut at 95 percentile
dta$Quantity[dta$Quantity > 2000] <- NA


tapply(dta$Price[dta$crop=="soy_bean"],dta$Location[dta$crop=="soy_bean"], FUN=mean)
tapply(dta$Price[dta$crop=="maize"],dta$Location[dta$crop=="maize"], FUN=mean)
t.test(dta$Price[dta$crop=="soy_bean"]~dta$Location[dta$crop=="soy_bean"])
t.test(dta$Price[dta$crop=="maize"]~dta$Location[dta$crop=="maize"])
#we only find a market premium for soja

png(paste(path,"paper/figures/fig1.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
pl <- ggplot(dta, aes(x=Quantity, y=as.numeric(Location=="Market"), color = crop)) + 
        geom_smooth(method="loess") +
        coord_cartesian(
  xlim =c(100,1500), ylim=c(.2,.7))

print(pl)
dev.off()

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

png(paste(path,"paper/figures/fig2.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
pl <- ggplot(dta, aes(x=Time2Market, y=as.numeric(Location=="Market"), color = crop)) + 
        geom_smooth(method="loess") +
        coord_cartesian(
  xlim =c(0,120), ylim=c(.25,.7))
  print(pl)
dev.off()
