#run in /home/bjvca/data/projects/malawi/crowdsourcing/papers/market_integration
#rm(list=ls())
library(ggplot2)
library(tseries)
library(reshape2)
library(leaflet)
#library(stringr)
#set.seed(28052021)

path <- getwd()
path <- strsplit(path, "/crowdsourcing/papers/market_integration")[[1]]

#this is only for 2020
dta_cs <- read.csv(paste(path,"crowdsourcing/data/public/crowd_sourced_all.csv", sep="/"), stringsAsFactors = FALSE)
#analysis only for maize for now
dta_cs <- subset(dta_cs, crop =="maize")

## this is for 2021
dta_cs_21 <- read.csv(paste(path,"crowdsourcing/data/raw/data_21.csv", sep="/"), stringsAsFactors = FALSE)
#analysis only for maize for now
dta_cs_21 <- subset(dta_cs_21, crop =="Maize")


#get market price data
dta_mkt <- read.csv(paste(path,"prices/data/latest_prices.csv", sep="/"), stringsAsFactors = FALSE)

## start at highest level of aggregation - aggregated all market data (overall average) and all farm gate prices 

dta_mkt$mean_wholesale_price <- rowMeans(dta_mkt[,2:17], na.rm=T)


# now match on data
dta_cs$date <- substr(dta_cs$Timestamp, 1, 9)
dta_cs_21$date <- substr(dta_cs_21$date, 1, 9)

agg_prices <- aggregate(dta_cs$Price, list(dta_cs$date), FUN=mean)
names(agg_prices) <- c("date","price")

agg_prices_21 <- aggregate(dta_cs_21$price, list(dta_cs_21$date), FUN=mean)
names(agg_prices_21) <- c("date","price")
agg_prices <- rbind(agg_prices, agg_prices_21)

library(lubridate) 

dta_mkt$date <- dmy(dta_mkt$Date)
agg_prices$date <- dmy(agg_prices$date)

dta <- merge(dta_mkt[c("date","Mzuzu")], agg_prices[c("date","price")],by="date") ################################## no market price data for 2021???
names(dta) <- c("date","market","farmgate")

meltdf <- melt(dta,id="date")

png(paste(path,"crowdsourcing/papers/market_integration/figures/fig1.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + geom_line()
dev.off()

## unit root tests on time series, let us just ignore missing values for now
adf.test(dta$market,k=1)
adf.test(dta$farmgate,k=1)

dta$margin <- dta$market - dta$farmgate
adf.test(dta$margin,k=1)
## simple error correction model
## regress change on lag

dta$dmargin <- NA
dta$dmargin[1:(dim(dta)[1]-1)] <- diff(dta$margin)

summary(lm(dmargin~margin-1,dta))

### now look at integration between markets - use blantyre as the reference market
##get gps coordinates of market
coords <- read.csv(paste(path,"prices/data/maize_markets_coordinates.csv", sep="/"), stringsAsFactors = FALSE)

m <-  leaflet() %>% setView(lat = -14.3, lng = 34.5, zoom=7)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=coords, lng=~as.numeric(as.character(Long)), lat=~as.numeric(as.character(Lat)),radius= 8, popup = ~as.character(Market))   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 



#do an analysis for Mzuzu
#Rumpi
dta_mkt_sel <- dta_mkt[c("date","Mzuzu","Rumphi")]
meltdf <- melt(dta_mkt_sel,id="date")

png(paste(path,"crowdsourcing/papers/market_integration/figures/fig2.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + geom_line()
dev.off()


## unit root tests on time series, let us just ignore missing values for now
adf.test(dta_mkt_sel$Mzuzu,k=1)
adf.test(dta_mkt_sel$Rumphi,k=1)

dta_mkt_sel$margin <- dta_mkt_sel$Mzuzu - dta_mkt_sel$Rumphi
adf.test(dta_mkt_sel$margin,k=1)
## simple error correction model
## regress change on lag

dta_mkt_sel$dmargin <- NA
dta_mkt_sel$dmargin[1:(dim(dta_mkt_sel)[1]-1)] <- diff(dta_mkt_sel$margin)

summary(lm(dmargin~margin-1,dta_mkt_sel))

#do an analysis for Mzuzu
#Rumpi
dta_mkt_sel <- dta_mkt[c("date","Mzuzu","Karonga")]
meltdf <- melt(dta_mkt_sel,id="date")

png(paste(path,"crowdsourcing/papers/market_integration/figures/fig3.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + geom_line()
dev.off()


## unit root tests on time series, let us just ignore missing values for now
adf.test(dta_mkt_sel$Mzuzu,k=1)
adf.test(dta_mkt_sel$Karonga,k=1)

dta_mkt_sel$margin <- dta_mkt_sel$Mzuzu - dta_mkt_sel$Karonga
adf.test(dta_mkt_sel$margin,k=1)
## simple error correction model
## regress change on lag

dta_mkt_sel$dmargin <- NA
dta_mkt_sel$dmargin[1:(dim(dta_mkt_sel)[1]-1)] <- diff(dta_mkt_sel$margin)

summary(lm(dmargin~margin-1,dta_mkt_sel))

#do an analysis for Mzuzu Chitipa
#Rumpi
dta_mkt_sel <- dta_mkt[c("date","Mzuzu","Chitipa")]
meltdf <- melt(dta_mkt_sel,id="date")

png(paste(path,"crowdsourcing/papers/market_integration/figures/fig4.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + geom_line()
dev.off()


## unit root tests on time series, let us just ignore missing values for now
adf.test(dta_mkt_sel$Mzuzu,k=1)
adf.test(dta_mkt_sel$Chitipa,k=1)

dta_mkt_sel$margin <- dta_mkt_sel$Mzuzu - dta_mkt_sel$Chitipa
adf.test(dta_mkt_sel$margin,k=1)
## simple error correction model
## regress change on lag

dta_mkt_sel$dmargin <- NA
dta_mkt_sel$dmargin[1:(dim(dta_mkt_sel)[1]-1)] <- diff(dta_mkt_sel$margin)

summary(lm(dmargin~margin-1,dta_mkt_sel))

#### not enough observations... crowd sourced data is stationary... use also data for this season
### trial 1 - 




