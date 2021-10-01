#run in /home/bjvca/data/projects/malawi/crowdsourcing/papers/market_integration
#rm(list=ls())
library(ggplot2)
library(tseries)
library(reshape2)
library(leaflet)
library(lubridate) 
library(gridExtra)

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

#start with simple MI TS analysis
#do an analysis for Mzuzu area
#graph of closest and furthers markets

dta_mkt$date <- dmy(dta_mkt$Date)

dta_mkt_sel <- dta_mkt[c("date","Mzuzu","Rumphi","Chitipa")]
meltdf <- melt(dta_mkt_sel,id="date")

png(paste(path,"crowdsourcing/papers/market_integration/figures/fig1.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
top_fig <- ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + geom_line()


### this is cool but also create on with differences below
dta_mkt$m_Rumphi <- dta_mkt$Mzuzu - dta_mkt$Rumphi  
dta_mkt$m_Chitipa <- dta_mkt$Mzuzu - dta_mkt$Chitipa
dta_mkt_sel <- dta_mkt[c("date","m_Rumphi","m_Chitipa")]
names(dta_mkt_sel) <- c("date","Rumphi","Chitipa")
meltdf <- melt(dta_mkt_sel,id="date")


botom_fig <- ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + geom_line() + geom_hline(yintercept = 0) 
grid.arrange(top_fig, botom_fig, ncol = 1, nrow = 2, heights=c(4,1))
dev.off()

res_n <- matrix(NA, 5,4)

j <-1
for (i in c("Rumphi","Chitipa","Karonga", "Mzimba","Jenda")) {
dta_mkt$margin <- dta_mkt$Mzuzu - dta_mkt[i]


###
dep <- ts(dta_mkt$margin,start= c(2000,1), frequency = 12)
ldep <- lag(dep,-1)
ddep<- diff(dep)
all <- cbind(dep,ldep,ddep)
res_n[j,1] <- format(round(summary(lm(all[,3]~all[,2]-1))$coefficients[1],digits=3),nsmall=3)
res_n[j,2] <- format(round(log(0.5)/log(1+summary(lm(all[,3]~all[,2]-1))$coefficients[1]),digits=0),nsmall=0)
res_n[j,3] <- format(round(summary(lm(all[,3]~all[,2]-1))$coefficients[4],digits=3),nsmall=3)
res_n[j,4] <- nobs(lm(all[,3]~all[,2]-1))

j <- j+1
}

res_c <- matrix(NA, 4,4)

j <-1
for (i in c("Mchinji","Mitundu","Chimbiya","Salima")) {
dta_mkt$margin <- dta_mkt$Nsungwi - dta_mkt[i]


###
dep <- ts(dta_mkt$margin,start= c(2000,1), frequency = 12)
ldep <- lag(dep,-1)
ddep<- diff(dep)
all <- cbind(dep,ldep,ddep)
res_c[j,1] <- format(round(summary(lm(all[,3]~all[,2]-1))$coefficients[1],digits=3),nsmall=3)
res_c[j,2] <- format(round(log(0.5)/log(1+summary(lm(all[,3]~all[,2]-1))$coefficients[1]),digits=0),nsmall=0)
res_c[j,3] <- format(round(summary(lm(all[,3]~all[,2]-1))$coefficients[4],digits=3),nsmall=3)
res_c[j,4] <- nobs(lm(all[,3]~all[,2]-1))

j <- j+1
}

j <-1

res_s <- matrix(NA, 9,4)

for (i in c("Lunzu","Luchenza","Mulanje", "Mpondabwino","Chiringa","Chikwawa","Ngabu","Bangula","Nsanje")) {
dta_mkt$margin <- dta_mkt$Mbayani - dta_mkt[i]


###
dep <- ts(dta_mkt$margin,start= c(2000,1), frequency = 12)
ldep <- lag(dep,-1)
ddep<- diff(dep)
all <- cbind(dep,ldep,ddep)
res_s[j,1] <- format(round(summary(lm(all[,3]~all[,2]-1))$coefficients[1],digits=3),nsmall=3)
res_s[j,2] <- format(round(log(0.5)/log(1+summary(lm(all[,3]~all[,2]-1))$coefficients[1]),digits=0),nsmall=0)
res_s[j,3] <- format(round(summary(lm(all[,3]~all[,2]-1))$coefficients[4],digits=3),nsmall=3)
res_s[j,4] <- nobs(lm(all[,3]~all[,2]-1))

j <- j+1
}

## start at highest level of aggregation - aggregated all market data (overall average) and all farm gate prices 

dta_mkt$mean_wholesale_price <- rowMeans(dta_mkt[,2:27], na.rm=T)


# now match on data
dta_cs$date <- substr(dta_cs$Timestamp, 1, 9)
dta_cs_21$date <- substr(dta_cs_21$date, 1, 9)

agg_prices <- aggregate(dta_cs$Price, list(dta_cs$date), FUN=mean)
names(agg_prices) <- c("date","price")

agg_prices_21 <- aggregate(dta_cs_21$price, list(dta_cs_21$date), FUN=mean)
names(agg_prices_21) <- c("date","price")
agg_prices <- rbind(agg_prices, agg_prices_21)


agg_prices$date <- dmy(agg_prices$date)

dta <- merge(dta_mkt[c("date","mean_wholesale_price")], agg_prices[c("date","price")],by="date")
names(dta) <- c("date","market","farmgate")

meltdf <- melt(dta,id="date")

png(paste(path,"crowdsourcing/papers/market_integration/figures/fig_1.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + geom_line()
dev.off()


#dta$index <- 1:nrow(dta)  # create index variable
#loessMod10 <- loess(farmgate ~ index, data=dta, span=0.35) # 10% smoothing span
#dta$farmgate <- predict(loessMod10)

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

dta_mkt <- dta_mkt[1:1000,]

#start by running simple unit root tests 
for (i in c("Mzuzu","Rumphi")) {
###
dep <- ts(dta_mkt[i],start= c(2000,1), frequency = 12)
ldep <- lag(dep,-1)
ddep<- diff(dep)
all <- cbind(dep,ldep,ddep)
summary(lm(all[,3]~all[,2]))
}
adf.test(dta_mkt$Mzuzu,k=1)


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

#### this is much slower than integration of farmgate prices to markets!
### H0: this is due to measurement error - to test, add random noise to 
noise <- rnorm(dim(dta_mkt_sel)[1],0,.25)
dta_mkt_sel$Mzuzu <- dta_mkt_sel$Mzuzu*(1+noise)

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


















##do an analysis for Mzuzu
##Rumpi
#dta_mkt_sel <- dta_mkt[c("date","Mzuzu","Rumphi","Chitipa")]
#meltdf <- melt(dta_mkt_sel,id="date")

#png(paste(path,"crowdsourcing/papers/market_integration/figures/fig3.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
#ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + geom_line()
#dev.off()


### unit root tests on time series, let us just ignore missing values for now
#adf.test(dta_mkt_sel$Mzuzu,k=1)
#adf.test(dta_mkt_sel$Karonga,k=1)

#dta_mkt_sel$margin <- dta_mkt_sel$Mzuzu - dta_mkt_sel$Karonga
#adf.test(dta_mkt_sel$margin,k=1)
### simple error correction model
### regress change on lag

#dta_mkt_sel$dmargin <- NA
#dta_mkt_sel$dmargin[1:(dim(dta_mkt_sel)[1]-1)] <- diff(dta_mkt_sel$margin)

#summary(lm(dmargin~margin-1,dta_mkt_sel))

##do an analysis for Mzuzu Chitipa
##Rumpi
#dta_mkt_sel <- dta_mkt[c("date","Mzuzu","Chitipa")]
#meltdf <- melt(dta_mkt_sel,id="date")

#png(paste(path,"crowdsourcing/papers/market_integration/figures/fig4.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
#ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + geom_line()
#dev.off()


### unit root tests on time series, let us just ignore missing values for now
#adf.test(dta_mkt_sel$Mzuzu,k=1)
#adf.test(dta_mkt_sel$Chitipa,k=1)

#dta_mkt_sel$margin <- dta_mkt_sel$Mzuzu - dta_mkt_sel$Chitipa
#adf.test(dta_mkt_sel$margin,k=1)
### simple error correction model
### regress change on lag

#dta_mkt_sel$dmargin <- NA
#dta_mkt_sel$dmargin[1:(dim(dta_mkt_sel)[1]-1)] <- diff(dta_mkt_sel$margin)

#summary(lm(dmargin~margin-1,dta_mkt_sel))

##### not enough observations... crowd sourced data is stationary... use also data for this season
#### trial 1 - 




