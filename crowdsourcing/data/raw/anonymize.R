#run in crowdsourcing/data/raw/

rm(list=ls())
path <- getwd()
path <- strsplit(path, "/raw")[[1]]

dta <- read.csv("soja_data.csv")
dta2 <- read.csv("maize_data.csv")



dta2[c("TAname","Date","Time")] <- NULL 
dta$crop <- "soy_bean"
dta2$crop <- "maize"



b_dta <- read.csv("soja_baseline.csv")

b_dta2 <- read.csv("maize_baseline.csv")

b_dta$crop <- "soy_bean"
b_dta2$crop <- "maize"


names(b_dta2)[names(b_dta2) == 'QuantitySoldinKilogram'] <- 'quantitysold'


### merge in time to market and timestampt
b_dta <- merge(b_dta,dta[c("PrimaryKey","Time2Market", "Timestamp")])
b_dta2 <- merge(b_dta2,dta2[c("PrimaryKey","Time2Market","Timestamp")])


#define farmgate sales
b_dta$farmgate <- FALSE
b_dta$farmgate[b_dta$loc2 == "0. Farm" & b_dta$CategoryofBuyer == "Other traders"] <- TRUE

b_dta2$farmgate <- FALSE
b_dta2$farmgate[b_dta2$LocationofTransaction == "Farmgate" & b_dta2$CategoryofBuyer == "Other traders"] <- TRUE


#define market sales
b_dta$market <- FALSE
b_dta$market[((b_dta$loc2 %in% c("1. Market","2. Trading center")) & b_dta$CategoryofBuyer == "Other traders")] <- TRUE

b_dta2$market <- FALSE
b_dta2$market[(b_dta2$LocationofTransaction == "Market" & b_dta2$CategoryofBuyer == "Other traders")] <- TRUE

#define ADMARC sales
b_dta$admarc <- FALSE
b_dta$admarc[b_dta$CategoryofBuyer == "ADMARC"] <- TRUE

b_dta2$admarc <- FALSE
b_dta2$admarc[b_dta2$CategoryofBuyer == "ADMARC"] <- TRUE

names(b_dta)[names(b_dta) =='numbuyers'] <- "competition"
names(b_dta2)[names(b_dta2) =='Howmanybuyershaveapproached'] <- "competition"


b_dta <- rbind(b_dta[c("crop", "pricesold","quantitysold","Time2Market","farmsize_acres", "crop_acres", "distance_km", "admarc","farmgate","market","competition","Timestamp")],b_dta2[c("crop", "pricesold","quantitysold","Time2Market","farmsize_acres", "crop_acres","distance_km", "admarc","farmgate","market","competition","Timestamp")])

write.csv(b_dta,file = paste(path,"public/crowd_sourced_all_baseline.csv", sep="/"), row.names=FALSE)

dta <- rbind(dta,dta2)
dta[c("EPA","GVH")] <- NULL

write.csv(dta,file = paste(path,"public/crowd_sourced_all.csv", sep="/"), row.names=FALSE)
