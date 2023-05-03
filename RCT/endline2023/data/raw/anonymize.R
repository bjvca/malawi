rm(list=ls())
library(leaflet)
library(reshape2)
library(htmlwidgets)
library(pracma) # for haversine function
set.seed(20042023)  #today's date

path <- getwd()
dta <- read.csv("latest.csv")
path <- strsplit(path,"endline2023/data/raw")[[1]]

dta_base <-
rbind(read.csv(paste(path,"baseline/data/raw/latest_form1.csv", sep="/"))[c("farmer_ID","check.check2.maize._gps_longitude","check.check2.maize._gps_latitude")],
read.csv(paste(path,"baseline/data/raw/latest_form2.csv", sep="/"))[c("farmer_ID","check.check2.maize._gps_longitude","check.check2.maize._gps_latitude")])


## three versions of the form were used. In the first change, this was just a change to the limits to allow enumerators to enter
## zero. The second change is more fundamental when we changed all measures for gnuts from kgs to debe
## this needs to be coded - we can use the variable X_version which is a unique ID for the XLS_form that was used
table(dta$X_version,dta$today)

set <- c("check.maize.G_exp1", "check.maize.G_exp2","check.maize.G_exp3","check.maize.G_exp4")
dta[set] <- lapply(dta[set],  function(x) as.numeric(as.character(x)))

dta[set] <- lapply(dta[set],  function(x) ifelse(x==999,NA,x))

dta$check.maize.G_exp1[dta$X_version %in% c("202304121045","202304170715")] <-  dta$check.maize.G_exp1[dta$X_version %in% c("202304121045","202304170715")] *7.4
dta$check.maize.G_exp2[dta$X_version %in% c("202304121045","202304170715")] <-  dta$check.maize.G_exp2[dta$X_version %in% c("202304121045","202304170715")] *7.4
dta$check.maize.G_exp3[dta$X_version %in% c("202304121045","202304170715")] <-  dta$check.maize.G_exp3[dta$X_version %in% c("202304121045","202304170715")] *7.4
dta$check.maize.G_exp4[dta$X_version %in% c("202304121045","202304170715")] <-  dta$check.maize.G_exp4[dta$X_version %in% c("202304121045","202304170715")] *7.4

### issues with missing farmer IDs - save those that are controls in second part

dta$farmer_ID[dta$X_uuid =="82ad25af-1970-4a09-8934-4030aef73c1b"] <- "F_1352"
dta$farmer_ID[dta$X_uuid =="34e4013e-66a7-43f6-b041-a965247a1b05"] <- "F_1340"
dta$farmer_ID[dta$X_uuid =="a7f12052-8a2e-4ab5-8de3-41d598758f9f"] <- "F_1349"
dta$farmer_ID[dta$X_uuid =="3162a5d9-58ca-4eb4-86a2-b0262f926229"] <- "F_1284"
dta$farmer_ID[dta$X_uuid =="bfa56209-bea5-4971-b630-f9d465ed8048"] <- "F_1413"
dta$farmer_ID[dta$X_uuid =="a4d4f16d-8ba7-4596-abb1-c653ddbe9429"] <- "F_1274"
dta$farmer_ID[dta$X_uuid =="d85f3ee0-993b-49cc-8070-24cc3315dcd7"] <- "F_1356"
dta$farmer_ID[dta$X_uuid =="2a408d7c-a4aa-4242-96a6-a82fa39f583d"] <- "F_1343"
dta$farmer_ID[dta$X_uuid =="51117b46-0ec2-4f14-b987-dc365f86f7fc"] <- "F_1399"

done <- names(table(dta$farmer_ID))
#progress
dta_base <- data.frame(dta_base)
dta_base$done <- FALSE
dta_base$done[dta_base$farmer_ID%in%done] <- TRUE



pal <- colorFactor(c("red", "green"),dta_base$done)

m <- leaflet() %>% setView(lat = -13.713, lng = 33.85, zoom=9)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=dta_base, lng=~as.numeric(check.check2.maize._gps_longitude), lat=~as.numeric(check.check2.maize._gps_latitude),radius= 2,  color=~pal(done), popup = ~as.character(farmer_ID), group = "X_uuid")   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 
saveWidget(m, file="malawi_progress.html") 

to_drop <- c(
  "start",                                        
  "end",                                          
  "deviceid",                                     
  "simserial",                                    
  "phonenumber",                                  
  "subscriberid",                                 
  "enumerator",                                   
  "q1",
  "q2",                                     
  "q3",                                          
  "farmer_name",                                                              
  "phone1",                         
  "phone2",
  "phone3",
  "reason",
  "atempts",
  "check.maize.new_resp",
  "check.maize.newp1", 
  "check.maize.newp2")            
dta <- dta[ , !(names(dta) %in% to_drop)]

names(dta) <- sub("check.maize.", "",names(dta))

to_drop <- c('GPS', '_GPS_latitude', '_GPS_longitude', '_GPS_altitude', '_GPS_precision', 'meta.instanceID', 'X_id', 'X_uuid', 'X_submission_time', 'X_date_modified', 'X_tags', 'X_notes', 'X_version', 'X_duration', 'X_submitted_by', 'X_total_media', 'X_media_count', 'X_media_all_received', 'X_xform_id')
dta <- dta[ , !(names(dta) %in% to_drop)]

path <- getwd()
path <- strsplit(path,"/raw")[[1]]
write.csv(dta,paste(path,"public/endline_2023.csv", sep="/"), row.names=FALSE)
