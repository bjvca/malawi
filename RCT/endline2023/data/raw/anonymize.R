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
rbind(read.csv(paste(path,"baseline/data/raw/latest_form1.csv", sep="/"))[c("farmer_ID","q3","check.check2.maize._gps_longitude","check.check2.maize._gps_latitude")],
read.csv(paste(path,"baseline/data/raw/latest_form2.csv", sep="/"))[c("farmer_ID","q3","check.check2.maize._gps_longitude","check.check2.maize._gps_latitude")])


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

#duplicates
dta$farmer_ID[duplicated(dta$farmer_ID)]

dupper <- "n/a"
#get name of village
dupper_vil <- "KOTHO" 
#what farmers from baseline can not be matched with a farmer from enldine 
unmatched <- setdiff(dta_base$farmer_ID[dta_base$q3 == dupper_vil],dta$farmer_ID[dta$q3 == dupper_vil])
#so one of the unmatched farmers should be linked to one of the duplicates


#let us put this on a map - in green the unmateched baselines and the duplicate

dta_base <- dta_base[dta_base$farmer_ID %in% c(dupper,unmatched),c("farmer_ID", "q3", "check.check2.maize._gps_longitude", "check.check2.maize._gps_latitude")]
names(dta_base) <- c("farmer_ID","village","longitude","latitude")
dta_base$X_uuid <- NA
dta_base$round <- "baseline"

dup <- dta[dta$farmer_ID==dupper,c("farmer_ID","q3","check.maize._GPS_longitude","check.maize._GPS_latitude","X_uuid")]
names(dup) <- c("farmer_ID","village","longitude","latitude","X_uuid")
dup$round <- "endline"



dup <- rbind(dup,dta_base)

pal <- colorFactor(c("red", "green"),dup$round)
m <- leaflet() %>% setView(lat = -13.713, lng = 33.85, zoom=9)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=dup, lng=~as.numeric(longitude), lat=~as.numeric(latitude),radius= 2,  color=~pal(round), popup = ~as.character(farmer_ID), group = "X_uuid")   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 

mX <- leaflet() %>% setView(lat = -13.713, lng = 33.85, zoom=9)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=dup, lng=~as.numeric(longitude), lat=~as.numeric(latitude),radius= 2,  color=~pal(round), popup = ~as.character(X_uuid), group = "X_uuid")   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 

dta <- dta[dta$X_uuid !="cac3c860-909a-440f-b23f-f6aa2b91fdb5",] #fix F_477
dta <- dta[dta$X_uuid !="cde953b8-4f25-4e92-97c2-8565a633280d",] #fix F_1195
dta <- dta[dta$X_uuid !="ac68b8d6-82c6-4054-83b4-74c0097190f6",] #fix F_1356
dta <- dta[dta$X_uuid !="9bf2acd1-f7d4-4ad6-8f11-3718cfa0ae73",] #fix F_2173
dta <- dta[dta$X_uuid !="b070c736-8a05-48c1-84ca-0fe64fe8e28d",] #fix F_2945
dta <- dta[dta$farmer_ID !="F_2953",]
dta <- dta[dta$farmer_ID !="F_2264",]
dta <- dta[dta$farmer_ID !="F_2266",]
dta <- dta[dta$X_uuid !="4bca98bd-1d66-4e7d-a2a2-44a7d0ca5904",] #fix F_2342
dta <- dta[dta$X_uuid !="e132965b-c3db-419a-bbba-49e586e0220d",] #fix F_2347
dta <- dta[dta$farmer_ID !="F_2521",]
dta <- dta[dta$farmer_ID !="F_2531",]
dta <- dta[dta$farmer_ID !="F_2374",]
dta <- dta[dta$X_uuid !="1661d0f8-8184-451e-8b3a-ffcdbe328a21",] #fix F_2368
dta <- dta[dta$farmer_ID !="F_3016",]
dta <- dta[dta$farmer_ID !="F_2385",]
dta <- dta[dta$farmer_ID !="F_2524",]
dta <- dta[dta$farmer_ID !="F_2534",]
dta <- dta[dta$farmer_ID !="F_2494",]
dta <- dta[dta$X_uuid !="450cc225-55ee-435a-92ac-d57bc9822261",]
dta <- dta[dta$farmer_ID !="F_1072",]
dta <- dta[dta$farmer_ID !="F_3540",]
dta <- dta[dta$X_uuid !="c40aa25e-31db-4f1a-a987-8473f22b223c",] #fix F_1084
dta <- dta[dta$farmer_ID !="F_518",]
dta <- dta[dta$farmer_ID !="F_1765",]
dta <- dta[dta$farmer_ID !="F_1284",]
dta <- dta[dta$farmer_ID !="F_2297",]
dta <- dta[dta$farmer_ID !="F_1274",]
dta <- dta[dta$farmer_ID !="F_506",]
dta <- dta[dta$farmer_ID !="F_512",]
dta <- dta[dta$farmer_ID !="F_897",]
dta <- dta[dta$farmer_ID !="F_135",]
dta <- dta[dta$farmer_ID !="F_3425",]
dta$farmer_ID[duplicated(dta$farmer_ID)]
dta$farmer_ID[dta$X_uuid == "c79c2166-f57e-4b94-a8d1-4edfee508659"] <- "F_1282"
dta$farmer_ID[dta$X_uuid == "4bb7f855-9c62-4c06-9c78-0b694093d541"] <- "F_1294"
dta$farmer_ID[dta$X_uuid == "29174733-8404-48c4-9dc9-7564ea39de4e"] <- "F_1407"
dta$farmer_ID[dta$X_uuid == "5679c60b-6398-426b-893e-8b26ee2b67b2"] <- "F_1419"
dta$farmer_ID[dta$X_uuid == "040ea61c-f9a3-46e0-9f92-12e8970bc617"] <- "F_1402"

dta <- dta[dta$farmer_ID !="n/a",]
#dupper <- "F_3425"
#dup <- dta[dta$farmer_ID==dupper,c("farmer_ID","q3","check.maize._GPS_longitude","check.maize._GPS_latitude","X_uuid")]
#names(dup) <- c("farmer_ID","village","longitude","latitude","X_uuid")
#dup$round <- "endline"


#dta_base <- dta_base[dta_base$q3 == dta$q3[dta$farmer_ID == dupper][1],c("farmer_ID", "q3", "check.check2.maize._gps_longitude", "check.check2.maize._gps_latitude")]
#names(dta_base) <- c("farmer_ID","village","longitude","latitude")
#dta_base$X_uuid <- NA
#dta_base$round <- "baseline"


#dup <- rbind(dup,dta_base)


#pal <- colorFactor(c("red", "green"),dup$round)
#m <- leaflet() %>% setView(lat = -13.713, lng = 33.85, zoom=9)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=dup, lng=~as.numeric(longitude), lat=~as.numeric(latitude),radius= 2,  color=~pal(round), popup = ~as.character(farmer_ID), group = "X_uuid")   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 

#mX <- leaflet() %>% setView(lat = -13.713, lng = 33.85, zoom=9)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=dup, lng=~as.numeric(longitude), lat=~as.numeric(latitude),radius= 2,  color=~pal(round), popup = ~as.character(X_uuid), group = "X_uuid")   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 


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
