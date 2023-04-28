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
