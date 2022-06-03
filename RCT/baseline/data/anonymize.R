#run in baseline/data
library(leaflet)
library(plyr)
library(htmlwidgets)


path <- getwd()

dta <- read.csv(paste(path,"raw/latest_form1.csv", sep="/"),stringsAsFactors = FALSE )
dta_2 <- read.csv(paste(path,"raw/latest_form2.csv", sep="/"),stringsAsFactors = FALSE)

dta <- rbind.fill(dta,dta_2)

#todo: remote this check.check2.maize. clutter

names(dta) <- sub("check.check2.maize.", "",names(dta))
names(dta) <- sub("_", "",names(dta))

names(dta)[names(dta) == 'farmerID'] <- 'farmer_ID'
dim(dta[duplicated(dta$farmer_ID),])


## save some records where we could not pull from sampling list due to trailing spaces
## these were essentially treated as control so we can keep the controls

dta$farmer_ID[dta$Xuuid == "27b660d0-95a0-41be-9a40-c422df65d34f"] <- "F_72"
dta$treatment[dta$Xuuid == "27b660d0-95a0-41be-9a40-c422df65d34f"] <- "C"

dta$farmer_ID[dta$Xuuid == "98ba3655-c4d2-4f6c-8e10-99696c20cf5c"] <- "F_1048"
dta$treatment[dta$Xuuid == "98ba3655-c4d2-4f6c-8e10-99696c20cf5c"] <- "C"

dta$farmer_ID[dta$Xuuid == "e5ec1f93-9363-4b30-bbf4-de163609bd51"] <- "F_974"
dta$treatment[dta$Xuuid == "e5ec1f93-9363-4b30-bbf4-de163609bd51"] <- "C"

dta[c("Xuuid","farmername","farmer_ID")][dta$farmer_ID=="n/a",]

### this is from the new form


dim(dta[duplicated(dta$farmer_ID),])

dta$farmer_ID[duplicated(dta$farmer_ID)]
 [1] "n/a"    "F_71"   "F_1055" "F_1014" "n/a"    "F_29"   "F_1083" "F_1065"
 [9] "n/a"    "F_502"  "n/a"    "F_1082" "n/a"    "F_128"  "n/a"    "n/a"   
[17] "F_455"  "F_575"  "F_577"  "F_572"  "n/a"    "F_538"  "F_476"  "F_589" 
[25] "F_559"  "F_109"  "F_384"  "F_970"  "F_352"  "F_97"  

### I dont think duplicates are a big problem at this stage - we can just treat them as extra interviews
### so for the duplicates where q4a is no, we can just change the farmer_ID to some random new number?

for (i in dta$farmer_ID[duplicated(dta$farmer_ID)]) {
if (i != "n/a") {
print(dta[dta$farmer_ID==i,1:27])
}
}









##create a map

pal <- colorFactor(c("red", "#4169E1","#228B22"),dta$treatment)

map <-  leaflet() %>% setView(lat =mean(as.numeric(as.character(dta$gps_latitude)),na.rm=T), lng = mean(as.numeric(as.character(dta$gps_longitude)),na.rm=T), zoom=11)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=dta, lng=~as.numeric(as.character(gps_longitude)), lat=~as.numeric(as.character(gps_latitude)),radius= 2,   color = ~pal(treatment), popup = ~as.character(farmer_ID))   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 

saveWidget(map, file="malawi_progress.html") #traders and farmers 

### create IDs for district, TA and village (q1, q2, q3)

i_dist <- 1
dta$distID <- NULL
dta$taID <- NULL
dta$vilID <- NULL

for (dist in names(table(dta$q1))) {
	print(dist)
	i_sub <- 1
	for (sub in names(table(dta$q2[dta$q1==dist]))) {
		print(sub)
			i_village <- 1
			for (village in names(table(dta$q3[dta$q1 == dist & dta$q2 == sub]))) {
				print(village)
				dta$vilID[dta$q1 == dist & dta$q2 == sub & dta$q3 == village] <- i_village
				i_village <- i_village + 1
			}
		dta$taID[dta$q1 == dist & dta$q2 == sub] <- i_sub
		i_sub <- i_sub + 1
	}
dta$distID[dta$q1==dist] <- i_dist
i_dist <- i_dist + 1
}

dta$distID <- as.numeric(dta$distID)
dta$taID <- as.numeric(dta$taID)
dta$vilID <- as.numeric(dta$vilID)


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
"farmername",                                                              
"q4a",                         
"q4b",                     
"q5",
"check.q6", "check.q7",
 "check.q8", "check.check2.q9", "q10",                         
 "q11",  
 "q11a",                     
 "q11b",   
 "gps",                       
"gps_latitude",             
"gps_longitude",            
"gps_altitude",             
"gps_precision",            
"meta.instanceID",                              
"Xid",                                         
"Xuuid",                                       
"Xsubmission_time" ,                           
"Xdate_modified",                              
"Xtags",                                      
"Xnotes",                                      
"Xversion",                                    
"Xduration",                                   
"Xsubmitted_by",                               
"Xtotal_media",                                
"Xmedia_count",                                
"Xmedia_all_received",                         
"Xxform_id","T1T2")            
dta <- dta[ , !(names(dta) %in% to_drop)]
 

write.csv(dta,paste(path,"public/baseline_data.csv", sep="/"), row.names=FALSE)
