#run in ~/data/projects/malawi/RCT/endline_2024/data/raw
rm(list=ls())
dta <- read.csv("latest.csv")

#get IDs of duplicates
dups <- dta$farmer_ID[duplicated(dta$farmer_ID)]

write.csv(subset(dta,farmer_ID %in% dups)[,1:25], file="duplis.csv")

##not sure how we can solve issues related to duplicates - delete for now
dta <- subset(dta, !(farmer_ID %in% dups))

to_drop <- c(
  "start",                                        
  "end",                                          
  "deviceid",                                     
  "simserial",                                    
  "phonenumber",                                  
  "subscriberid",                                 
  "enumerator", "q1",                                  
  "q2",                                     
  "q3",                                          
  "farmer_name",
  "check.maize.new_resp",
  "phone1",                         
  "phone2",
  "phone3",
  "check.maize.newp2",
  "meta.instanceID",                              
  "X_id",                                         
  "X_uuid",                                       
  "X_submission_time" ,                           
  "X_date_modified",                              
  "X_tags",                                      
  "X_notes",                                      
  "X_version",                                    
  "X_duration",                                   
  "X_submitted_by",                               
  "X_total_media",                                
  "X_media_count",                                
  "X_media_all_received",                         
  "X_xform_id")            
dta <- dta[ , !(names(dta) %in% to_drop)]

names(dta) <- sub("check.maize.", "",names(dta))
path <- getwd()
path <- strsplit(path,"/raw")[[1]]
write.csv(dta,paste(path,"public/endline_2024.csv", sep="/"), row.names=FALSE)

