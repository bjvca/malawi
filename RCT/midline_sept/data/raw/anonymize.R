#run in /home/bjvca/data/projects/malawi/RCT/midline_sept/data/raw/
dta <- read.csv("latest.csv")

#get IDs of duplicates
dups <- dta$farmer_ID[duplicated(dta$farmer_ID)]

write.csv(subset(dta,farmer_ID %in% dups)[,1:25], file="duplis.csv")

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
  "check.maize.newp2",
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
  "Xxform_id")            
dta <- dta[ , !(names(dta) %in% to_drop)]

names(dta) <- sub("check.maize.", "",names(dta))
path <- getwd()
path <- strsplit(path,"/raw")[[1]]
write.csv(dta,paste(path,"public/midline_sept.csv", sep="/"), row.names=FALSE)

prop.table(table(dta$q41, dta$treatment),2)

summary(lm((q41=="Yes")~treatment,data=dta))
