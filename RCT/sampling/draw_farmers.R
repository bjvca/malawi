#this script connects names and national IDS to the farmers that were already sampled using "sample_frame.R" resulting in the file "/malawi/RCT/sampling/sampling_frame_ODK.csv"
#run in /home/bjvca/data/projects/malawi/RCT/sampling/

# work with relative paths
path <- getwd()
set.seed(17052022)
# load in all the AIP data
sampling_frame <-read.csv(paste(path,"sampling_frame_ODK.csv", sep="/"))
sampling_frame$ones <- 1

raw_dowa <-read.csv(paste(path,"data/full_data/dowa.csv", sep="/"))
raw_kasungu <-read.csv(paste(path,"data/full_data/kasungu.csv", sep="/"))


### cleaning data
raw_dowa[c("district","ta","gvh","village")] <- lapply(raw_dowa[c("district","ta","gvh","village")], function(x) toupper(as.character(x) ))
raw_kasungu[c("district","ta","gvh","village")] <- lapply(raw_kasungu[c("district","ta","gvh","village")], function(x) toupper(as.character(x) ))

raw_dowa[c("district","ta","gvh","village")] <- lapply(raw_dowa[c("district","ta","gvh","village")], function(x) trimws(as.character(x) ))
raw_kasungu[c("district","ta","gvh","village")] <- lapply(raw_kasungu[c("district","ta","gvh","village")], function(x) trimws(as.character(x) ))

##for each village in sampling_frame - randomly select 40 households and cbind them to the sampling frame

### first we need to get district, ta, gvh, village combinations
iterator <- aggregate(ones~district+ta+gvh+village,data=sampling_frame, FUN=sum)

#select DOWA

dowa_villages <- subset(iterator,district=="DOWA")
kasungu_villages <- subset(iterator,district=="KASUNGU")

#initialize sample_names:
sample_names <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(sample_names) <- c("farmer_ID","district","ta","gvh","village","treat","ones", "person_type", "person_name", "nid", "sex")  

for (i in (1:nrow(dowa_villages))) {
df <- raw_dowa[as.character(raw_dowa$ta) == as.character(dowa_villages$ta[i]) & as.character(raw_dowa$gvh) == as.character(dowa_villages$gvh[i]) & as.character(raw_dowa$village) == as.character(dowa_villages$village[i]),]
sample_names_int <- cbind(sampling_frame[as.character(sampling_frame$ta)==as.character(dowa_villages$ta[i]) & as.character(sampling_frame$gvh)==as.character(dowa_villages$gvh[i]) & as.character(sampling_frame$village)==as.character(dowa_villages$village[i]),] ,df[sample(nrow(df), 40), c("person_type", "person_name", "nid", "sex") ])
sample_names <- rbind(sample_names, sample_names_int)
}

colnames(sample_names) <- c("farmer_ID","district","ta","gvh","village","treat","ones", "person.type", "person.name", "NID", "sex")  

for (i in (1:nrow(kasungu_villages))) {
df <- raw_kasungu[as.character(raw_kasungu$ta) == as.character(kasungu_villages$ta[i]) & as.character(raw_kasungu$gvh) == as.character(kasungu_villages$gvh[i]) & as.character(raw_kasungu$village) == as.character(kasungu_villages$village[i]),]
sample_names_int <- cbind(sampling_frame[as.character(sampling_frame$ta)==as.character(kasungu_villages$ta[i]) & as.character(sampling_frame$gvh)==as.character(kasungu_villages$gvh[i]) & as.character(sampling_frame$village)==as.character(kasungu_villages$village[i]),] ,df[sample(nrow(df), 40),c("person.type", "person.name", "NID", "sex")  ])
sample_names <- rbind(sample_names, sample_names_int)
}

sample_names <- sample_names[,names(sample_names)!="ones"]

write.csv(sample_names, file = paste(path,"sampling_frame_ODK_names.csv", sep="/"), row.names=F)




