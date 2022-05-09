### script to draw sampling frame as input of ODK for baseline data collection and treatments
### Emmanual Hami and Bjorn Van Campenout May 6th 2022

#run in /RCT/sampling

# load libraries
library(haven)
library(randomizr)

# work with relative paths
path <- getwd()

# load in all the AIP data
data <- data.frame(read_dta(paste(path,"data/malawi_rct_AIP_hhs.dta", sep="/")))

names(data)
nrow(data)

#just to make sure, we need to remove villages wiht less than 40 households
data$ones <- 1
hh_size_per_village <- data.frame(tapply(data$ones, data$village, FUN=sum))
hh_size_per_village$villate <- rownames(hh_size_per_village)
names(hh_size_per_village) <- c("nr_hh","village")
#get village names of villages with more than 40 hh
to_select <- names(table(hh_size_per_village$village[hh_size_per_village$nr_hh>40]))

data <- subset(data, village %in% to_select)

# randomly select 113 villages - let us do 120 for attrition
set.seed(6522)

sampling_frame <- data[sample(1:dim(data)[1], size=120),]

#These are the 120 villages sampled proportional to the number of people living there
#let us see if there are duplicates in village names
#sum(table(sampling_frame$village)>1) # yeah there is one (CHIFWAMSALA CHISI) this village has more than 200 households while the average village has only about 50 so this makes sense

 
#each row represents a village -  in each village we have 10 households in on treatment arm so we need to repeat 4 times 10:

contr <- sampling_frame[rep(1:nrow(sampling_frame), 10),]
contr$treat <- "C"
t1 <- sampling_frame[rep(1:nrow(sampling_frame), 10),]
t1$treat <- "T1"
t2 <- sampling_frame[rep(1:nrow(sampling_frame), 10),]
t2$treat <- "T2"
t3 <- sampling_frame[rep(1:nrow(sampling_frame), 10),]
t3$treat <- "T3"

#now stack
all <- rbind(contr,t1,t2,t3)

#just scramble them to make sure there is not a predictable order in the treatments in the ODK app
all <- all[sample(1:dim(all)[1], size=nrow(all)),]

#now order them again on district ta, gvh and village

all <- all[with(all, order(all$district, all$ta, all$gvh, all$village)),]

#add unique farmer ID already here

all$farmer_ID <- paste("F",1:nrow(all), sep="_")

#save csv output
write.csv(all[,c("farmer_ID","district", "ta", "gvh", "village", "treat")], file = paste(path,"sampling_frame_ODK.csv", sep="/"), row.names=F)
