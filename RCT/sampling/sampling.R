# load packages
install.packages("magrittr")
install.packages("expss")
install.packages("gmodels")
install.packages("stats")

library(stats)
library(expss)
library(magrittr)
library(forcats)
library(haven)
library(tidyverse)
library(ggplot2)
library(gmodels)

# set working directory
setwd("C:/git/MALAWI/malawi/RCT/sampling")

# bring in all the census data of all households
census_list <- read_dta("census_data_2018.dta")

census_list <- census_list[ , -which(names(census_list) %in% 
                    c("SEGMENT", "DATE_OF_BIRTH", "EA_CODE", 
                      "Sex", "Residential_District" ))]
str(census_list)
class(census_list$DISTRICT)

# keep only the 6 study districts

val_lab(census_list$DISTRICT)
# (104- Rumphi; 105- Mzimba; 201- Kasungu; 203- Ntchisi; 
# 204- Dowa; 207- Mchinji)

study_dists <- census_list[census_list$DISTRICT %in% c("201", 
                                                      "204", 
                                                      "104", 
                                                      "105", 
                                                      "203", 
                                                      "207"), ] 

val_lab(study_dists$DISTRICT) = num_lab("104 Rumphi 
                                         105 Mzimba 
                                         201 Kasungu 
                                         203 Ntchisi 
                                         204 Dowa 
                                         207 Mchinji")
val_lab(study_dists$DISTRICT)

# keeping RELATIONSHIP == 1 (hh head) to remove duplicates for hhs

sampling_frame <- subset(study_dists, RELATIONSHIP == 1)

as.factor(sampling_frame$DISTRICT)
as.integer(sampling_frame$RELATIONSHIP)
as.factor(sampling_frame$VILLAGE_NAME)
as.factor(sampling_frame$TA_CODE)
as.factor(sampling_frame$GVH_NAME)

sampling_frame$number_hhs <- sampling_frame$RELATIONSHIP

remove(census_list, study_dists)

# aggregating to total up the number of households per VILLAGE_NAME
table(sampling_frame$DISTRICT)
names(sampling_frame)

sampling_frame_hhs <- aggregate(number_hhs ~ DISTRICT + TA_CODE +  GVH_NAME + VILLAGE_NAME + RESIDENCE, sampling_frame, sum)

val_lab(sampling_frame_hhs$RESIDENCE) = num_lab("1 Urban 
                                                 2 Rural")
sampling_frame_hhs1 <- subset(sampling_frame_hhs, number_hhs > 200)

sampling_frame_hhs1 <- sampling_frame_hhs1[complete.cases(sampling_frame_hhs1), ]
table(sampling_frame_hhs1$DISTRICT)

# sort by DISTRICT and number of hhs in each VILLAGE_NAME
names(sampling_frame_hhs1)
sampling_frame_sort <- arrange(sampling_frame_hhs1, DISTRICT, TA_CODE, GVH_NAME, VILLAGE_NAME, desc(number_hhs))
table(sampling_frame_sort$DISTRICT)

# output sampling frame with total hhs per VILLAGE_NAME
write.csv(sampling_frame_sort,"C:/git/MALAWI/malawi/RCT/sampling/total_hhs_per_village.csv", row.names = TRUE)

table(sampling_frame_sort$DISTRICT)

          
## random sampling of the study villages
set.seed(782644418)
# Treatments will be implemented in 113 villages. 
# But we will also collect price data from another 30 villages
# 113 + 30 = 143 villages to be sampled.

sampled_villages <- sample_n(sampling_frame_sort, 143)



table(sampled_villages$VILLAGE_NAME)

# sorting the sampled villages
sampled_villages <- arrange(sampled_villages, DISTRICT, TA_CODE, GVH_NAME, VILLAGE_NAME, desc(number_hhs))

# output sampled villages
write.csv(sampled_villages,"C:/git/MALAWI/malawi/RCT/sampling/sampled_villages.csv", row.names = TRUE)

