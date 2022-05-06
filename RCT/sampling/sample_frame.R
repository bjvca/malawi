# load libraries

library(haven)
library(randomizr)

# set working directory
setwd("C:/Users/EHami/Dropbox (IFPRI)/Hami-Leocardia/Sampling_list")

# load in all the AIP data
data <- read_dta("malawi_rct_AIP_hhs.dta")
View(data)
names(data)
nrow(data)

# randomly select 113 villages
set.seed(6522)
data <- within(data,{
  treat <- complete_ra(N = 874163, m = 113)
})

#save csv output
write.csv(data[,c("district", "ta", "gvh", "village", "treat")], file = "results/sampling_frame.csv")