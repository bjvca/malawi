path <- getwd()
path <- strsplit(path, "/all_rounds/analysis")[[1]]
### calcuate income from gnuts
##@@@ this is where midline data gets read in

midline_sept <- read.csv(paste(path,"midline_sept/data/public/midline_sept.csv", sep="/"))
##remove duplicates
dups <- midline_sept$farmer_ID[duplicated(midline_sept$farmer_ID)]
dta_sept <- midline_sept[!(midline_sept$farmer_ID) %in% dups,]

midline_dec <- read.csv(paste(path,"midline_dec/data/public/midline_dec.csv", sep="/"))
##remove duplicates
dups <- midline_dec$farmer_ID[duplicated(midline_dec$farmer_ID)]
dta_dec <- midline_dec[!(midline_dec$farmer_ID) %in% dups,]

###
set_q <- paste(paste("trans1",1:7,sep="."),"group4.q47c", sep="..")
dta_sept[set_q] <- lapply(dta_sept[set_q],  function(x) as.numeric(as.character(x)))

set_p <- paste(paste("trans1",1:7,sep="."),"q47d", sep="..")
dta_sept[set_p] <- lapply(dta_sept[set_p],  function(x) as.numeric(as.character(x)))

dta_sept[set_p] <- lapply(dta_sept[set_p],  function(x) ifelse(is.na(x),0,x))
dta_sept[set_q] <- lapply(dta_sept[set_q],  function(x) ifelse(is.na(x),0,x))

dta_sept$gnuts_sept <- rowSums(dta_sept[set_p]*dta_sept[set_q]*3.62)

summary(dta_sept$gnuts_sept )


###now for second midline
set_q <- c("trans1.1..group4.q47c","trans1.2..group4.q47c","trans1.3..group4.q47c","trans1.4..group4.q47c")
dta_dec[set_q] <- lapply(dta_dec[set_q],  function(x) as.numeric(as.character(x)))

set_p <- c("trans1.1..q47d","trans1.2..q47d","trans1.3..q47d","trans1.4..q47d")
dta_dec[set_p] <- lapply(dta_dec[set_p],  function(x) as.numeric(as.character(x)))

dta_dec[set_p] <- lapply(dta_dec[set_p],  function(x) ifelse(is.na(x),0,x))
dta_dec[set_q] <- lapply(dta_dec[set_q],  function(x) ifelse(is.na(x),0,x))

dta_dec$gnuts_dec <- rowSums(dta_dec[set_p]*dta_dec[set_q]*3.62)


summary(dta_dec$gnuts_dec)
## merge this 

gnuts_all <- merge(dta_sept[c("farmer_ID","gnuts_sept","q44","q1")],dta_dec[c("farmer_ID","gnuts_dec")] )
gnuts <- subset(gnuts_all, q44!="88")
summary(gnuts$gnuts_sept+gnuts$gnuts_dec)

gnuts <- subset(gnuts, q1=="KASUNGU")
summary(gnuts$gnuts_sept+gnuts$gnuts_dec)
#Now for soy 

set_q <- paste(paste("trans2",1:11,sep="."),"group6.q52c", sep="..")
dta_sept[set_q] <- lapply(dta_sept[set_q],  function(x) as.numeric(as.character(x)))
set_p <- paste(paste("trans2",1:11,sep="."),"q52d", sep="..")
dta_sept[set_p] <- lapply(dta_sept[set_p],  function(x) as.numeric(as.character(x)))

dta_sept[set_p] <- lapply(dta_sept[set_p],  function(x) ifelse(is.na(x),0,x))
dta_sept[set_q] <- lapply(dta_sept[set_q],  function(x) ifelse(is.na(x),0,x))

dta_sept$soy_sept <- rowSums(dta_sept[set_p]*dta_sept[set_q]*50)

summary(dta_sept$soy_sept)

set_q <- paste(paste("trans2",1:3,sep="."),"group6.q52c", sep="..")
dta_dec[set_q] <- lapply(dta_dec[set_q],  function(x) as.numeric(as.character(x)))
set_p <- paste(paste("trans2",1:3,sep="."),"q52d", sep="..")
dta_dec[set_p] <- lapply(dta_dec[set_p],  function(x) as.numeric(as.character(x)))

dta_dec[set_p] <- lapply(dta_dec[set_p],  function(x) ifelse(is.na(x),0,x))
dta_dec[set_q] <- lapply(dta_dec[set_q],  function(x) ifelse(is.na(x),0,x))

dta_dec$soy_dec <- rowSums(dta_dec[set_p]*dta_dec[set_q]*50)

summary(dta_dec$soy_dec)

soy_all <- merge(dta_sept[c("farmer_ID","soy_sept","q48","q1")],dta_dec[c("farmer_ID","soy_dec")] )
soy <- subset(soy_all, q48!="88")
summary(soy$soy_sept+soy$soy_dec)

soy <- subset(soy, q1=="KASUNGU")
summary(soy$soy_sept+soy$soy_dec)
