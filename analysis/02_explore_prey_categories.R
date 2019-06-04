## ADB July 25, 2017.
## The intent of this script is to
## explore which prey species to include in prey categories


## load libraries  ----
library(dplyr)

## read data  ----
load('interimsteps/diet.rdata')
preysp <- read.csv('data/tlkp_preysp.csv',header = T)

## subsetting parameters    ----
# mammalsp <- c(1,2,5,6)
# nafodiet <- c('2H','2J','3K')
# dietby <-  c('mmspcode','year','nafo')

## define prey categories    ----
preysp <- preysp[order(preysp$preycode, decreasing = T),]
preysp$preycat <- NA
preysp$preycat <- ifelse(between(preysp$preycode, 9971, 9999) | preysp$prey == 'Reserved', 'donotuse', preysp$preycat)
preysp$preycat <- ifelse(preysp$prey != 'Reserved' & between(preysp$preycode, 9600, 9697), 'bird', preysp$preycat)
preysp[grepl("Pandalus", preysp$prey.scientific.name),'preycat'] <- 'shrimp pandalus'
preysp[grepl("Shrimp", preysp$prey) & !grepl("Pandalus", preysp$prey.scientific.name), 'preycat'] <- 'shrimp non-pandalid'
preysp[grepl("shrimp", preysp$preycat),'preycat'] <- 'shrimp'
preysp$preycat <- ifelse(is.na(preysp$preycat) & between(preysp$preycode, 1100, 9000) , 'invertebrate', preysp$preycat)
preysp$preycat <- ifelse(between(preysp$preycode, 7925, 7989) | preysp$preycode == 7951, 'mysid', preysp$preycat)
#preysp$preycat <- ifelse(between(preysp$preycode, 7991, 8017) , 'euphasiid', preysp$preycat)
preysp[grepl("Hyperiid", preysp$prey), 'preycat'] <- 'hyperiid amphipod'
#preysp$preycat <- ifelse(between(preysp$preycode, 7023, 7739) , 'gammarid amphipod', preysp$preycat)
preysp[grepl("Crab", preysp$prey), 'preycat'] <- 'crab'
#preysp[grepl("Gad", preysp$prey) | grepl("Cod", preysp$prey) & preysp$preycode != 451 & preysp$preycode != 438, 'preycat'] <- 'gadoid'
#preysp[grepl("Squid", preysp$prey), 'preycat'] <- 'squid'
preysp$preycat <- ifelse(between(preysp$preycode, 1000, 1099) , 'marine mammal', preysp$preycat)
preysp[which(preysp$preycode < 1000),'preycat'] <- 'fish'
preysp[which(preysp$preycode == 187),'preycat'] <- 'capelin'
preysp[which(preysp$preycode == 451),'preycat'] <- 'arctic cod'
preysp[which(preysp$preycode == 438),'preycat'] <- 'atlantic cod'
preysp[which(preysp$preycode == 892),'preycat'] <- 'greenland halibut'
preysp$preycat <- ifelse(between(preysp$preycode, 149, 150) , 'atlantic herring', preysp$preycat)
#preysp$preycat <- ifelse(between(preysp$preycode, 17, 122) , 'chondrichthyes', preysp$preycat)
#preysp[grepl("Ammodyt", preysp$prey.scientific.name), 'preycat'] <- 'sandlance'
preysp[grepl("Sebast", preysp$prey.scientific.name), 'preycat'] <- 'redfish'
preysp$preycat <- ifelse(between(preysp$preycode, 808, 832) , 'sculpin', preysp$preycat)
#preysp$preycat <- ifelse(between(preysp$preycode, 882, 909) &  preysp$preycode != 892, 'flatfish', preysp$preycat)

preycat <- data.frame(preycat = unique(preysp$preycat), stringsAsFactors = F)
cats <- c(
  "arctic cod",
  "atlantic cod",
  "capelin",
  "atlantic herring",
  "redfish",
  "greenland halibut",
  "sculpin",
  "fish",
#  "squid",
#  "shrimp pandalus",
#  "shrimp non-pandalid",
  "shrimp",
  "crab",
  "mysid",
  "hyperiid amphipod",
  "invertebrate"
)
preycat <- preycat %>%
               slice(match(cats, preycat))
preycat$order <- nrow(preycat):1

## add prey categories to diet data and eliminate non-prey items ----
diet <- merge(diet, preysp[,c('preycode','preycat')], by = 'preycode')
exclude <- c('donotuse', 'marine mammal', 'bird')
diet <- (diet[which(!diet$preycat %in% exclude),])

save(diet, preycat, file = 'interimsteps/diet_categories.rdata')
