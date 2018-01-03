## ADB July 28, 2017.
## The intent of this script is to
## carry out comparative diet analyses of 4 seal sp


## load libraries  ----
library(doBy)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

## read data  ----
load('interimsteps/diet_categories_harbour.rdata')

## TEMPORAL ----
# Temporarily create column 'season' in object diet, so the code works
# Definitive fix: create the column in file 01_calculate_length_weight.R, based on criteria defined in database
diet$season <- rep('N', nrow(diet))

## subsetting parameters    ----
mammalsp <- c(4)
nafodiet <- c('2H','2J','3K','3L','3Pn','3Ps','4R')
#dietby <-  c('mmspcode','nafo')
dietby <-  c('mmspcode','year')


## look only at data from main stomach and nafo Divs ----
diet <- diet[which(diet$digestivetractsection == 'Main Stomach'),]
diet <- diet[which(diet$nafo %in% nafodiet),]

## define percentage weight by prey and selected predator species    ----
prepercbio <- na.omit(diet[which(diet$mmspcode %in% mammalsp),c('mmsp', 'mmspcode', 'year', 'month', 'nafo', 'area', 'group', 'season', 'preycat','totalpreyweight')])
numpercbio <- summaryBy( as.formula(paste("totalpreyweight~",paste(c(dietby,'preycat'),collapse = "+"))),data = prepercbio,FUN = sum)
denpercbio <- summaryBy( as.formula(paste("totalpreyweight~",paste(dietby,collapse = "+"))),data = prepercbio,FUN = sum)
names(denpercbio) <- c(dietby, 'totalweight')
percbio <- merge(numpercbio, denpercbio ,by = dietby)
percbio$percbio <- with(percbio, (totalpreyweight.sum/totalweight))
percbio <- percbio[order(percbio[,"mmspcode"],-percbio[,"percbio"]),]

## merge diet summary info with prey category info ----
percbio <- merge(percbio, preycat, by = 'preycat', all.y = T)

## reorder
percbio$mmsps <- ifelse(percbio$mmspcode == 1, 'Harp seal',
                       ifelse(percbio$mmspcode == 2, 'Hooded seal',
                              ifelse(percbio$mmspcode == 5, 'Ringed seal',
                                     ifelse(percbio$mmspcode == 6, 'Bearded seal',
                                            ifelse(percbio$mmspcode == 4, 'Harbour seal',
                                                   ifelse(percbio$mmspcode == 3, 'Grey seal','flag'))))))
#percbio <- transform(percbio,
                # mmsp=factor(mmsp,levels=c(
                #   "Harp seal",
                #   "Ringed seal",
                #   "Hooded seal",
                #   "Bearded seal")))


## calculate sample size by Nafo Div and predator sp ----
sn <- diet[which(diet$mmspcode %in% mammalsp),] %>% group_by(nafo, mmsp)
sampsize.nafo <-  summarize(sn, length(unique(idsex)))
names(sampsize.nafo)[3] <- 'n'
sampsize.nafo <- sampsize.nafo %>% group_by(nafo, mmsp)



sn <- diet[which(diet$mmspcode %in% mammalsp),] %>% group_by( year, mmsp)
sampsize.nafo <-  summarize(sn, length(unique(idsex)))
names(sampsize.nafo)[3] <- 'n'
sampsize.nafo <- sampsize.nafo %>% group_by( year, mmsp)


## plot diet composition ----
## define palette
if(nrow(preycat) == 14) {mypalette <- c(brewer.pal(12,"Paired"),'black' ,'grey50')}
if(nrow(preycat) == 13) {mypalette <- c(brewer.pal(12,"Paired"),'black' )}
if(nrow(preycat) < 13) {mypalette <- brewer.pal(nrow(preycat),"Paired")}
mypalette <- rev(mypalette)

## labels
preys <- preycat$preycat




## plot
wp <- ggplot(percbio, aes(x = year, y = percbio,
                          color = as.factor((order)), fill = as.factor((order)),width = 0.7))
wp <- wp + geom_bar(stat = 'identity')
wp <- wp + theme_bw()
wp <- wp  + xlab("Year") + ylab("%W")
wp <- wp + scale_y_continuous(limits = c(0,1.09), labels = scales::percent, breaks = c(0,0.25,0.5,0.75,1))
wp <- wp + geom_text(data = sampsize.nafo, aes(x = 1980, y = 1.06, label =  paste("n = ",n)),
                      colour = "black", inherit.aes = FALSE, parse = FALSE,size = 3)
wp <- wp + scale_fill_manual(name = "", values = mypalette ,
                             breaks = as.factor(nrow(preycat):1),
                             labels = preys)
wp <- wp + scale_color_manual(name = "", values = mypalette ,
                              breaks = as.factor(nrow(preycat):1),
                              labels = preys)
#wp <- wp + facet_grid(nafo~mmsps, drop = TRUE)
wp <- wp + facet_grid(.~mmsps, drop = TRUE)
wp


wpagg <- ggplot(percbio, aes(x = order, y = percbio,
                          color = as.factor((order)), fill = as.factor((order)),width = 0.7))
wpagg <- wpagg + geom_bar(stat = 'identity')
wpagg <- wpagg + theme_bw()
wpagg <- wpagg  + xlab("") + ylab("%W")
wpagg <- wpagg + scale_y_continuous( limits = c(0,0.4), labels = scales::percent)
wpagg <- wpagg + geom_text(data = sampsize.nafo, aes(x = 1, y = .35, label =  paste("n = ",n)),
                           colour = "black", inherit.aes = FALSE, parse = FALSE,size = 3)
wpagg <- wpagg + scale_fill_manual(name = "", values = mypalette ,
                             breaks = as.factor(nrow(preycat):1),
                             labels = preys)
wpagg <- wpagg + scale_color_manual(name = "", values = mypalette ,
                              breaks = as.factor(nrow(preycat):1),
                              labels = preys)

wpagg <- wpagg + facet_grid(nafo~mmsps, drop = TRUE)#, ncol = 1)
wpagg

## plot by year
pby <- merge(percbio, sampsize.nafo[,c('year','n')], by = 'year')
wpy <- ggplot(pby, aes(x = year, y = percbio,
                          color = as.factor((order)), fill = as.factor((order)),width = 0.7, label = n ))
wpy <- wpy + geom_bar(stat = 'identity')
wpy <- wpy + theme_bw()
wpy <- wpy  + xlab("Year") + ylab("%W")
wpy <- wpy + scale_y_continuous(limits = c(0,1.09), labels = scales::percent, breaks = c(0,0.25,0.5,0.75,1))

#wpy <- wpy + geom_text(size = 3, position = position_stack(vjust = 0.5), col=' black')
wpy <- wpy + geom_text(data = sampsize.nafo, aes(x = sampsize.nafo$year, y = 1.01, label =  n),
                    colour = "black", inherit.aes = FALSE, parse = FALSE,size = 3)
wpy <- wpy + scale_fill_manual(name = "", values = mypalette ,
                             breaks = as.factor(nrow(preycat):1),
                             labels = preys)
wpy <- wpy + scale_color_manual(name = "", values = mypalette ,
                              breaks = as.factor(nrow(preycat):1),
                              labels = preys)
#wpy <- wpy + facet_grid(nafo~mmsps, drop = TRUE)
wpy <- wpy + facet_grid(.~mmsps, drop = TRUE)
wpy


#save(diet, mammalsp, nafodiet, file = 'interimsteps/diet_postanalysis_harbour.rdata')
