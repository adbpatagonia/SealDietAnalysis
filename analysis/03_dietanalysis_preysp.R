## ADB July 28, 2017.
## The intent of this script is to 
## carry out comparative diet analyses of 4 seal sp


## load libraries  ----
library(doBy)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(cowplot)

## read data  ----
#setwd('/media/alejandro/adb_data/Buren_files/GitHub/DietAnalysis/')
setwd('D:/Buren_files/GitHub/DietAnalysis/')
load('interimsteps/diet_categories_comparative.rdata')

## subsetting parameters    ----
mammalsp <- c(1,5)
nafodiet <- c('2H','2J','3K')
dietby <-  c('year', 'mmsp')


## look only at data from main stomach and nafo Divs ----
diet <- diet[which(diet$digestivetractsection == 'Main Stomach'),]
diet <- diet[which(diet$nafo %in% nafodiet),]

## define percentage weight by prey and selected predator species    ----
prepercbio <- na.omit(diet[which(diet$mmspcode %in% mammalsp),c('mmsp', 'mmspcode', 'year', 'month', 'nafo', 'area', 'group', 'season', 'prey','totalpreyweight')])
numpercbio <- summaryBy( as.formula(paste("totalpreyweight~",paste(c(dietby,'prey'),collapse = "+"))),data = prepercbio,FUN = sum)
denpercbio <- summaryBy( as.formula(paste("totalpreyweight~",paste(dietby,collapse = "+"))),data = prepercbio,FUN = sum)
names(denpercbio) <- c(dietby, 'totalweight')
percbio <- merge(numpercbio, denpercbio ,by = dietby)
percbio$percbio <- with(percbio, (totalpreyweight.sum/totalweight))
#percbio <- percbio[order(percbio[,"mmspcode"],-percbio[,"percbio"]),]

percbio <- percbio[order(percbio[,"mmsp"],percbio[,"year"],-percbio[,"percbio"]),]

## merge diet summary info with prey category info ----
percbio <- merge(percbio, prey, by = 'prey')

## reorder 
percbio$mmsp <- ifelse(percbio$mmspcode == 1, 'Harp seal',
                       ifelse(percbio$mmspcode == 2, 'Hooded seal',
                              ifelse(percbio$mmspcode == 5, 'Ringed seal',
                                     ifelse(percbio$mmspcode == 6, 'Bearded seal','flag'))))
percbio <- transform(percbio,
                mmsp=factor(mmsp,levels=c(
                  "Harp seal", 
                  "Ringed seal",
                  "Hooded seal",
                  "Bearded seal")))


## calculate sample size by Nafo Div and predator sp ----
#detach(package:plyr)
sampsize.nafo <- diet %>%
  filter(mmspcode %in% mammalsp) %>%
  group_by(mmsp) %>%
  summarize(n = length(unique(idsex)))

## plot diet composition ----
## define palette
if(nrow(prey) == 13) {mypalette <- c(brewer.pal(12,"Paired"),'black' )}
if(nrow(prey) < 13) {mypalette <- brewer.pal(nrow(prey),"Paired")}
mypalette <- rev(mypalette)

## labels
preys <- prey$prey




## plot
wp <- ggplot(percbio, aes(x = year, y = percbio, 
                          color = as.factor((order)), fill = as.factor((order)),width = 0.7)) 
wp <- wp + geom_bar(stat = 'identity')
wp <- wp + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
wp <- wp  + xlab("Year") + ylab("%Biomass")
wp <- wp + scale_y_continuous(limits = c(0,1.09), labels = scales::percent, breaks = c(0,0.5,1))
wp <- wp + geom_text(data = sampsize.nafo, aes(x = 1980, y = 1.07, label =  paste("n = ",n)),
                      colour = "black", inherit.aes = FALSE, parse = FALSE,size = 5)
wp <- wp + scale_fill_manual(name = "", values = mypalette ,
                             breaks = as.factor(nrow(prey):1),
                             labels = preys,
                             guide = guide_legend( nrow = 2, byrow = T))
wp <- wp + scale_color_manual(name = "", values = mypalette ,
                              breaks = as.factor(nrow(prey):1),
                              labels = preys,
                              guide = guide_legend( nrow = 2, byrow = T))


wp <- wp + scale_x_continuous(breaks = seq(1980, 2009, 5),limits = c(1978, 2007))#,
wp <- wp + facet_grid(mmsp~., drop = TRUE)
wp <- wp + theme(legend.position = "bottom")
wp <- wp + ggtitle("Diet composition")
wp <- wp + theme(plot.title = element_text(size = 25, face = "bold"), 
                 strip.text = element_text(size = 18),
                 legend.text=element_text(size = 12),
                 axis.text=element_text(size = 12),
                 axis.title=element_text(size = 15, face="bold"))
wp 


#save_plot("output/comparative_diet2.png", wp, base_width = 21, base_height = 10)#, dpi = 900) # make room for figure legend)


## plot
wpagg <- ggplot(percbio, aes(x = order, y = percbio, 
                          color = as.factor((order)), fill = as.factor((order)),width = 0.7)) 
wpagg <- wpagg + geom_bar(stat = 'identity')
wpagg <- wpagg + theme_bw()
wpagg <- wpagg  + xlab("Prey") + ylab("%W")
wpagg <- wpagg + scale_y_continuous(limits = c(0,1.09), labels = scales::percent, breaks = c(0,0.25,0.5,0.75,1))
# wpagg <- wpagg + geom_text(data = sampsize.nafo, aes(x = 1980, y = 1.06, label =  paste("n = ",n)),
#                      colour = "black", inherit.aes = FALSE, parse = FALSE,size = 3)
wpagg <- wpagg + scale_fill_manual(name = "", values = mypalette ,
                             breaks = as.factor(nrow(prey):1),
                             labels = preys)
wpagg <- wpagg + scale_color_manual(name = "", values = mypalette ,
                              breaks = as.factor(nrow(prey):1),
                              labels = preys)
wpagg <- wpagg + facet_grid(mmsp~., drop = TRUE)
wpagg 



save(percbio, file = 'interimsteps/diet.Rdata')
#save(diet, mammalsp, nafodiet, file = 'interimsteps/diet_postanalysis.rdata')
