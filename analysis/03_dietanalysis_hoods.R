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
# diet
load('interimsteps/diet_categories_hoods.rdata')
# morph


## subsetting parameters    ----
mammalsp <- c(2)
nafodiet <- c('3L','2J','3K')
dietby <-  c('year', 'mmspcode',  'area')


## define season ----
diet$season <- ifelse((diet$month > 3) & (diet$month < 10), 'S', 'W')


## look only at data from main stomach and nafo Divs ----
diet <- diet[which(diet$digestivetractsection == 'Main Stomach'),]
diet <- diet[which(diet$nafo %in% nafodiet),]

## remove data pre 1985 -----
diet <- filter(diet, year > 1984)

## define percentage weight by prey and selected predator species    ----
prepercbio <- na.omit(diet[which(diet$mmspcode %in% mammalsp),c('idsex', 'mmsp', 'mmspcode', 'year', 'month', 'nafo', 'area', 'group', 'season', 'preycat','totalpreyweight')])
numpercbio <- summaryBy( as.formula(paste("totalpreyweight~",paste(c(dietby,'preycat'),collapse = "+"))),data = prepercbio,FUN = sum)
denpercbio <- summaryBy( as.formula(paste("totalpreyweight~",paste(dietby,collapse = "+"))),data = prepercbio,FUN = sum)
names(denpercbio) <- c(dietby, 'totalweight')
percbio <- merge(numpercbio, denpercbio ,by = dietby)
percbio$percbio <- with(percbio, (totalpreyweight.sum/totalweight))
percbio <- percbio[order(percbio[,"mmspcode"],-percbio[,"percbio"]),]

## merge diet summary info with prey category info ----
percbio <- merge(percbio, preycat, by = 'preycat')

## reorder
percbio$mmsp <- ifelse(percbio$mmspcode == 1, 'Harp seal',
                       ifelse(percbio$mmspcode == 2, 'Hooded seal',
                              ifelse(percbio$mmspcode == 5, 'Ringed seal',
                                     ifelse(percbio$mmspcode == 6, 'Bearded seal','flag'))))
percbio <- transform(percbio,
                mmsp = factor(mmsp,levels = c(
                  "Harp seal",
                  "Ringed seal",
                  "Hooded seal",
                  "Bearded seal")))

percbio <- droplevels(percbio)
## calculate sample size by Nafo Div and predator sp ----
#detach(package:plyr)
sampsize.nafo <- prepercbio %>%
  filter(mmspcode %in% mammalsp) %>%
  group_by(mmsp, area) %>%
  summarize(n = length(unique(idsex)))

## plot diet composition ----
## define palette
if (nrow(preycat) == 13) { mypalette <- c(brewer.pal(12, "Paired"), 'black') }
if (nrow(preycat) < 13) { mypalette <- brewer.pal(nrow(preycat), "Paired") }
mypalette <- rev(mypalette)

## labels
preys <- preycat$preycat




## plot
wp <- ggplot((percbio), aes(x = year, y = percbio,
                          color = as.factor((order)), fill = as.factor((order)),width = 0.7))
wp <- wp + geom_bar(stat = 'identity')
wp <- wp + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
wp <- wp  + xlab("Year") + ylab("%Biomass")
wp <- wp + scale_y_continuous(limits = c(0,1.09), labels = scales::percent, breaks = c(0,0.5,1))
wp <- wp + geom_text(data = sampsize.nafo, aes(x = 1986, y = 1.07, label =  paste("n = ",n)),
                      colour = "black", inherit.aes = FALSE, parse = FALSE,size = 4)
wp <- wp + scale_fill_manual(name = "", values = mypalette ,
                             breaks = as.factor(nrow(preycat):1),
                             labels = preys)#,
                             #guide = guide_legend( nrow = 2, byrow = T))
wp <- wp + scale_color_manual(name = "", values = mypalette ,
                              breaks = as.factor(nrow(preycat):1),
                              labels = preys)#,
                              #guide = guide_legend( nrow = 2, byrow = T))
wp <- wp + scale_x_continuous(breaks = seq(1985, 2020, 5),limits = c(1985, 2016))#,
wp <- wp + facet_grid(. ~ area, drop = TRUE)
wp <- wp + theme(legend.position = "bottom")
wp <- wp + ggtitle("Cc diet composition")
wp <- wp + theme(plot.title = element_text(size = 15, face = "bold"),
                 strip.text = element_text(size = 12),
                 legend.text = element_text(size = 11),
                 axis.text = element_text(size = 10),
                 axis.title = element_text(size = 13, face = "bold"))
wp


# save_plot("output/hood_diet.png", wp, base_width = 21, base_height = 10)#, dpi = 900) # make room for figure legend)
#
# # percbionoseason <- droplevels(subset(percbio, area == 'Inshore' | area =='Offshore'))
#  save(percbio, file = 'interimsteps/hood_percbio.Rdata')
# save(diet, mammalsp, nafodiet, file = 'interimsteps/diet_postanalysis_hood.rdata')
