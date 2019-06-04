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
# * diet ---
load('interimsteps/diet_categories_harp.rdata')

## drop seals with lots of cod ----
# diet <- diet %>%
#   filter(preyunits < 31)
# diet <- diet %>%
#   filter(subunitarea != 340)


## subsetting parameters    ----
mammalsp <- c(1)
nafodiet <- c('3L','2J','3K')
dietby <-  c('year', 'mmspcode', 'nafo', 'area', 'season')

## define season ----
diet$season <- ifelse((diet$month > 3) & (diet$month < 10), 'S', 'W')


## look only at data from main stomach and nafo Divs ----
diet <- diet[which(diet$digestivetractsection == 'Main Stomach'),]
diet <- diet[which(diet$nafo %in% nafodiet),]

## remove data pre 1985 -----
diet <- filter(diet, year > 1984)

## define percentage weight by prey and selected predator species    ----

prop.sp.seal <- diet %>%
  group_by(idsex, preycat) %>%
  summarise(totalpw = sum(totalpreyweight, na.rm = TRUE)) %>%
  mutate(totalw = sum(totalpw), proportion = totalpw/totalw)

prop.sp.seal <- prop.sp.seal[which(!is.nan(prop.sp.seal$proportion)),]

preprops <- left_join(prop.sp.seal, diet[,c(dietby, 'idsex')], by = 'idsex') %>% unique()

props <- preprops %>%
  group_by(preycat, year, mmspcode, nafo,  area, season) %>%
  summarise(avproportion = mean(proportion))

denprops <- props %>%
  group_by( year, mmspcode, nafo,  area, season) %>%
  summarise(totprop = sum(avproportion))

propsbio <- left_join(props, denprops, by = dietby)

  propsbio$stprop <- propsbio$avproportion / propsbio$totprop

  propsbio %>%
    group_by( year, mmspcode, nafo,  area, season) %>%
    summarise(sumstprop = sum(stprop)) %>%
    ungroup() %>%
    select(sumstprop) %>% unique()
#
# prepercbio <- na.omit(diet[which(diet$mmspcode %in% mammalsp),c('idsex', 'mmsp', 'mmspcode', 'year', 'month', 'nafo', 'area', 'group', 'season', 'preycat','totalpreyweight')])
# numpercbio <- summaryBy( as.formula(paste("totalpreyweight~",paste(c(dietby,'preycat'),collapse = "+"))),data = prepercbio,FUN = sum)
# denpercbio <- summaryBy( as.formula(paste("totalpreyweight~",paste(dietby,collapse = "+"))),data = prepercbio,FUN = sum)
# names(denpercbio) <- c(dietby, 'totalweight')
# percbio <- merge(numpercbio, denpercbio ,by = dietby)
# percbio$percbio <- with(percbio, (totalpreyweight.sum/totalweight))
# percbio <- percbio[order(percbio[,"mmspcode"],-percbio[,"percbio"]),]

## merge diet summary info with prey category info ----
# percbio <- merge(percbio, preycat, by = 'preycat')
props <- propsbio
props <- merge(props, preycat,  by = 'preycat')

## reorder
props$mmsp <- ifelse(props$mmspcode == 1, 'Harp seal',
                       ifelse(props$mmspcode == 2, 'Hooded seal',
                              ifelse(props$mmspcode == 5, 'Ringed seal',
                                     ifelse(props$mmspcode == 6, 'Bearded seal','flag'))))
props <- transform(props,
                mmsp = factor(mmsp,levels = c(
                  "Harp seal",
                  "Ringed seal",
                  "Hooded seal",
                  "Bearded seal")))

props <- droplevels(props)
## calculate sample size by Nafo Div and predator sp ----
#detach(package:plyr)
sampsize.nafo <- percbio %>%
  filter(mmspcode %in% mammalsp) %>%
  group_by(mmsp, nafo, area, season) %>%
  summarize(n = length(unique(idsex)))

## plot diet composition ----
## define palette
if (nrow(preycat) == 13) { mypalette <- c(brewer.pal(12, "Paired"), 'black') }
if (nrow(preycat) < 13) { mypalette <- brewer.pal(nrow(preycat), "Paired") }
mypalette <- rev(mypalette)

## labels
preys <- preycat$preycat


props <- subset(props, mmspcode ==1)
props <- props[which(props$nafo %in% nafodiet),]
props <- props[which(!is.na(props$season)),]
nafodiet

## plot
wp <- ggplot((props), aes(x = year, y = stprop,
                          color = as.factor((order)), fill = as.factor((order)),width = 0.7))
wp <- wp + geom_bar(stat = 'identity')
wp <- wp + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
wp <- wp  + xlab("Year") + ylab("%Biomass")
wp <- wp + scale_y_continuous(limits = c(0,1.09), labels = scales::percent, breaks = c(0,0.5,1))
# wp <- wp + geom_text(data = sampsize.nafo, aes(x = 1986, y = 1.07, label =  paste("n = ",n)),
#                       colour = "black", inherit.aes = FALSE, parse = FALSE,size = 4)
wp <- wp + scale_fill_manual(name = "", values = mypalette ,
                             breaks = as.factor(nrow(preycat):1),
                             labels = preys)#,
                             #guide = guide_legend( nrow = 2, byrow = T))
wp <- wp + scale_color_manual(name = "", values = mypalette ,
                              breaks = as.factor(nrow(preycat):1),
                              labels = preys)#,
                              #guide = guide_legend( nrow = 2, byrow = T))
wp <- wp + scale_x_continuous(breaks = seq(1985, 2020, 5),limits = c(1985, 2016))#,
wp <- wp + facet_grid(nafo ~ area + season , drop = TRUE)
wp <- wp + theme(legend.position = "bottom")
wp <- wp + ggtitle("PG diet composition")
wp <- wp + theme(plot.title = element_text(size = 15, face = "bold"),
                 strip.text = element_text(size = 12),
                 legend.text = element_text(size = 11),
                 axis.text = element_text(size = 10),
                 axis.title = element_text(size = 13, face = "bold"))
wp


save_plot("output/harp_diet_not340.png", wp, base_width = 21, base_height = 10)#, dpi = 900) # make room for figure legend)

# percbionoseason <- droplevels(subset(percbio, area == 'Inshore' | area =='Offshore'))
# save(percbio, file = 'interimsteps/harp_percbio.Rdata')
#save(diet, mammalsp, nafodiet, file = 'interimsteps/diet_postanalysis_harp.rdata')
