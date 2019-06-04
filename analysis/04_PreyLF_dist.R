## ADB July 28, 2017.
## The intent of this script is to
## compare the LF distributions of fish in harp and ringed seal diets

## read data  ----
load('interimsteps/diet_postanalysis.rdata')


## load libraries  ----
library(plyr)
library(ggplot2)
library(cowplot)

## subset diet data by mm sp, prey sp, nafo, and eliminate erroneous length (> 4 mts long) ----
preyplotdata <- subset(diet,mmspcode == 1 | mmspcode == 5)
preyplotdata <- subset(preyplotdata,
                       preycat == 'arctic cod' |
                         preycat == 'capelin' |
                         #                         preycat == 'Sandlances' |
                         #                         preycat == 'redfish' |
                         preycat == 'atlantic cod' |
                         preycat == 'atlantic herring'
                       #                         preycat == 'Pleuronectidae' |
                       #                         preycat == 'Other Fishes'
)
preyplotdata <- preyplotdata[which(preyplotdata$nafo %in% nafodiet), ]
preyplotdata <- subset(preyplotdata,predlength < 400)
preyplotdata <- droplevels.data.frame(preyplotdata)

## plot LF dists ----

preyplotdata$preyc <- ifelse(preyplotdata$preycat == 'arctic cod', 'A',
                             ifelse(preyplotdata$preycat == 'capelin', 'B',
                                ifelse(preyplotdata$preycat == 'atlantic herring', 'C',
                                       ifelse(preyplotdata$preycat == 'atlantic cod', 'D', 'E'))))
labels <- c(A = "arctic cod", B = "capelin", C = 'atlantic herring', D = 'atlantic cod')

colores <- c(rgb(0,115/255,76/255),rgb(1,85/255,0))
colores <- c('#00734C', '#FF5500')
preyplot <- ggplot(preyplotdata, aes(predlength, fill = mmsp, colour = mmsp))
preyplot <- preyplot + geom_density(alpha = 0.1)
preyplot <- preyplot  + xlab("Prey length") + ylab("Density")
preyplot <- preyplot + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
preyplotnafo <- preyplot + facet_grid(nafo ~ preycat, drop = TRUE,scales = "free")
preyplot <- preyplot + facet_wrap( ~preyc, drop = TRUE, scales = "free_x", labeller = labeller(preyc = labels))
preyplot <- preyplot + theme(legend.title = element_blank())
preyplot <- preyplot + scale_fill_manual(values = colores, guide = FALSE) +
                        scale_colour_manual(values = colores)
preyplot <- preyplot + theme(legend.justification = c(1,1), legend.position = c(.98,0.4))
preyplot <- preyplot + scale_y_continuous(limits = c(0,0.2), breaks = c(0,0.2), minor_breaks = c(0.1))
preyplot <- preyplot + ggtitle("Prey length frequency distribution")
preyplot <- preyplot + theme(plot.title = element_text(size = 16, face = "bold"),
                             strip.text = element_text(size = 14, face = "bold"),
                             legend.text=element_text(size = 13),
                             axis.text=element_text(size = 12),
                             axis.title=element_text(size = 13, face="bold")
                             )
preyplot
save_plot("output/Prey_LF.png", preyplot, base_width = 10, base_height = 10)#, dpi = 900) # make room for figure legend)



## sample sizes -----
sampsize.nafo <- ddply(diet[which(diet$mmspcode %in% mammalsp),],
                       .(nafo,mmsp),
                       summarize,
                       n = paste("n =", length(unique(idsex))))

sampsize.nafo <- sampsize.nafo[which(sampsize.nafo$nafo %in% nafodiet),]


sampsize.nafomonth <- ddply(diet[which(diet$mmspcode %in% mammalsp),],
                            .(nafo,month,mmsp),
                            summarize,
                            n = length(unique(idsex)))


sampsize.nafomonth <- sampsize.nafomonth[which(sampsize.nafomonth$nafo %in% nafodiet),]
sampsize.nafomonth <- droplevels(sampsize.nafomonth)


totals <- aggregate(sampsize.nafomonth$n,by = list(sampsize.nafomonth$nafo,sampsize.nafomonth$mmsp),FUN = sum)
names(totals) <- c('nafo','mmsp','totals')

sampsize.nafomonth <- merge(sampsize.nafomonth,totals,by = c('nafo','mmsp'))
sampsize.nafomonth$reln <- with(sampsize.nafomonth,n/totals)


sampsize.nafomonth <- transform(sampsize.nafomonth,
                     mmsp = factor(mmsp,levels = c(
                       "Harp seal",
                       "Ringed seal",
                       "Hooded seal",
                       "Bearded seal")))

ssp <- ggplot(sampsize.nafomonth, aes(x = month,y = n,width = 0.7))
ssp <- ssp + scale_x_discrete("Month", limit = c(1:12))#, breaks = c(1:12), labels = c(1:12))
#ssp <- ssp + scale_x_date(as.Date(month), xlim = c(1,12))
ssp <- ssp + geom_bar(stat = 'identity')
ssp <- ssp + theme_bw()
ssp <- ssp + facet_grid(mmsp ~ nafo, drop = TRUE,scales = "free_y")
ssp


