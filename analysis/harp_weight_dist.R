## load libraries ----
library(dplyr)
library(cowplot)

## read data  ----
# morph
morph <- read.csv('data/qsel_Morph.csv', header = T)
# diet
load('interimsteps/diet.Rdata')

## define season ----
diet$season <- ifelse((diet$month > 3) & (diet$month < 10), 'S', 'W')

## remove years prior to 1985
diet <- subset(diet, year > 1984)

## subsetting parameters    ----
mammalsp <- c(1)
nafodiet <- c('3L','2J','3K')
dietby <-  c('year', 'nafo', 'area', 'season')


## look only at data from main stomach and nafo Divs ----
diet <- diet[which(diet$digestivetractsection == 'Main Stomach'),]
diet <- diet[which(diet$nafo %in% nafodiet),]
diet <- diet[which(diet$mmspcode %in% mammalsp),]

## merge datasets ----
md <- merge(diet[!duplicated(diet$idsex), c('idsex', 'nafo', 'year', 'area', 'season')],
            select(morph, -year, -nafo),
            by = 'idsex')

p <- ggplot(na.omit(md), aes(x = as.factor(year), y = mmweight))
p <- p + geom_violin()
p <- p  + xlab("Year") + ylab("Body weight (kg)")
p <- p + scale_x_discrete(breaks = c("1985", "1990", "1995", '2000', '2005', '2010', '2015'),
                          labels = c("1985", "1990", "1995", '2000', '2005', '2010', '2015'))#,
                          #limits = c(1985, 2016))
p <- p + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 270, hjust = 0.5))
p <- p + facet_grid(nafo ~ area + season, drop = TRUE)
p <- p + ggtitle("PG weight distribution")
p <- p + theme(plot.title = element_text(size = 15, face = "bold"),
                 strip.text = element_text(size = 12),
                 legend.text = element_text(size = 11),
                 axis.text = element_text(size = 10),
                 axis.title = element_text(size = 13, face = "bold"))
p
save_plot("output/harp_weight_dist.png", p, base_width = 21, base_height = 9)#, dpi = 900) # make room for figure legend)
