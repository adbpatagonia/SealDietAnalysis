library(dplyr)

morph <- read.csv('input/morph_data.csv', header = T)


## subsetting parameters    ----
mammalsp <- c(1,2,5,6)
nafodiet <- c('2H','2J','3K')
dietby <-  c('year', 'mmsp')


## look only at data from main stomach and nafo Divs ----
diet <- diet[which(diet$digestivetractsection == 'Main Stomach'),]
diet <- diet[which(diet$nafo %in% nafodiet),]
diet <- diet[which(diet$codemmsp %in% mammalsp),]

## merge datasets ----
md <- merge(diet[!duplicated(diet$idsex), c('idsex', 'nafo', 'year')], 
            select(morph, -year), 
            by = 'idsex')
md <- subset(md, year < 2007)

## reorder 
md$mmsp <- ifelse(md$codemmsp == 1, 'Harp seal',
                       ifelse(md$codemmsp == 2, 'Hooded seal',
                              ifelse(md$codemmsp == 5, 'Ringed seal',
                                     ifelse(md$codemmsp == 6, 'Bearded seal','flag'))))


md <- transform(md,
                     mmsp=factor(mmsp,levels=c(
                       "Harp seal", 
                       "Ringed seal",
                       "Hooded seal",
                       "Bearded seal")))


p <- ggplot(md, aes( factor(year), mmweight))
p <- p + geom_violin()
p <- p + facet_grid(mmsp~., drop = TRUE, scales = 'free_y')
#p <- p + stat_summary(aes(factor(year)), fun.y = median, geom = "point", fill = "red", shape = 21, size = 1.5)
p <- p + theme_bw()
p

