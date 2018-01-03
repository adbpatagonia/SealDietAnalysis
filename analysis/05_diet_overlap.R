## ADB July 28, 2017.
## The intent of this script is to 
## compare the LF distributions of fish in harp and ringed seal diets

## read data  ----
setwd('D:/Buren_files/GitHub/DietAnalysis/')
load('interimsteps/diet.rdata')


## load libraries  ----
library(dplyr)
#library(ggplot2)
library(cowplot)
library(tidyr)
library(spaa)
library(stringr)

percbio$mmsp <- percbio$mmsp %>%
                   str_replace(" seal", "")

dietmatrix <- percbio %>%
  select(-totalpreyweight.sum, -totalweight) %>%
  spread(key = mmsp, value = percbio) %>%
  select(-preycat, -year)

dietmatrix[is.na(dietmatrix)] <- 0

pianka <- niche.overlap.boot(dietmatrix, method = "pianka", times = 10000)
# schoener <- niche.overlap.boot(dietmatrix, method = "schoener", times = 10000)
# czech <- niche.overlap.boot(dietmatrix, method = "czech", times = 10000) 
# levins <- niche.overlap.boot(dietmatrix, method = "levins", times = 10000)
# petraitis <- niche.overlap.boot(dietmatrix, method = "petraitis", times = 10000)
# morisita <- niche.overlap.boot(dietmatrix, method = "morisita", times = 10000)


pianka <- data.frame(pair = dimnames(pianka)[[1]], as_tibble(pianka))
#morisita <- data.frame(pair = dimnames(morisita)[[1]], as_tibble(morisita))

# I <- order(pianka$Observed)
# 
# m <- ggplot(morisita, aes(y = pair, x = Boot.mean)) 
# m <- m + geom_point()
# m <- m + geom_errorbarh(aes(xmin = Boot.CI1, xmax = Boot.CI2, height = 0))
# m <- m + theme_set(theme_cowplot())
# m <- m + labs(y = '', x = 'Diet overlap')
# m



p <- ggplot(pianka, aes(y = pair, x = Boot.mean)) 
p <- p + geom_point(size = 4)
p <- p + geom_errorbarh(aes(xmin = Boot.CI1, xmax = Boot.CI2, height = 0), size =2, show.legend = NA)
p <- p + theme_set(theme_cowplot())
p <- p + labs(y = '', x = 'Diet overlap')
p <- p + ggtitle("Pairwise diet Overlap, Pianka's index")
p <- p +  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0),
#                strip.text = element_text(size = 14, face = "bold"),
#                legend.text=element_text(size = 13),
                axis.text=element_text(size = 15),
                axis.title=element_text(size = 18, face="bold")
)

p

save_plot("output/diet_overlap_cuadrado.png", p, base_width = 10, base_height = 10)#, dpi = 900) # make room for figure legend)
