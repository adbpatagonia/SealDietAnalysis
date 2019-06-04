## ADB
## May 13, 2019
## GBS will come to Vancouver to work on diet data
## prior to that I want to find those harp seals that consumed a lot of cod so GBS can have the data sheets checked


# libraries ----
library('tidyverse')

# load data ----
# this dataset is created in script analysis/01_calculate_length_weight.r
load('interimsteps/diet.rdata')

# find harps with lots of g morhua ----
diet %>%
  filter(mmspcode == 1) %>%
  filter(preycode == 438) %>%
  filter(totalpreyweight > 500) %>%
  ggplot( aes(x = totalpreyweight/1000)) +
  geom_histogram(aes(y=..density..), colour="black", ) +
  geom_density(aes(y=..density..)) +
#  scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3)) +
  ylab("Density") + xlab("Total Prey Weight (kg)") +
  theme_bw() + theme(plot.title=element_text(size=20),
                     axis.title.y=element_text(size = 16, vjust=+0.2),
                     axis.title.x=element_text(size = 16, vjust=-0.2),
                     axis.text.y=element_text(size = 14),
                     axis.text.x=element_text(size = 14),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())


dd <- diet %>%
  filter(mmspcode == 1) %>%
  filter(preycode == 438) %>%
  filter(totalpreyweight > 500)  %>%
  droplevels()

# export ----
write.csv(x = dd, file = 'output/Harps_LotsofGMorhua.csv', row.names = FALSE)
