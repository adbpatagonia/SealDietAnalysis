source('analysis/01_calculate_length_weight.R')
library(tidyverse)

diet %>%
  filter(codemeasuredestimated == 1 & is.na(pMVmm) & !is.na(length)) %>%
  select(idsex,  preycode, codemeasuredestimated, pMVmm, length, weight, idpreyitem) %>%
  write.csv(file = 'output/IncorrectCodeForMeasurement_july2019.csv', row.names = FALSE)
