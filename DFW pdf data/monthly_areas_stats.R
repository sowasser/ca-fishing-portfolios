# Script for stats on species parings (complimentary vs. substitutional) for 
# the DFW data, aggregated by area and month, for the species of interest.

all_soi <- read.csv("Data/dfw_areas_soi.csv")

all_soi_stable <- all_soi %>% filter(between(year, 2009, 2014))

stable_means <- all_soi_stable %>% 
  group_by(Species, area) %>% 
  summarize(across(January:Landings, mean))

