# Script for stats on species parings (complimentary vs. substitutional) for 
# the DFW data, aggregated by area and month, for the species of interest.

library(stringr)
library(dplyr)
library(reshape2)


all_soi <- read.csv("Data/dfw_areas_soi.csv")

# Isolate the stable period ---------------------------------------------------
all_soi_stable <- all_soi %>% filter(between(year, 2009, 2014))


# Kruskall-Wallis test for the mean for each area across the stable period ----
stable_area_means <- all_soi_stable %>% 
  group_by(Species, area) %>% 
  summarize(across(January:Landings, mean))

stable_area_means <- stable_area_means[, -15]  # remove total landings

crab_means <- stable_area_means %>% filter(str_detect(Species, "Dungeness"))
crab_area_means <- melt(crab_means[, -1], id.vars = "area")
crab_area_means <- crab_area_means[, -2]

kruskal.test(crab_area_means$value ~ crab_area_means$area)


# Kruskall-Wallis test for the mean by year across the stable period ----------
stable_year_means <- all_soi_stable %>% 
  group_by(Species, year) %>% 
  summarize(across(January:Landings, mean))

stable_year_means <- stable_year_means[, -15]  # remove total landings

crab_year_means <- stable_year_means %>% filter(str_detect(Species, "Dungeness"))
crab_year_means <- melt(crab_year_means[, -1], id.vars = "year")
crab_year_means <- crab_year_means[, -2]

kruskal.test(crab_year_means$value ~ crab_year_means$year)
