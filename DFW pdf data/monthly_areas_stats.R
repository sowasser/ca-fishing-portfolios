# Script for stats on species parings (complimentary vs. substitutional) for 
# the DFW data, aggregated by area and month, for the species of interest.

library(stringr)
library(dplyr)
library(reshape2)
library(PMCMR)

all_soi <- read.csv("Data/dfw_areas_soi.csv")

# Kruskal-Wallis test for the mean by year for entire period -----------------
year_means <- all_soi %>% 
  group_by(Species, year) %>% 
  summarize(across(January:Landings, mean))

year_means <- year_means[, -15]  # remove total landings

# Dungeness Crab
crab_year_means <- year_means %>% filter(str_detect(Species, "Dungeness"))
crab_year_means <- melt(crab_year_means[, -1], id.vars = "year")
crab_year_means <- crab_year_means[, -2]

kruskal.test(crab_year_means$value ~ crab_year_means$year)  # p = 0.1225

# Salmon
salmon_means <- year_means %>% filter(str_detect(Species, "Salmon"))
salmon_year_means <- melt(salmon_means[, -1], id.vars = "year")
salmon_year_means <- salmon_year_means[, -2]

kruskal.test(salmon_year_means$value ~ salmon_year_means$year)  # p = 0.027729*
posthoc.kruskal.nemenyi.test(salmon_year_means$value ~ salmon_year_means$year,
                             dist = "Tukey")

# Groundfish
groundfish_means <- year_means %>% filter(str_detect(Species, "Groundfish"))
groundfish_year_means <- melt(groundfish_means[, -1], id.vars = "year")
groundfish_year_means <- groundfish_year_means[, -2]

kruskal.test(groundfish_year_means$value ~ groundfish_year_means$year)  # p = 0.0006661***

# Squid
squid_means <- year_means %>% filter(str_detect(Species, "Squid"))
squid_year_means <- melt(squid_means[, -1], id.vars = "year")
squid_year_means <- squid_year_means[, -2]

kruskal.test(squid_year_means$value ~ squid_year_means$year)  # p = 0.03281*


# Kruskal-Wallis test for the mean for each area across the stable period ----
# Information on Nemenyi post-hoc test here: 
# https://www.rdocumentation.org/packages/PMCMR/versions/4.3/topics/posthoc.kruskal.nemenyi.test
all_soi_stable <- all_soi %>% filter(between(year, 2009, 2014))

stable_area_means <- all_soi_stable %>% 
  group_by(Species, area) %>% 
  summarize(across(January:Landings, mean))

stable_area_means <- stable_area_means[, -15]  # remove total landings

# Dungeness Crab
crab_means <- stable_area_means %>% filter(str_detect(Species, "Dungeness"))
crab_area_means <- melt(crab_means[, -1], id.vars = "area")
crab_area_means <- crab_area_means[, -2]
crab_area_means$area <- as.factor(crab_area_means$area)

kruskal.test(crab_area_means$value ~ crab_area_means$area)  # p = 6.097e-09****
posthoc.kruskal.nemenyi.test(crab_area_means$value ~ crab_area_means$area)

# Salmon
salmon_means <- stable_area_means %>% filter(str_detect(Species, "Salmon"))
salmon_area_means <- melt(salmon_means[, -1], id.vars = "area")
salmon_area_means <- salmon_area_means[, -2]
salmon_area_means$area <- as.factor(salmon_area_means$area)

kruskal.test(salmon_area_means$value ~ salmon_area_means$area)  # p = 0.5133

# Groundfish
groundfish_means <- stable_area_means %>% filter(str_detect(Species, "Groundfish"))
groundfish_area_means <- melt(groundfish_means[, -1], id.vars = "area")
groundfish_area_means <- groundfish_area_means[, -2]
groundfish_area_means$area <- as.factor(groundfish_area_means$area)

kruskal.test(groundfish_area_means$value ~ groundfish_area_means$area)  # p = 1.714e-15****
posthoc.kruskal.nemenyi.test(groundfish_area_means$value ~ groundfish_area_means$area)

# Squid
squid_means <- stable_area_means %>% filter(str_detect(Species, "Squid"))
squid_area_means <- melt(squid_means[, -1], id.vars = "area")
squid_area_means <- squid_area_means[, -2]
squid_area_means$area <- as.factor(squid_area_means$area)

kruskal.test(squid_area_means$value ~ squid_area_means$area)  # p = 1.492e-11****
posthoc.kruskal.nemenyi.test(squid_area_means$value ~ squid_area_means$area)
