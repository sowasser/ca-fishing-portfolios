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


# Kruskal-Wallis test for species within each area ------------------------
# Eureka
e_means <- stable_area_means %>% filter(area == "Eureka")
e_means <- melt(e_means, id.vars = c("Species", "area"))
e_means$Species <- as.factor(e_means$Species)

kruskal.test(e_means$value ~ e_means$Species)  # p = 1.785e-08****
posthoc.kruskal.nemenyi.test(e_means$value ~ e_means$Species)

# Fort Bragg
fb_means <- stable_area_means %>% filter(area == "Fort Bragg")
fb_means <- melt(fb_means, id.vars = c("Species", "area"))
fb_means$Species <- as.factor(fb_means$Species)

kruskal.test(fb_means$value ~ fb_means$Species)  # p = 1.837e-12****
posthoc.kruskal.nemenyi.test(fb_means$value ~ fb_means$Species)

# Bodega Bay
bb_means <- stable_area_means %>% filter(area == "Bodega Bay")
bb_means <- melt(bb_means, id.vars = c("Species", "area"))
bb_means$Species <- as.factor(bb_means$Species)

kruskal.test(bb_means$value ~ bb_means$Species)  # p = 1.542e-10****
posthoc.kruskal.nemenyi.test(bb_means$value ~ bb_means$Species)

# San Francisco
sf_means <- stable_area_means %>% filter(area == "San Francisco")
sf_means <- melt(sf_means, id.vars = c("Species", "area"))
sf_means$Species <- as.factor(sf_means$Species)

kruskal.test(sf_means$value ~ sf_means$Species)  # p = 0.0004046****
posthoc.kruskal.nemenyi.test(sf_means$value ~ sf_means$Species)

# Monterey
m_means <- stable_area_means %>% filter(area == "Monterey")
m_means <- melt(m_means, id.vars = c("Species", "area"))
m_means$Species <- as.factor(m_means$Species)

kruskal.test(m_means$value ~ m_means$Species)  # p = 2.144e-12****
posthoc.kruskal.nemenyi.test(m_means$value ~ m_means$Species)

# Morro Bay
mb_means <- stable_area_means %>% filter(area == "Morro Bay")
mb_means <- melt(mb_means, id.vars = c("Species", "area"))
mb_means$Species <- as.factor(mb_means$Species)

kruskal.test(mb_means$value ~ mb_means$Species)  # p = 2.793e-08****
posthoc.kruskal.nemenyi.test(mb_means$value ~ mb_means$Species)

# Santa Barbara
sb_means <- stable_area_means %>% filter(area == "Santa Barbara")
sb_means <- melt(sb_means, id.vars = c("Species", "area"))
sb_means$Species <- as.factor(sb_means$Species)

kruskal.test(sb_means$value ~ sb_means$Species)  # p < 2.2e-16****
posthoc.kruskal.nemenyi.test(sb_means$value ~ sb_means$Species)

# Los Angeles
la_means <- stable_area_means %>% filter(area == "Los Angeles")
la_means <- melt(la_means, id.vars = c("Species", "area"))
la_means$Species <- as.factor(la_means$Species)

kruskal.test(la_means$value ~ la_means$Species)  # p = 7.993e-15****
posthoc.kruskal.nemenyi.test(la_means$value ~ la_means$Species)

# San Diego
sd_means <- stable_area_means %>% filter(area == "San Diego")
sd_means <- melt(sd_means, id.vars = c("Species", "area"))
sd_means$Species <- as.factor(sd_means$Species)

kruskal.test(sd_means$value ~ sd_means$Species)  # p = 2.924e-13****
posthoc.kruskal.nemenyi.test(sd_means$value ~ sd_means$Species)
