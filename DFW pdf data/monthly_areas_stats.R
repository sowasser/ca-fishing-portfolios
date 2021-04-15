# Script for stats on species parings (complimentary vs. substitutional) for 
# the DFW data, aggregated by area and month, for the species of interest.

library(stringr)
library(dplyr)
library(reshape2)
library(PMCMRplus)
library(kSamples)
library(ggcorrplot)

all_soi <- read.csv("Data/dfw_areas_all_soi.csv")
top_soi <- read.csv("Data/dfw_areas_top_soi.csv")

# Kruskal-Wallis test for the mean by year for top SOI ------------------------
# Information on Nemenyi post-hoc test here: 
# https://www.rdocumentation.org/packages/PMCMR/versions/4.3/topics/posthoc.kruskal.nemenyi.test
year_means <- all_soi %>% 
  group_by(Species, year) %>% 
  summarize(across(January:Landings, mean))
year_means <- year_means[, -15]  # remove total landings

# Squid
squid_means <- year_means %>% filter(str_detect(Species, "Squid"))
squid_year_means <- melt(squid_means[, -1], id.vars = "year")
squid_year_means <- squid_year_means[, -2]
kruskal.test(squid_year_means$value ~ squid_year_means$year)  # p = 1.579e-14****

# Pelagics 
pelagics_means <- year_means %>% filter(str_detect(Species, "Pelagics"))
pelagics_year_means <- melt(pelagics_means[, -1], id.vars = "year")
pelagics_year_means <- pelagics_year_means[, -2]
kruskal.test(pelagics_year_means$value ~ pelagics_year_means$year)  # p = 0.03281*

# Groundfish
groundfish_means <- year_means %>% filter(str_detect(Species, "Groundfish"))
groundfish_year_means <- melt(groundfish_means[, -1], id.vars = "year")
groundfish_year_means <- groundfish_year_means[, -2]
kruskal.test(groundfish_year_means$value ~ groundfish_year_means$year)  # p = 1.7e-07****

# Dungeness Crab
crab_means <- year_means %>% filter(str_detect(Species, "Dungeness"))
crab_year_means <- melt(crab_means[, -1], id.vars = "year")
crab_year_means <- crab_year_means[, -2]
kruskal.test(crab_year_means$value ~ crab_year_means$year)  # p = 0.6775

# Red Sea Urchin
urchin_means <- year_means %>% filter(str_detect(Species, "Urchin"))
urchin_year_means <- melt(urchin_means[, -1], id.vars = "year")
urchin_year_means <- urchin_year_means[, -2]
kruskal.test(urchin_year_means$value ~ urchin_year_means$year)  # p < 2.2e-16****

# Ocean Shrimp
shrimp_means <- year_means %>% filter(str_detect(Species, "Ocean Shrimp"))
shrimp_year_means <- melt(shrimp_means[, -1], id.vars = "year")
shrimp_year_means <- shrimp_year_means[, -2]
kruskal.test(shrimp_year_means$value ~ shrimp_year_means$year)  # p = 0.2062

# Herring Roe
roe_means <- year_means %>% filter(str_detect(Species, "Roe"))
roe_year_means <- melt(roe_means[, -1], id.vars = "year")
roe_year_means <- roe_year_means[, -2]
kruskal.test(roe_year_means$value ~ roe_year_means$year)  # p = .5099

# Salmon
salmon_means <- year_means %>% filter(str_detect(Species, "Salmon"))
salmon_year_means <- melt(salmon_means[, -1], id.vars = "year")
salmon_year_means <- salmon_year_means[, -2]
kruskal.test(salmon_year_means$value ~ salmon_year_means$year)  # p = 0.027729*


# Kruskal-Wallis test for the mean for each area across the stable period ----
area_means <- all_soi %>% 
  group_by(Species, area) %>% 
  summarize(across(January:Landings, mean))
area_means <- area_means[, -15]  # remove total landings

# Squid
squid_area_means <- area_means %>% filter(str_detect(Species, "Squid"))
squid_area_means <- melt(squid_area_means[, -1], id.vars = "area")
squid_area_means <- squid_area_means[, -2]
kruskal.test(squid_area_means$value ~ squid_area_means$area)  # p = 1.032e-1****

# Pelagics 
pelagics_area_means <- area_means %>% filter(str_detect(Species, "Pelagics"))
pelagics_area_means <- melt(pelagics_area_means[, -1], id.vars = "area")
pelagics_area_means <- pelagics_area_means[, -2]
kruskal.test(pelagics_area_means$value ~ pelagics_area_means$area)  # p = 2.447e-16****

# Groundfish
groundfish_area_means <- area_means %>% filter(str_detect(Species, "Groundfish"))
groundfish_area_means <- melt(groundfish_area_means[, -1], id.vars = "area")
groundfish_area_means <- groundfish_area_means[, -2]
kruskal.test(groundfish_area_means$value ~ groundfish_area_means$area)  # p < 2.2e-16****

# Dungeness Crab
crab_area_means <- area_means %>% filter(str_detect(Species, "Dungeness"))
crab_area_means <- melt(crab_area_means[, -1], id.vars = "area")
crab_area_means <- crab_area_means[, -2]
kruskal.test(crab_area_means$value ~ crab_area_means$area)  # p = 2.39e-08****

# Red Sea Urchin
urchin_area_means <- area_means %>% filter(str_detect(Species, "Urchin"))
urchin_area_means <- melt(urchin_area_means[, -1], id.vars = "area")
urchin_area_means <- urchin_area_means[, -2]
kruskal.test(urchin_area_means$value ~ urchin_area_means$area)  # p < 2.2e-16****

# Ocean Shrimp
shrimp_area_means <- area_means %>% filter(str_detect(Species, "Ocean Shrimp"))
shrimp_area_means <- melt(shrimp_area_means[, -1], id.vars = "area")
shrimp_area_means <- shrimp_area_means[, -2]
kruskal.test(shrimp_area_means$value ~ shrimp_area_means$area)  # p = 0.0008066****

# Herring Roe
roe_area_means <- area_means %>% filter(str_detect(Species, "Roe"))
roe_area_means <- melt(roe_area_means[, -1], id.vars = "area")
roe_area_means <- roe_area_means[, -2]
kruskal.test(roe_area_means$value ~ roe_area_means$area)  # p = 0.01987**

# Salmon
salmon_means <- area_means %>% filter(str_detect(Species, "Salmon"))
salmon_area_means <- melt(salmon_means[, -1], id.vars = "area")
salmon_area_means <- salmon_area_means[, -2]
kruskal.test(salmon_area_means$value ~ salmon_area_means$area)  # p = 0.0003448***


# Kruskal-Wallis test for the top SOI within each area ------------------------
top_means <- top_soi %>% 
  group_by(Species, area) %>% 
  summarize(across(January:Landings, mean))
top_means <- top_means[, -15]  # remove total landings

# Eureka
e_means <- top_means %>% filter(area == "Eureka")
e_means <- melt(e_means, id.vars = c("Species", "area"))
e_means$Species <- as.factor(e_means$Species)

kruskal.test(e_means$value ~ e_means$Species)  # p = 1.605e-07****
kwAllPairsNemenyiTest(e_means$value ~ e_means$Species)

# Fort Bragg
fb_means <- top_means %>% filter(area == "Fort Bragg")
fb_means <- melt(fb_means, id.vars = c("Species", "area"))
fb_means$Species <- as.factor(fb_means$Species)

kruskal.test(fb_means$value ~ fb_means$Species)  # p = 2.9e-11****
kwAllPairsNemenyiTest(fb_means$value ~ fb_means$Species)

# Bodega Bay
bb_means <- top_means %>% filter(area == "Bodega Bay")
bb_means <- melt(bb_means, id.vars = c("Species", "area"))
bb_means$Species <- as.factor(bb_means$Species)

kruskal.test(bb_means$value ~ bb_means$Species)  # p = 2.354e-05****
kwAllPairsNemenyiTest(bb_means$value ~ bb_means$Species)

# San Francisco
sf_means <- top_means %>% filter(area == "San Francisco")
sf_means <- melt(sf_means, id.vars = c("Species", "area"))
sf_means$Species <- as.factor(sf_means$Species)

kruskal.test(sf_means$value ~ sf_means$Species)  # p = 0.0001385****
kwAllPairsNemenyiTest(sf_means$value ~ sf_means$Species)

# Monterey
m_means <- top_means %>% filter(area == "Monterey")
m_means <- melt(m_means, id.vars = c("Species", "area"))
m_means$Species <- as.factor(m_means$Species)

kruskal.test(m_means$value ~ m_means$Species)  # p = 1.579e-14****
kwAllPairsNemenyiTest(m_means$value ~ m_means$Species)

# Morro Bay
mb_means <- top_means %>% filter(area == "Morro Bay")
mb_means <- melt(mb_means, id.vars = c("Species", "area"))
mb_means$Species <- as.factor(mb_means$Species)

kruskal.test(mb_means$value ~ mb_means$Species)  # p = 8.655e-07****
kwAllPairsNemenyiTest(mb_means$value ~ mb_means$Species)

# Santa Barbara
sb_means <- top_means %>% filter(area == "Santa Barbara")
sb_means <- melt(sb_means, id.vars = c("Species", "area"))
sb_means$Species <- as.factor(sb_means$Species)

kruskal.test(sb_means$value ~ sb_means$Species)  # p = 4.403e-14****
kwAllPairsNemenyiTest(sb_means$value ~ sb_means$Species)

# Los Angeles
la_means <- top_means %>% filter(area == "Los Angeles")
la_means <- melt(la_means, id.vars = c("Species", "area"))
la_means$Species <- as.factor(la_means$Species)

kruskal.test(la_means$value ~ la_means$Species)  # p = 5.427e-16****
kwAllPairsNemenyiTest(la_means$value ~ la_means$Species)

# San Diego
sd_means <- top_means %>% filter(area == "San Diego")
sd_means <- melt(sd_means, id.vars = c("Species", "area"))
sd_means$Species <- as.factor(sd_means$Species)

kruskal.test(sd_means$value ~ sd_means$Species)  # p = 4.756e-13****
kwAllPairsNemenyiTest(sd_means$value ~ sd_means$Species)


# Species distribution across months ------------------------------------------

# Anderson-Darling test to see if all of the species come from one dist.
total_means <- list(c(colMeans(squid_means[, -c(1, 2)], na.rm = TRUE)),
                     c(colMeans(pelagics_means[, -c(1, 2)], na.rm = TRUE)),
                     c(colMeans(groundfish_means[, -c(1, 2)], na.rm = TRUE)),
                     c(colMeans(urchin_means[, -c(1, 2)], na.rm = TRUE)),
                     c(colMeans(shrimp_means[, -c(1, 2)], na.rm = TRUE)),
                     c(colMeans(roe_means[, -c(1, 2)], na.rm = TRUE)),
                     c(colMeans(salmon_means[, -c(1, 2)], na.rm = TRUE)))

ad.test(total_means)  # p = 1.045e-16 / 7.000e-17

# Correlation between species
total_means2 <- cbind(c(colMeans(squid_means[, -c(1, 2)], na.rm = TRUE)),
                      c(colMeans(pelagics_means[, -c(1, 2)], na.rm = TRUE)),
                      c(colMeans(groundfish_means[, -c(1, 2)], na.rm = TRUE)),
                      c(colMeans(urchin_means[, -c(1, 2)], na.rm = TRUE)),
                      c(colMeans(shrimp_means[, -c(1, 2)], na.rm = TRUE)),
                      c(colMeans(roe_means[, -c(1, 2)], na.rm = TRUE)),
                      c(colMeans(salmon_means[, -c(1, 2)], na.rm = TRUE)))
colnames(total_means2) <- c("Market Squid", "Coastal Pelagics", "Groundfish",
                            "Red Sea Urchin", "Ocean Shrimp", "Herring Roe",
                            "Salmon")

species_cor <- cor(total_means2)

ggcorrplot(species_cor, hc.order = TRUE, type = "lower")
