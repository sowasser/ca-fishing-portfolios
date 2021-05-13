# Script for stats on species parings (complimentary vs. substitutional) for 
# the DFW data, aggregated by area and month, for the species of interest.

# Information on Nemenyi post-hoc test here: 
# https://www.rdocumentation.org/packages/PMCMR/versions/4.3/topics/posthoc.kruskal.nemenyi.test

library(stringr)
library(dplyr)
library(reshape2)
library(PMCMRplus)
library(kSamples)
library(ggplot2)
library(viridis)

all_soi <- read.csv("Data/dfw_areas_all_soi.csv")
top_soi <- read.csv("Data/dfw_areas_top_soi.csv")

species <- c("Market Squid", "Coastal Pelagics", 
             "Dover Sole/Thornyhead/Sablefish", "Pacific Whiting", 
             "Other Groundfish", "Dungeness Crab", "Red Sea Urchin", 
             "Ocean Shrimp", "Herring Roe", "Salmon")

# Kruskal-Wallis test for the mean by year for top SOI ------------------------
year_means <- all_soi %>% 
  group_by(Species, year) %>% 
  summarize(across(January:Landings, mean, na.rm = TRUE))
year_means <- year_means[, -15]  # remove total landings

# Squid
squid_means <- year_means %>% filter(str_detect(Species, "Squid"))
squid_year_means <- melt(squid_means[, -1], id.vars = "year")
squid_year_means <- squid_year_means[, -2]
kruskal.test(squid_year_means$value ~ squid_year_means$year)  # p = 0.03281*

# Pelagics 
pelagics_means <- year_means %>% filter(str_detect(Species, "Pelagics"))
pelagics_year_means <- melt(pelagics_means[, -1], id.vars = "year")
pelagics_year_means <- pelagics_year_means[, -2]
kruskal.test(pelagics_year_means$value ~ pelagics_year_means$year)  # p = 1.579e-14****

# Dover Sole / Thornyhead / Sablefish 
dsts_means <- year_means %>% filter(str_detect(Species, "Dover Sole_Thornyhead_Sablefish"))
dsts_year_means <- melt(dsts_means[, -1], id.vars = "year")
dsts_year_means <- dsts_year_means[, -2]
kruskal.test(dsts_year_means$value ~ dsts_year_means$year)  # p = 1.177e-05****

# Pacific Whiting
whiting_means <- year_means %>% filter(str_detect(Species, "Whiting"))
whiting_year_means <- melt(whiting_means[, -1], id.vars = "year")
whiting_year_means <- whiting_year_means[, -2]
kruskal.test(whiting_year_means$value ~ whiting_year_means$year)  # p = 0.1134

# Other Groundfish
groundfish_means <- year_means %>% filter(str_detect(Species, "Other Groundfish"))
groundfish_year_means <- melt(groundfish_means[, -1], id.vars = "year")
groundfish_year_means <- groundfish_year_means[, -2]
kruskal.test(groundfish_year_means$value ~ groundfish_year_means$year)  # p = 4.141e-14****

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
kruskal.test(salmon_year_means$value ~ salmon_year_means$year)  # p = 0.02781*


# Kruskal-Wallis test for the mean for each area across the stable period ----
area_means <- all_soi %>% 
  group_by(Species, area) %>% 
  summarize(across(January:Landings, mean, na.rm = TRUE))
area_means <- area_means[, -15]  # remove total landings

# Squid
squid_area_means <- area_means %>% filter(str_detect(Species, "Squid"))
squid_area_means <- melt(squid_area_means[, -1], id.vars = "area")
squid_area_means <- squid_area_means[, -2]
kruskal.test(squid_area_means$value ~ squid_area_means$area)  # p = 1.032e-13****

# Pelagics 
pelagics_area_means <- area_means %>% filter(str_detect(Species, "Pelagics"))
pelagics_area_means <- melt(pelagics_area_means[, -1], id.vars = "area")
pelagics_area_means <- pelagics_area_means[, -2]
kruskal.test(pelagics_area_means$value ~ pelagics_area_means$area)  # p = 2.447e-16****

# Dover Sole / Thornyhead / Sablefish 
dsts_area_means <- area_means %>% filter(str_detect(Species, "Dover Sole_Thornyhead_Sablefish"))
dsts_area_means <- melt(dsts__area_means[, -1], id.vars = "area")
dsts_area_means <- dsts_area_means[, -2]
kruskal.test(dsts_area_means$value ~ dsts_area_means$area)  # p < 2.2e-16****

# Pacific Whiting
whiting_area_means <- area_means %>% filter(str_detect(Species, "Whiting"))
whiting_area_means <- melt(whiting_area_means[, -1], id.vars = "area")
whiting_area_means <- whiting_area_means[, -2]
kruskal.test(whiting_area_means$value ~ whiting_area_means$area)  # p = 5.185e-10****

# Other Groundfish
groundfish_area_means <- area_means %>% filter(str_detect(Species, "Other Groundfish"))
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
  summarize(across(January:Landings, mean, na.rm = TRUE))
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
total_means_ad <- list(c(colMeans(squid_means[, -c(1, 2)], na.rm = TRUE)),
                       c(colMeans(pelagics_means[, -c(1, 2)], na.rm = TRUE)),
                       c(colMeans(dsts_means[, -c(1, 2)], na.rm = TRUE)),
                       c(colMeans(whiting_means[, -c(1, 2)], na.rm = TRUE)),
                       c(colMeans(groundfish_means[, -c(1, 2)], na.rm = TRUE)),
                       c(colMeans(crab_means[, -c(1, 2)], na.rm = TRUE)),
                       c(colMeans(urchin_means[, -c(1, 2)], na.rm = TRUE)),
                       c(colMeans(shrimp_means[, -c(1, 2)], na.rm = TRUE)),
                       c(colMeans(roe_means[, -c(1, 2)], na.rm = TRUE)),
                       c(colMeans(salmon_means[, -c(1, 2)], na.rm = TRUE)))

ad.test(total_means_ad)  # p = 2.213e-21 / 1.368e-21****

# Kruskal-Wallis for all means
total_means_kw <- rbind(c(colMeans(squid_means[, -c(1, 2)], na.rm = TRUE)),
                        c(colMeans(pelagics_means[, -c(1, 2)], na.rm = TRUE)),
                        c(colMeans(dsts_means[, -c(1, 2)], na.rm = TRUE)),
                        c(colMeans(whiting_means[, -c(1, 2)], na.rm = TRUE)),
                        c(colMeans(groundfish_means[, -c(1, 2)], na.rm = TRUE)),
                        c(colMeans(crab_means[, -c(1, 2)], na.rm = TRUE)),
                        c(colMeans(urchin_means[, -c(1, 2)], na.rm = TRUE)),
                        c(colMeans(shrimp_means[, -c(1, 2)], na.rm = TRUE)),
                        c(colMeans(roe_means[, -c(1, 2)], na.rm = TRUE)),
                        c(colMeans(salmon_means[, -c(1, 2)], na.rm = TRUE)))
rownames(total_means_kw) <- species
total_means_kw <- melt(total_means_kw)
colnames(total_means_kw) <- c("species", "month", "landings")
total_means_kw$species <- as.factor(total_means_kw$species)

kruskal.test(total_means_kw$landings ~ total_means_kw$species)  # p = 2.738e-12****
kwAllPairsNemenyiTest(total_means_kw$landings ~ total_means_kw$species)


# Correlation between species
total_means_cr <- cbind(c(colMeans(squid_means[, -c(1, 2)], na.rm = TRUE)),
                        c(colMeans(pelagics_means[, -c(1, 2)], na.rm = TRUE)),
                        c(colMeans(dsts_means[, -c(1, 2)], na.rm = TRUE)),
                        c(colMeans(whiting_means[, -c(1, 2)], na.rm = TRUE)),
                        c(colMeans(groundfish_means[, -c(1, 2)], na.rm = TRUE)),
                        c(colMeans(crab_means[, -c(1, 2)], na.rm = TRUE)),
                        c(colMeans(urchin_means[, -c(1, 2)], na.rm = TRUE)),
                        c(colMeans(shrimp_means[, -c(1, 2)], na.rm = TRUE)),
                        c(colMeans(roe_means[, -c(1, 2)], na.rm = TRUE)),
                        c(colMeans(salmon_means[, -c(1, 2)], na.rm = TRUE)))
colnames(total_means_cr) <- species

# Get correlation & round to 2 decimal places for plot
cor(total_means_cr, method = "spearman")
species_cor <- round(cor(total_means_cr, method = "spearman"), 2)

species_cor_plot <- ggplot(melt(species_cor), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(height = 0.8, width = 0.8) +
  scale_fill_gradient2(low="blue", mid="white", high="red", limits = c(-1, 1)) +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme_minimal() +
  coord_equal() +
  labs(x =" ", y = "", fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, margin = margin(-3,0,0,0)),
        axis.text.y = element_text(margin = margin(0, -3, 0, 0)),
        panel.grid.major = element_blank())

ggsave(filename = "Monthly pdf data/Figures/species_correlations.pdf", 
       plot = species_cor_plot, width = 195, height = 160, units = "mm", dpi = 300)


# Density plot of top species of interest
# TODO: FIX THIS
total_means_den <- total_means_kw[, c(11, 12, 1:10) ]  # re-order months
colnames(total_means_den) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
total_means_den <- melt(total_means_den)
colnames(total_means_den) <- c("species", "month", "landings")

species_overlap <- ggplot(total_means_den, aes(x = month, y = landings, fill = species)) +
  theme_bw() +
  geom_area(position = "identity", alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7 ,8 ,9, 10, 11, 12), 
                     labels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", 
                                "May", "Jun", "Jul", "Aug", "Sep", "Oct")) +
  xlab(" ") + ylab("mean landings") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename = "Monthly pdf data/Figures/species_overlap.pdf", 
       plot = species_overlap, width = 200, height = 130, units = "mm", dpi = 300)
