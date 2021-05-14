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


fishyears <- read.csv("Data/DFW areas/fisheries_year_soi.csv")
fishyears$month <- factor(fishyears$month, levels = c("Nov", "Dec", "Jan", 
                                                      "Feb", "Mar", "Apr", 
                                                      "May", "Jun", "Jul",
                                                      "Aug", "Sep", "Oct"))

original_foi <- c("Market Squid", "Pelagics", "Dungeness Crab", "Red Sea Urchin",
                  "Ocean Shrimp", "Herring Roe", "Dover Sole_Thornyhead_Sablefish",
                  "Pacific Whiting", "Other Groundfish", "Salmon")
new_foi <- c("squid", "pelagics", "dungneness crab", "urchin", "shrimp", 
             "herring roe", "DSTS", "whiting", "groundfish", "salmon")


# Kruskal-Wallis test for the mean by year for top SOI ------------------------
all_ca <- fishyears %>%
  group_by(species, year, month) %>%
  summarize(landings = sum(landings, na.rm = TRUE))

# Squid
squid_allCA <- all_ca %>% filter(str_detect(species, "Squid"))
kruskal.test(squid_allCA$landings ~ squid_allCA$year)  # p = 0.03507*

# Pelagics 
pelagics_allCA <- all_ca %>% filter(str_detect(species, "Pelagics"))
kruskal.test(pelagics_allCA$landings ~ pelagics_allCA$year)  # p = 7.226e-16*

# Dover Sole / Thornyhead / Sablefish 
dsts_allCA <- all_ca %>% filter(str_detect(species, "Dover Sole_Thornyhead_Sablefish"))
kruskal.test(dsts_allCA$landings ~ dsts_allCA$year)  # p = 2.218e-05****

# Pacific Whiting
whiting_allCA <- all_ca %>% filter(str_detect(species, "Whiting"))
kruskal.test(whiting_allCA$landings ~ whiting_allCA$year)  # p = 0.02513*

# Other Groundfish
groundfish_allCA <- all_ca %>% filter(str_detect(species, "Other Groundfish"))
kruskal.test(groundfish_allCA$landings ~ groundfish_allCA$year)  # p = 4.45e-15****

# Dungeness Crab
crab_allCA <- all_ca %>% filter(str_detect(species, "Dungeness"))
kruskal.test(crab_allCA$landings ~ crab_allCA$year)  # p = 0.7351

# Red Sea Urchin
urchin_allCA <- all_ca %>% filter(str_detect(species, "Urchin"))
kruskal.test(urchin_allCA$landings ~ urchin_allCA$year)  # p < 2.2e-16****

# Ocean Shrimp
shrimp_allCA <- all_ca %>% filter(str_detect(species, "Ocean Shrimp"))
kruskal.test(shrimp_allCA$landings ~ shrimp_allCA$year)  # p = 0.2094

# Herring Roe
roe_allCA <- all_ca %>% filter(str_detect(species, "Roe"))
kruskal.test(roe_allCA$landings ~ roe_allCA$year)  # p = 0.5101

# Salmon
salmon_allCA <- all_ca %>% filter(str_detect(species, "Salmon"))
kruskal.test(salmon_allCA$landings ~ salmon_allCA$year)  # p = 0.01609*


# Kruskal-Wallis test for the mean for each area across the stable period ----
area_means <- fishyears %>%
  group_by(species, area, month) %>%
  summarize(landings = mean(landings, na.rm = TRUE))

# Squid
squid_area_means <- area_means %>% filter(str_detect(species, "Squid"))
kruskal.test(squid_area_means$landings ~ squid_area_means$area)  # p = 1.032e-13****

# Pelagics 
pelagics_area_means <- area_means %>% filter(str_detect(species, "Pelagics"))
kruskal.test(pelagics_area_means$landings ~ pelagics_area_means$area)  # p = 2.385e-16****

# Dover Sole / Thornyhead / Sablefish 
dsts_area_means <- area_means %>% filter(str_detect(species, "Dover Sole_Thornyhead_Sablefish"))
kruskal.test(dsts_area_means$landings ~ dsts_area_means$area)  # p < 2.2e-16****

# Pacific Whiting
whiting_area_means <- area_means %>% filter(str_detect(species, "Whiting"))
kruskal.test(whiting_area_means$landings ~ whiting_area_means$area)  # p = 5.626e-10****

# Other Groundfish
groundfish_area_means <- area_means %>% filter(str_detect(species, "Other Groundfish"))
kruskal.test(groundfish_area_means$landings ~ groundfish_area_means$area)  # p < 2.2e-16****

# Dungeness Crab
crab_area_means <- area_means %>% filter(str_detect(species, "Dungeness"))
kruskal.test(crab_area_means$landings ~ crab_area_means$area)  # p = 2.39e-08****

# Red Sea Urchin
urchin_area_means <- area_means %>% filter(str_detect(species, "Urchin"))
kruskal.test(urchin_area_means$landings ~ urchin_area_means$area)  # p < 2.2e-16****

# Ocean Shrimp
shrimp_area_means <- area_means %>% filter(str_detect(species, "Ocean Shrimp"))
kruskal.test(shrimp_area_means$landings ~ shrimp_area_means$area)  # p = 0.0008066****

# Herring Roe
roe_area_means <- area_means %>% filter(str_detect(species, "Roe"))
kruskal.test(roe_area_means$landings ~ roe_area_means$area)  # p = 0.01987*

# Salmon
salmon_area_means <- area_means %>% filter(str_detect(species, "Salmon"))
kruskal.test(salmon_area_means$landings ~ salmon_area_means$area)  # p = 0.0005064***


# Kruskal-Wallis test for the top fisheries of interest within each area ------
top_foi <- fishyears %>%
  filter(str_detect(species, "Market Squid|Pelagics|Dover Sole_Thornyhead_Sablefish|Pacific Whiting|Other Groundfish|Dungeness Crab|Red Sea Urchin|Ocean Shrimp|Herring Roe|Salmon"))

top_means <- top_foi %>% 
  group_by(species, area, month) %>%
  summarize(landings = mean(landings, na.rm = TRUE))
top_means$species <- factor(top_means$species, levels = original_foi, labels = new_foi)

# Eureka
e_means <- top_means %>% filter(area == "Eureka")
kruskal.test(e_means$landings ~ e_means$species)  # p = 8.244e-09****
kwAllPairsNemenyiTest(e_means$landings ~ e_means$species)

# Fort Bragg
fb_means <- top_means %>% filter(area == "Fort Bragg")
kruskal.test(fb_means$landings ~ fb_means$species)  # p = 8.313e-14****
kwAllPairsNemenyiTest(fb_means$landings ~ fb_means$species)

# Bodega Bay
bb_means <- top_means %>% filter(area == "Bodega Bay")
kruskal.test(bb_means$landings ~ bb_means$species)  # p = 1.581e-07****
kwAllPairsNemenyiTest(bb_means$landings ~ bb_means$species)

# San Francisco
sf_means <- top_means %>% filter(area == "San Francisco")
kruskal.test(sf_means$landings ~ sf_means$species)  # p = 1.859e-06****
kwAllPairsNemenyiTest(sf_means$landings ~ sf_means$species)

# Monterey
m_means <- top_means %>% filter(area == "Monterey")
kruskal.test(m_means$landings ~ m_means$species)  # p < 2.2e-16****
kwAllPairsNemenyiTest(m_means$landings ~ m_means$species)

# Morro Bay
mb_means <- top_means %>% filter(area == "Morro Bay")
kruskal.test(mb_means$landings ~ mb_means$species)  # p = 4.278e-09****
kwAllPairsNemenyiTest(mb_means$landings ~ mb_means$species)

# Santa Barbara
sb_means <- top_means %>% filter(area == "Santa Barbara")
kruskal.test(sb_means$landings ~ sb_means$species)  # p < 2.2e-16****
kwAllPairsNemenyiTest(sb_means$landings ~ sb_means$species)

# Los Angeles
la_means <- top_means %>% filter(area == "Los Angeles")
kruskal.test(la_means$landings ~ la_means$species)  # p < 2.2e-16****
kwAllPairsNemenyiTest(la_means$landings ~ la_means$species)

# San Diego
sd_means <- top_means %>% filter(area == "San Diego")
kruskal.test(sd_means$landings ~ sd_means$species)  # p < 2.2e-16****
kwAllPairsNemenyiTest(sd_means$landings ~ sd_means$species)


# Species distribution across months for all areas & years --------------------
allCA_foi_means <- all_ca %>%
  filter(str_detect(species, "Market Squid|Pelagics|Dover Sole_Thornyhead_Sablefish|Pacific Whiting|Other Groundfish|Dungeness Crab|Red Sea Urchin|Ocean Shrimp|Herring Roe|Salmon")) %>%
  group_by(species, month) %>%
  summarize(landings = mean(landings, na.rm = TRUE))

# Kruskal-Wallis for all means
allCA_foi_means$species <- as.factor(allCA_foi_means$species)
kruskal.test(allCA_foi_means$landings ~ allCA_foi_means$species)  # p = 2.403e-14****
kwAllPairsNemenyiTest(allCA_foi_means$landings ~ allCA_foi_means$species)

# Correlation between species
allCA_means_cr <- dcast(allCA_foi_means, month ~ species)
colnames(allCA_means_cr) <- c("month", "DSTS", "dungeness crab", "herring roe", 
                              "market squid", "ocean shrimp", 
                              "other groundfish", "Pacific whiting", "pelagics",
                              "red sea urchin", "salmon")

species_cor <- round(cor(allCA_means_cr[, -1], method = "spearman"), 2)

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
den_allCA_foi <- allCA_foi_means
levels(den_allCA_foi$month) <- 1:12
den_allCA_foi$month <- as.numeric(den_allCA_foi$month)
den_allCA_foi$species <- factor(den_allCA_foi$species, levels = original_foi, labels = new_foi)

species_overlap <- ggplot(den_allCA_foi, aes(x = month, y = landings, fill = species)) +
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
