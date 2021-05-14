# Analysis of the data from the nine areas within California, gathered from the
# California Department of Natural Resources, accessed here: 
# https://wildlife.ca.gov/Fishing/Commercial/Landings

library(stringr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(viridis)

fishyears <- read.csv("Data/DFW areas/fisheries_year_soi.csv")
fishyears$month <- factor(fishyears$month, levels = c("Nov", "Dec", "Jan", 
                                                      "Feb", "Mar", "Apr", 
                                                      "May", "Jun", "Jul",
                                                      "Aug", "Sep", "Oct"))


# Order of areas from North -> South
area_order <- c("Eureka", "Fort Bragg", "Bodega Bay", "San Francisco", 
                "Monterey", "Morro Bay", "Santa Barbara", "Los Angeles",
                "San Diego")

# Abbreviation of months for nice plots
months_abbrev <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                   "Sep", "Oct", "Nov", "Dec")

original_foi <- c("Market Squid", "Pelagics", "Dungeness Crab", "Red Sea Urchin",
                  "Ocean Shrimp", "Herring Roe", "Dover Sole_Thornyhead_Sablefish",
                  "Pacific Whiting", "Other Groundfish", "Salmon")
new_foi <- c("squid", "pelagics", "dungneness crab", "urchin", "shrimp", 
             "herring roe", "DSTS", "whiting", "groundfish", "salmon")

# Species of interest ordered by highest landings 
species_order <- c("Market Squid", "Pelagics", "Dungeness Crab",
                   "Red Sea Urchin", "Ocean Shrimp", "Herring Roe", 
                   "Dover Sole_Thornyhead_Sablefish", "Pacific Whiting",
                   "Other Groundfish", "Salmon", "Yellowfin_Skipjack", 
                   "Spiny Lobster", "Hagfish", "Pacific Bonito", "Rock Crab", 
                   "Swordfish", "Ridgeback Prawn", "Albacore Tuna", 
                   "Bigeye Tuna", "Halibut", "Spot Prawn", "Opah", "other")
# Top species of interest ordered by highest landings
top_species_order <- c("Market Squid", "Pelagics", "Dungeness Crab", 
                       "Red Sea Urchin", "Ocean Shrimp", "Herring Roe", 
                       "Dover Sole_Thornyhead_Sablefish", "Pacific Whiting",
                       "Other Groundfish", "Salmon")


# All landings ----------------------------------------------------------------
area_means <- fishyears %>%
  group_by(area, month) %>%
  summarize(landings = mean(landings, na.rm = TRUE))

area_means$area <- factor(area_means$area, levels = area_order)

area_mean_landings <- ggplot(area_means, aes(y = landings, x = month)) +
  geom_bar(stat = "identity") +
  ylab("mean landings (lbs)") + xlab(" ") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~area, ncol = 3, scale = "free")

ggsave(filename="Monthly pdf data/Figures/all_landings.pdf", area_mean_landings,
       width=400, height=250, units="mm", dpi=300)


# Find monthly mean for each year for the top species of interest -------------
top_foi <- fishyears %>%
  filter(str_detect(species, "Market Squid|Pelagics|Dover Sole_Thornyhead_Sablefish|Pacific Whiting|Other Groundfish|Dungeness Crab|Red Sea Urchin|Ocean Shrimp|Herring Roe|Salmon"))

top_means <- top_foi %>% 
  group_by(species, area, month) %>%
  summarize(landings = mean(landings, na.rm = TRUE))
top_means$species <- factor(top_means$species, levels = original_foi, labels = new_foi)
top_means$area <- factor(top_means$area, levels = area_order)

# Plot monthly means for stable period
monthly_areas <- ggplot(top_means, aes(y = landings, x = month, fill = species)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("mean landings (lbs)") + xlab(" ") +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~area, ncol = 3, scale = "free")

ggsave(filename="Monthly pdf data/Figures/area_monthly_landings.pdf", monthly_areas,
       width=350, height=220, units="mm", dpi=300)


# Overall monthly trends for species of interest ------------------------------
overall_means <- fishyears %>%
  group_by(species, year, month) %>%
  summarize(landings = sum(landings, na.rm = TRUE)) %>%
  group_by(species, month) %>%
  summarize(landings = mean(landings, na.rm = TRUE))

overall_means$species <- factor(overall_means$species, 
                                levels = c("Market Squid", "Pelagics", "Dungeness Crab",
                                           "Red Sea Urchin", "Ocean Shrimp", "Herring Roe", 
                                           "Dover Sole_Thornyhead_Sablefish", "Pacific Whiting",
                                           "Other Groundfish", "Salmon", "Yellowfin_Skipjack", 
                                           "Spiny Lobster", "Hagfish", "Pacific Bonito", "Rock Crab", 
                                           "Swordfish", "Ridgeback Prawn", "Albacore Tuna", 
                                           "Bigeye Tuna", "Halibut", "Spot Prawn", "Opah", "other"),
                                labels = c("Market Squid", "Pelagics", "Dungeness Crab",
                                           "Red Sea Urchin", "Ocean Shrimp", "Herring Roe", 
                                           "Dover Sole/Thornyhead/Sablefish", "Pacific Whiting",
                                           "Other Groundfish", "Salmon", "Yellowfin/Skipjack", 
                                           "Spiny Lobster", "Hagfish", "Pacific Bonito", "Rock Crab", 
                                           "Swordfish", "Ridgeback Prawn", "Albacore Tuna", 
                                           "Bigeye Tuna", "Halibut", "Spot Prawn", "Opah", "other"))

monthly_species <- ggplot(overall_means, aes(y = landings, x = month)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("mean landings (lbs)") + xlab(" ") +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~species, ncol = 4, scale = "free")

ggsave(filename="Monthly pdf data/Figures/monthly_species.pdf", monthly_species,
       width=400, height=350, units="mm", dpi=300)
