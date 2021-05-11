# Analysis of the data from the nine areas within California, gathered from the
# California Department of Natural Resources, accessed here: 
# https://wildlife.ca.gov/Fishing/Commercial/Landings

library(stringr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(viridis)


# Import datasets for all areas and for species of interest
all_areas <- read.csv("Data/DFW areas/all_areas.csv")
all_soi <- read.csv("Data/dfw_areas_all_soi.csv")
top_soi <- read.csv("Data/dfw_areas_top_soi.csv")


# Order of areas from North -> South
area_order <- c("Eureka", "Fort Bragg", "Bodega Bay", "San Francisco", 
                "Monterey", "Morro Bay", "Santa Barbara", "Los Angeles",
                "San Diego")

# Abbreviation of months for nice plots
months_abbrev <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                   "Sep", "Oct", "Nov", "Dec")

# Species of interest ordered by highest landings 
species_order <- c("Market Squid", "Pelagics", "Groundfish", "Dungeness Crab",
                   "Red Sea Urchin", "Ocean Shrimp", "Herring Roe", "Salmon", 
                   "Yellowfin_Skipjack", "Spiny Lobster", "Hagfish", 
                   "Pacific Bonito", "Rock Crab", "Swordfish", 
                   "Ridgeback Prawn", "Albacore Tuna", "Bigeye Tuna", 
                   "Spot Prawn", "Opah", "other")
# Top species of interest ordered by highest landings
top_species_order <- c("Market Squid", "Pelagics", "Groundfish", 
                       "Dungeness Crab","Red Sea Urchin", "Ocean Shrimp",
                       "Herring Roe", "Salmon")


# All landings ----------------------------------------------------------------
all_landings <- all_areas %>% group_by(area, year) %>% 
  summarize(across(January:Landings, sum))

all_landings <- all_landings[, -2]  # remove year column

all_means <- all_landings %>%
  group_by(area) %>%
  summarize(across(January:Landings, mean))

colnames(all_means) <- c("species", months_abbrev)
all_means <- all_means[, c(1, 12, 13, 2:11)]  # Year starts in Nov.
all_means <- melt(all_means, id_vars = c("area"))
colnames(all_means) <- c("area", "month", "landings")
all_means$area <- factor(all_means$area, levels = area_order)

all_mean_landings <- ggplot(all_means, aes(y = landings, x = month)) +
  geom_bar(stat = "identity") +
  ylab("mean landings (lbs)") + xlab(" ") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~area, ncol = 3, scale = "free")

ggsave(filename="Monthly pdf data/Figures/all_landings.pdf", all_mean_landings,
       width=400, height=250, units="mm", dpi=300)


# Find monthly mean for each year for the top species of interest -------------
top_soi2 <- top_soi[, -15]  # remove year column
top_soi2$Species <- str_trim(top_soi2$Species, side = "both")  # Remove extra white spaces

top_soi_means <- top_soi2 %>% 
  group_by(Species, area) %>% 
  summarize(across(January:Landings, mean, na.rm = TRUE))

top_soi_means <- top_soi_means[, -15]  # Remove total landings

colnames(top_soi_means) <- c("species", "area", months_abbrev)  # Update to abbreviated months

# Change column & row order to match analyses
top_soi_means <- top_soi_means[, c(1, 2, 13, 14, 3:12)]  # Year starts in Nov.
top_soi_means <- melt(top_soi_means, id_vars = c("area", "species"))
colnames(top_soi_means) <- c("species", "area", "month", "landings")
top_soi_means$area <- factor(top_soi_means$area, levels = area_order)
top_soi_means$species <- factor(top_soi_means$species, levels = top_species_order)

# Plot monthly means for stable period
monthly_areas <- ggplot(top_soi_means, aes(y = landings, x = month, fill = species)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("mean landings (lbs)") + xlab(" ") +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~area, ncol = 3, scale = "free")

ggsave(filename="Monthly pdf data/Figures/area_monthly_landings.pdf", monthly_areas,
       width=400, height=250, units="mm", dpi=300)


# Overall monthly trends for species of interest ------------------------------
overall_means <- all_soi %>% 
  group_by(Species) %>% 
  summarize(across(January:Landings, mean, na.rm = TRUE))

# Reorder columns & remove total landings
colnames(overall_means) <- c("species", months_abbrev)
overall_means <- overall_means[, c(1, 12, 13, 2:11)]
overall_means <- melt(overall_means, id_vars = c("species"))
colnames(overall_means) <- c("species", "month", "landings")
overall_means$species <- factor(overall_means$species, levels = species_order)

monthly_species <- ggplot(overall_means, aes(y = landings, x = month)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("mean landings (lbs)") + xlab(" ") +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~species, ncol = 4, scale = "free")

ggsave(filename="Monthly pdf data/Figures/monthly_species.pdf", monthly_species,
       width=400, height=300, units="mm", dpi=300)
