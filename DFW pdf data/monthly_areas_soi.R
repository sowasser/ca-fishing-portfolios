# Script for isolating the species of interest from the DFW dataset of monthly
# landings by area.

library(stringr)
library(dplyr)
library(reshape2)

# Import dataset of all areas
all_areas <- read.csv("Data/DFW areas/all_areas.csv")

# Find most-landed species ----------------------------------------------------
# Isolate total landings and find mean by year
total_landings <- all_areas[, c(1, 14:16)]
total_landings <- total_landings %>% group_by(area, Species) %>% 
  summarize(landings = mean(Landings))  

# Isolate each area & order by landings in descending order
total_e <- total_landings %>% filter(area == "Eureka")
total_e <- total_e[order(-total_e$landings), ]

total_fb <- total_landings %>% filter(area == "Fort Bragg")
total_fb <- total_fb[order(-total_fb$landings), ]

total_bb <- total_landings %>% filter(area == "Bodega Bay")
total_bb <- total_bb[order(-total_bb$landings), ]

total_sf <- total_landings %>% filter(area == "San Francisco")
total_sf <- total_sf[order(-total_sf$landings), ]

total_m <- total_landings %>% filter(area == "Monterey")
total_m <- total_m[order(-total_m$landings), ]

total_mb <- total_landings %>% filter(area == "Morro Bay")
total_mb <- total_mb[order(-total_mb$landings), ]

total_sb <- total_landings %>% filter(area == "Santa Barbara")
total_sb <- total_sb[order(-total_sb$landings), ]

total_la <- total_landings %>% filter(area == "Los Angeles")
total_la <- total_la[order(-total_la$landings), ]

total_sd <- total_landings %>% filter(area == "San Diego")
total_sd <- total_sd[order(-total_sd$landings), ]

# Create dataframe of highest landed species
high_landings <- rbind(total_e[1:11, 1:2], total_fb[1:11, 1:2], 
                       total_bb[1:11, 1:2], total_sf[1:11, 1:2], 
                       total_m[1:11, 1:2], total_mb[1:11, 1:2],
                       total_sb[1:11, 1:2], total_la[1:11, 1:2], 
                       total_sd[1:11, 1:2])

# Get list of unique species names (not fully unique :/ )
high_landings <- trimws(high_landings$Species)
high_landings <- levels(factor(high_landings$Species))
print(high_landings)


# Isolate species of interest -------------------------------------------------
# Column names for the initial columns of the overall dataframe
initial_cols <- c("Species", "January", "February", "March", "April", "May", "June",
                  "July", "August", "September", "October", "November", "December", 
                  "Landings", "year", "area")

crab <- all_areas %>% filter(str_detect(Species, "Dungeness")) %>% bind_rows
lobster <- all_areas %>% filter(str_detect(Species, "Lobster")) %>% bind_rows
squid <- all_areas %>% filter(str_detect(Species, "Squid market")) %>% bind_rows
albacore <- all_areas %>% filter(str_detect(Species, "albacore")) %>% bind_rows
prawn <- all_areas %>% filter(str_detect(Species, "Prawn spot")) %>% bind_rows
swordfish <- all_areas %>% filter(str_detect(Species, "Swordfish")) %>% bind_rows


# Red sea urchin --------------------------------------------------------------
urchin <- all_areas %>% filter(str_detect(Species, "Sea urchin red|Urchin red")) %>% bind_rows
# Update dataframe with new species name
urchin <- urchin[, -1] %>% group_by(area, year) %>%
  summarize(across(January:Landings, sum))  
urchin <- cbind(rep("Red Sea Urchin", length(urchin$year)), urchin[, c(3:15, 2, 1)])
colnames(urchin) <- initial_cols


# Groundfish - from list here -------------------------------------------------
# https://wildlife.ca.gov/Conservation/Marine/Federal-Groundfish 
groundfish <- all_areas %>% filter(str_detect(Species, "Halibut|Rockfish|
                                              Thornyhead|Sablefish|Skate|
                                              Shark leopard|Shark soupfin|
                                              Shark spiny dogfish|Ratfish|
                                              Cabezon|Greenling|Lingcod|Cod|
                                              Whiting|Scorpionfish|Flounder|
                                              Sole|Sanddab")) %>% bind_rows
# Update dataframe with new species name
groundfish <- groundfish[, -1] %>% group_by(area, year) %>%
  summarize(across(January:Landings, sum))  
groundfish <- cbind(rep("Groundfish", length(groundfish$year)), groundfish[, c(3:15, 2, 1)])
colnames(groundfish) <- initial_cols


# Salmon ----------------------------------------------------------------------
salmon <- all_areas %>% filter(str_detect(Species, "Salmon")) %>% bind_rows
salmon <- salmon %>% filter(!str_detect(Species, "Roe"))  # Remove salmon roe fishery
# Update dataframe with new species name
salmon <- salmon[, -1] %>%  group_by(area, year) %>% 
  summarize(across(January:Landings, sum))
salmon <- cbind(rep("Salmon", length(salmon$year)), salmon[, c(3:15, 2, 1)])
colnames(salmon) <- initial_cols


# Coastal pelagic species gathered from NOAA fisheries ------------------------
pelagics <- all_areas %>% filter(str_detect(Species, "Sardine|Mackerel Pacific|
                                            Mackerel jack|Anchovy northern")) %>% bind_rows
# Update dataframe with new species name
pelagics <- pelagics[, -1] %>% group_by(area, year) %>% 
  summarize(across(January:Landings, sum))
pelagics <- cbind(rep("Pelagics", length(pelagics$year)), pelagics[, c(3:15, 2, 1)])
colnames(pelagics) <- initial_cols


# Create dataframe of all species of interest ---------------------------------
all_soi <- rbind(crab, lobster, squid, albacore, prawn, swordfish, urchin, 
                 groundfish, salmon, pelagics)

all_soi$Species <- trimws(all_soi$Species)  # trim white space from names

# Replace DFW species names with more understandable text
all_soi$Species <- str_replace_all(all_soi$Species, 
                                   c("Crab Dungeness" = "Dungeness Crab",
                                     "Lobster California spiny" = "Spiny Lobster",
                                     "Prawn spot" = "Spot Prawn",
                                     "Squid market" = "Market Squid",
                                     "Tuna albacore" = "Albacore Tuna"))
species <- levels(factor(all_soi$Species))  # list of species
print(species)  # check species list

# Write a .csv file with just the species of interest
write.csv(all_soi, "Data/dfw_areas_soi.csv", row.names = FALSE)
