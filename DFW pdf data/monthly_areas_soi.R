# Script for isolating the species of interest from the DFW dataset of monthly
# landings by area.

library(stringr)
library(dplyr)
library(reshape2)

# Import dataset of all areas
all_areas <- read.csv("Data/DFW areas/all_areas.csv")

# Column names for the initial columns of the overall dataframe
initial_cols <- c("Species", "January", "February", "March", "April", "May", "June",
                  "July", "August", "September", "October", "November", "December", 
                  "Landings", "year", "area")


# Islolate species of interest ------------------------------------------------
crab <- all_areas %>% filter(str_detect(Species, "Dungeness")) %>% bind_rows
lobster <- all_areas %>% filter(str_detect(Species, "Lobster")) %>% bind_rows
squid <- all_areas %>% filter(str_detect(Species, "Squid market")) %>% bind_rows
albacore <- all_areas %>% filter(str_detect(Species, "albacore")) %>% bind_rows
prawn <- all_areas %>% filter(str_detect(Species, "Prawn spot")) %>% bind_rows
urchin <- all_areas %>% filter(str_detect(Species, "Sea urchin red")) %>% bind_rows
swordfish <- all_areas %>% filter(str_detect(Species, "Swordfish")) %>% bind_rows


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

write.csv(groundfish, "Data/dfw_groundfish.csv", row.names = FALSE)


# Salmon ----------------------------------------------------------------------
salmon <- all_areas %>% filter(str_detect(Species, "Salmon")) %>% bind_rows
salmon <- salmon %>% filter(!str_detect(Species, "Roe"))  # Remove salmon roe fishery
# Update dataframe with new species name
salmon <- salmon[, -1] %>%  group_by(area, year) %>% 
  summarize(across(January:Landings, sum))
salmon <- cbind(rep("Salmon", length(salmon$year)), salmon[, c(3:15, 2, 1)])
colnames(salmon) <- initial_cols

write.csv(salmon, "Data/dfw_salmon.csv", row.names = FALSE)


# Coastal pelagic species gathered from NOAA fisheries ------------------------
pelagics <- all_areas %>% filter(str_detect(Species, "Sardine|Mackerel Pacific|
                                            Mackerel jack|Anchovy northern")) %>% bind_rows
# Update dataframe with new species name
pelagics <- pelagics[, -1] %>% group_by(area, year) %>% 
  summarize(across(January:Landings, sum))
pelagics <- cbind(rep("Pelagics", length(pelagics$year)), pelagics[, c(3:15, 2, 1)])
colnames(pelagics) <- initial_cols

write.csv(pelagics, "Data/dfw_pelagics.csv", row.names = FALSE)


# Create dataframe of all species of interest ---------------------------------
all_soi <- rbind(crab, lobster, squid, albacore, prawn, urchin, swordfish, 
                 groundfish, salmon, pelagics)

all_soi$Species <- trimws(all_soi$Species)  # trim white space from names

# Replace DFW species names with more understandable text
all_soi$Species <- str_replace_all(all_soi$Species, 
                                   c("Crab Dungeness" = "Dungeness Crab",
                                     "Lobster California spiny" = "Spiny Lobster",
                                     "Prawn spot" = "Spot Prawn",
                                     "Sea urchin red" = "Red Sea Urchin",
                                     "Squid market" = "Market Squid",
                                     "Tuna albacore" = "Albacore Tuna"))
species <- levels(factor(all_soi$Species))  # list of species
print(species)  # check species list

# Write a .csv file with just the species of interest
write.csv(all_soi, "Data/dfw_areas_soi.csv", row.names = FALSE)
