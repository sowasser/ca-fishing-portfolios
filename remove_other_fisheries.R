# Script for removing the "extra" fishery categories (all other or misc 
# fisheries) from the the California landings data, organized by port from 
# 1992-2014, downloaded from the California Natural Resources Agency:
# https://data.cnra.ca.gov/dataset/human-uses-and-socioeconomic-dimensions-ca-north-coast-mpa-baseline-study-1992-2014

library(dplyr)

# Read in initial data-set
original <- read.csv("Data/port_landings_92-14.csv")

# List of fisheries to not include
exclude <- c("MISC", "OTHER", "OTHER CRAB", "OTHER FLATFISH", 
             "OTHER GROUNDFISH", "OTHER OFFSHORE PELAGICS", "OTHER-misc")

# Remove fisheries in the list - run function through a for-loop w/ all species
exclude_species <- function(fish) {
  df <- original %>% filter(fishery != fish)
}

for (e in exclude) {
  port_landings <- exclude_species(e)
}

# Check the list of remaining species
species <- levels(factor(port_landings$fishery))  # list of species
print(species)
