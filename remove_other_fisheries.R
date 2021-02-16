# Script for removing the "extra" fishery categories (all other or misc 
# fisheries) from the the California landings data, organized by port from 
# 1992-2014, downloaded from the California Natural Resources Agency:
# https://data.cnra.ca.gov/dataset/human-uses-and-socioeconomic-dimensions-ca-north-coast-mpa-baseline-study-1992-2014

library(dplyr)

# Read in initial data-set
pl <- read.csv("Data/port_landings_92-14.csv")

# Remove fisheries that won't be used in the analysis
pl <- pl %>% filter(fishery != "MISC")
pl <- pl %>% filter(fishery != "OTHER")
pl <- pl %>% filter(fishery != "OTHER CRAB")
pl <- pl %>% filter(fishery != "OTHER FLATFISH")
pl <- pl %>% filter(fishery != "OTHER GROUNDFISH")
pl <- pl %>% filter(fishery != "OTHER OFFSHORE PELAGICS")
pl <- pl %>% filter(fishery != "OTHER-misc")

# Check to make sure all "other" or "misc" fisheries have been removed
species <- levels(factor(pl$fishery))  # list of species
print(species)

# Write new .csv file without rownames creating their own column
write.csv(pl, file = "Data/port_landings_updated.csv", row.names = FALSE)
