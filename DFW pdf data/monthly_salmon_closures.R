# Script for examining response in monthly landings around the salmon closures
# in 2008 & 2009, using the CDFW dataset downloaded from here: 
# https://wildlife.ca.gov/Fishing/Commercial/Landings

library(stringr)
library(dplyr)
library(reshape2)

# Read in cleaned data with only the fisheries of interest
all_soi <- read.csv("Data/dfw_areas_all_soi.csv")

# Remove more southern areas where salmon aren't fished
salmon_areas <- all_soi %>% 
  filter(!area %in% c("Santa Barbara", "Los Angeles", "San Diego"))

# Separate closed & open years
closed_years <- c(2008, 2009)
open_years <- c(2000:2007, 2010:2019)

closed <- salmon_areas %>% filter(year %in% closed_years)
open <- salmon_areas %>% filter(!year %in% closed_years)
