# Script for examining response in monthly landings around the delay in the 
# Dungeness crab season caused by domoic acid levels in 2015-2016 using the
# CDFW dataset downloaded from here: 
# https://wildlife.ca.gov/Fishing/Commercial/Landings

library(stringr)
library(dplyr)
library(reshape2)
library(PMCMRplus)
library(ggplot2)
library(viridis)

# Read in cleaned data with only the fisheries of interest
all_soi <- read.csv("Data/dfw_areas_all_soi.csv")

# Find sum across all areas - represents all of California
all_ca <- all_soi %>%
  group_by(Species, year) %>%
  summarize(across(January:Landings, sum, na.rm = TRUE))

# Reorder by year, change order of columns, remove total landings
all_ca <- all_ca[order(all_ca$year), c(1, 2, 3:14)]

# Split into Nov-Dec starting in 2000 and Jan-Oct starting in 2001
# Update years to match combined year range (year 1 Nov-Dec, year 2 Jan-Oct)
nov.dec <- all_ca[, c(1, 2, 13, 14)]
nov.dec <- nov.dec %>% filter(!year == 2019)
for (i in 2000:2018) {
  nov.dec$year[nov.dec$year == i] <- paste(i, "-", i + 1, sep = "")
}

jan.oct <- all_ca[, c(1:12)]
jan.oct <- jan.oct %>% filter(!year == 2000)
for (i in 2001:2019) {
  jan.oct$year[jan.oct$year == i] <- paste(i - 1, "-", i, sep = "")
}

# Check to see where nov.dec & jan.oct species differ.....
