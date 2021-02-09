# Exploring the California landings & fishery participation data for 1992-2014,
# downloaded from the California Natural Resources Agency:
# https://data.cnra.ca.gov/dataset/human-uses-and-socioeconomic-dimensions-ca-north-coast-mpa-baseline-study-1992-2014
# Specifically, this script is for the landings data by port, listed as:
# "Landing Receipt Records: Landings Data By Port 1992-2014"

library(dplyr)
library(tidyr)
library(reshape2)

port_landings <- read.csv("Data/port_landings_92-14.csv")

species <- levels(factor(port_landings$fishery))  # list of species

# Workflow for Dungeness Crab -------------------------------------------------
# Select only the dungeness crab data
crab_all <- port_landings %>% filter(fishery == "DUNGENESS CRAB")
crab <- crab_all %>% drop_na()

# NB: when finding the means, this aggregates different gear types.
# Find means by year
crab_yr_mean <- group_by(crab, year) %>% summarize(m= mean(ex.vessel_revenue))

# Find means by year & port
crab_yr_port_mean <- group_by(crab, year, port) %>% summarize(m= mean(ex.vessel_revenue))


# Workflow for salmon closure -------------------------------------------------
# Select salmon data
salmon_all <- port_landings %>% filter(fishery == "SALMON")
salmon_ports <- levels(factor(salmon_all$port))  # ports where salmon was fished

# Select all data for ports where salmon was fished at some point
sal_port_data <- filter(port_landings, port %in% salmon_ports)
sal_port_data <- sal_port_data %>% drop_na()

# Find count of number of fisheries for salmon-catching ports by year
sal_port_count <- group_by(crab, year, port) %>% summarize(m= mean(ex.vessel_revenue))
