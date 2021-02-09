# Exploring the California landings & fishery participation data for 1992-2014,
# downloaded from the California Natural Resources Agency:
# https://data.cnra.ca.gov/dataset/human-uses-and-socioeconomic-dimensions-ca-north-coast-mpa-baseline-study-1992-2014
# Specifically, this script is for the landings data by port, listed as:
# "Landing Receipt Records: Landings Data By Port 1992-2014"

library(reshape2)
library(ggplot2)
library(viridis)
library(patchwork)

port_landings_all <- read.csv("Data/port_landings_92-14.csv")
# Remove non-applicable columns
port_landings <- port_landings_all[, -c(1, 5, 6, 10)] 

species <- levels(factor(port_landings$fishery))  # list of species
