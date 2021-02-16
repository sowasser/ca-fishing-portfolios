# Script for looking at the changes in fishery exploitation around the 2008
# king salmon closure in California, using landings data aggregated by port,
# downloaded from: 
# https://data.cnra.ca.gov/dataset/human-uses-and-socioeconomic-dimensions-ca-north-coast-mpa-baseline-study-1992-2014
# Specifically, "Landing Receipt Records: Landings Data By Port 1992-2014"

library(dplyr)
library(tidyr)

# Read in all landings data
port_landings <- read.csv("Data/port_landings_updated.csv")

# Isolate stable period: 2009-2014
stable_years <- port_landings %>% filter(between(year, 2009, 2014))

# Select salmon data
salmon_all <- port_landings %>% filter(fishery == "SALMON")
salmon_ports <- levels(factor(salmon_all$port))  # ports where salmon was fished

# Select all data for ports where salmon was fished at some point
sal_port_data <- filter(port_landings, port %in% salmon_ports)
sal_port_data <- sal_port_data %>% drop_na()

# Stable salmon port years 
stable_sal_ports <- filter(stable_years, port %in% salmon_ports)

# Find count of number of fisheries for salmon-catching ports by year
sal_port_count <- count(sal_port_data, year, port)
