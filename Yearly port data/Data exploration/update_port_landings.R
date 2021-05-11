# Script for updating the port landings dataset - the California landings data,
# organized by port from 1992-2014, downloaded from the California Natural 
# Resources Agency:
# https://data.cnra.ca.gov/dataset/human-uses-and-socioeconomic-dimensions-ca-north-coast-mpa-baseline-study-1992-2014, by:

# Updates are:
# 1) removing the "extra" fishery categories (all other or misc fisheries)
# 2) Adjusting revenue values for inflation

library(dplyr)
library(priceR)


# Remove "extra" fisheries ----------------------------------------------------
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


# Adjust for inflation --------------------------------------------------------
# Conduct adjustments by year, bringing everything to 2014
revenue_adj <- adjust_for_inflation(price = pl$ex.vessel_revenue, 
                                    from_date = pl$year, 
                                    country = "US", 
                                    to_date = 2014)

# Add adjusted revenue column to the original dataframe
pl_adj <- cbind(pl, revenue_adj)


# Write new .csv file without rownames creating their own column --------------
write.csv(pl_adj, file = "Data/port_landings_updated.csv", row.names = FALSE)



