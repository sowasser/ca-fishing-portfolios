# Ananlysis of the monthly landings data for all of California, downloaded from
# the CA Department of Natural Resources 
# (https://wildlife.ca.gov/Fishing/Commercial/Landings).
# The data has been converted to .csv format and manipulated for analysis.

library(stringr)
library(dplyr)
library(reshape2)

# Read in each file with a unique name matching the year ----------------------
names <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
           "11", "12", "13", "14", "15", "16", "17", "18", "19")

for(n in names) {
  name <- paste("m", n, sep = "")
  assign(name, 
         read.csv(paste("Data/DFW all CA/m", n, ".csv", sep = ""), 
                  # Declare data types to make sure everything matches
                  stringsAsFactors = FALSE,
                  colClasses = c("character", rep("numeric", 14))))
}

# Subset dataframes by species of interest ------------------------------------
dfs <- list(m00, m01, m02, m03, m04, m05, m06, m07, m08, m09, m10, m11, m12,
            m13, m14, m15, m16, m17, m18, m19)

halibut <- lapply(dfs, function(x) x %>% filter(str_detect(Species, "Halibut California"))) %>% bind_rows
crab <- lapply(dfs, function(x) x %>% filter(str_detect(Species, "Dungeness"))) %>% bind_rows
lobster <- lapply(dfs, function(x) x %>% filter(str_detect(Species, "Lobster"))) %>% bind_rows
squid <- lapply(dfs, function(x) x %>% filter(str_detect(Species, "Squid market"))) %>% bind_rows
albacore <- lapply(dfs, function(x) x %>% filter(str_detect(Species, "albacore"))) %>% bind_rows
prawn <- lapply(dfs, function(x) x %>% filter(str_detect(Species, "Prawn spot"))) %>% bind_rows
urchin <- lapply(dfs, function(x) x %>% filter(str_detect(Species, "Sea urchin red"))) %>% bind_rows
sablefish <- lapply(dfs, function(x) x %>% filter(str_detect(Species, "Sablefish"))) %>% bind_rows
rockfish <- lapply(dfs, function(x) x %>% filter(str_detect(Species, "Rockfish"))) %>% bind_rows
swordfish <- lapply(dfs, function(x) x %>% filter(str_detect(Species, "Swordfish"))) %>% bind_rows
thornyhead <- lapply(dfs, function(x) x %>% filter(str_detect(Species, "Thornyhead"))) %>% bind_rows
# TODO: determine if rockfish & thornyhead should be further sub-sampled or combined

# Coastal pelagic species gathered from NOAA fisheries
sardine <- lapply(dfs, function(x) x %>% filter(str_detect(Species, "Sardine"))) %>% bind_rows
pac_mackerel <- lapply(dfs, function(x) x %>% filter(str_detect(Species, "Mackerel Pacific"))) %>% bind_rows
jack_mackerel <- lapply(dfs, function(x) x %>% filter(str_detect(Species, "Mackerel jack"))) %>% bind_rows
anchovy <- lapply(dfs, function(x) x %>% filter(str_detect(Species, "Anchovy northern"))) %>% bind_rows
pelagics <- rbind(sardine, pac_mackerel, jack_mackerel, anchovy)

# Create dataframe of all species of interest
soi <- rbind(halibut, crab, lobster, squid, albacore, prawn, urchin, sablefish,
             rockfish, swordfish, thornyhead, pelagics)

# Monthly means for years in the 'stable period' - 2009-2014 ------------------
# Filter for 2009-2014
soi_stable <- soi %>% filter(between(year, 2009, 2014))
soi_stable <- soi_stable[, -14]  # remove total landings column

# Reshape data & find mean
soi_stable_means <- melt(soi_stable, id.vars = c("Species", "year"))
colnames(soi_stable_means) <- c("species", "year", "month", "landings")
  
soi_stable_means <- soi_stable_means %>%
  group_by(species, month) %>% 
  summarize(landings = mean(landings))
