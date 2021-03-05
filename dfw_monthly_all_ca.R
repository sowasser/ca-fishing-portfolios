# Ananlysis of the monthly landings data for all of California, downloaded from
# the CA Department of Natural Resources 
# (https://wildlife.ca.gov/Fishing/Commercial/Landings).
# The data has been converted to .csv format and manipulated for analysis.

library(stringr)
library(dplyr)

# Read in each file with a unique name matching the year
names <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
           "11", "12", "13", "14", "15", "16", "17", "18", "19")

for(n in names) {
  name <- paste("m", n, sep = "")
  assign(name, 
         read.csv(paste("Data/DFW all CA/m", n, ".csv", sep = ""), 
                  stringsAsFactors = FALSE,
                  colClasses = c("character", rep("numeric", 14))))
}

# Subset dataframes by species <of interest
dfs <- list(m00, m01, m02, m03, m04, m05, m06, m07, m08, m09, m10, m11, m12,
            m13, m14, m15, m16, m17, m18, m19)

crab <- lapply(dfs, function(x) x %>% filter(str_detect(Species, "Dungeness"))) %>% bind_rows
