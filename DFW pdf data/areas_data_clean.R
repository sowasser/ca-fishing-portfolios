# Script for reading in, manipulating, and writing a new .csv file for the pdf 
# data for nine areas within California, downloaded from the California 
# Department of Natural Resources, accessed here: 
# https://wildlife.ca.gov/Fishing/Commercial/Landings

library(stringr)
library(tidyverse)

# For Bodega Bay --------------------------------------------------------------
# Read in each file as a dataframe within a list
files <- list.files(path = "DFW pdf data/updated/areas/Bodega Bay/", pattern = "*.csv")

bb <- lapply(files, function(x) 
  read.csv(paste("DFW pdf data/updated/areas/Bodega Bay/", x, sep = "")))

# Function to remove weird punctuation from species column
data_clean <- function(file) {
  file$Species <- str_replace_all(file$Species, "[[:punct:]]", "")
  return(file)
}

bb_clean <- lapply(bb, data_clean)

# Add a column with the year of the data
# TODO: add 2015-2017 when pdf download issue is fixed
years <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 
           2011, 2012, 2013, 2014, 2018, 2019)

bb_years <- map2(bb_clean, years, ~cbind(.x, year = .y))

# Combine all dfs together into one, add column with area name, write new .csv
bodega_bay <- bind_rows(bb_years)
bodega_bay[, "area"] <- rep("Bodega Bay", length(bodega_bay$Species))

write.csv(bodega_bay, "Data/DFW areas/bodega_bay.csv", row.names = FALSE)
