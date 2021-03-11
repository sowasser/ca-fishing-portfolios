# Script for reading in, manipulating, and writing a new .csv file for the pdf 
# data for nine areas within California, downloaded from the California 
# Department of Natural Resources, accessed here: 
# https://wildlife.ca.gov/Fishing/Commercial/Landings

library(stringr)
library(tidyverse)

# Function for reading in and manipulating the data ---------------------------
# First need to define the years of the data to be added later
# TODO: add 2015-2017 when pdf download issue is fixed
years <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 
           2011, 2012, 2013, 2014, 2018, 2019)

read_and_clean <- function(area) {
  # Read in and clean all of the files for an area. Process:
  # 1. Read in data from the sub-directory for that area
  # 2. Remove all unneccessary punctuation from the "Species" column
  # 3. Add a column with the year of each dataframe, 
  # 4. Combine all years together & add a column with the area name
  files <- list.files(path = paste("DFW pdf data/updated/areas/", area, "/", sep = ""), 
                     pattern = "*.csv")
  df <- lapply(files, function(x) 
    read.csv(paste("DFW pdf data/updated/areas/", area, "/", x, sep = "")))
  
  data_clean <- function(file) {
    file$Species <- str_replace_all(file$Species, "[[:punct:]]", "")
    return(file)
  }
  df_clean <- lapply(df, data_clean)
  
  df_years <- map2(df_clean, years, ~cbind(.x, year = .y))
  df_final <- bind_rows(df_years)
  df_final[, "area"] <- rep(area, length(df_final$Species))
  return(df_final)
}

# Apply function to each of the areas & write new .csv file -------------------
bodega_bay <- read_and_clean("Bodega Bay")
write.csv(bodega_bay, "Data/DFW areas/bodega_bay.csv", row.names = FALSE)

