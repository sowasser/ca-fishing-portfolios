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
           2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)

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

# For debugging messed up .csv files ------------------------------------------
debug_csv <- function(area) {
  # Just read in the files to check for number of columns, etc.
  files <- list.files(path = paste("DFW pdf data/updated/areas/", area, "/", sep = ""), 
                      pattern = "*.csv")
  df <- lapply(files, function(x) 
    read.csv(paste("DFW pdf data/updated/areas/", area, "/", x, sep = "")))
  return(df)
}

df <- debug_csv("Bodega Bay")

# Apply function to each of the areas & write new .csv file -------------------
bb <- read_and_clean("Bodega Bay")
write.csv(bb, "Data/DFW areas/bodega_bay.csv", row.names = FALSE)

e <- read_and_clean("Eureka")
write.csv(e, "Data/DFW areas/eureka.csv", row.names = FALSE)

fb <- read_and_clean("Fort Bragg")
write.csv(fb, "Data/DFW areas/fort_bragg.csv", row.names = FALSE)

la <- read_and_clean("Los Angeles")
write.csv(la, "Data/DFW areas/los_angeles.csv", row.names = FALSE)

m <- read_and_clean("Monterey")
write.csv(m, "Data/DFW areas/monterey.csv", row.names = FALSE)

mb <- read_and_clean("Morro Bay")
write.csv(mb, "Data/DFW areas/morro_bay.csv", row.names = FALSE)

sd <- read_and_clean("San Diego")
write.csv(sd, "Data/DFW areas/san_diego.csv", row.names = FALSE)

sf <- read_and_clean("San Francisco")
write.csv(sf, "Data/DFW areas/san_francisco.csv", row.names = FALSE)

sb <- read_and_clean("Santa Barbara")
write.csv(sb, "Data/DFW areas/santa_barbara.csv", row.names = FALSE)

all <- rbind(bb, e, fb, la, m, mb, sd, sf, sb)
write.csv(all, "Data/DFW areas/all_areas.csv", row.names = FALSE)
