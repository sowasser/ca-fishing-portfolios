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

# TODO: add 2015-2017 when pdf download issue is fixed
data_years <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 
                2010, 2011, 2012, 2013, 2014, 2018, 2019)

# Function to fix weird formatting issues
data_clean <- function(files, years) {
  area <- list()
  for(x in files) {
    for(year in years) {
      x$Species <- str_replace_all(x$Species, "[[:punct:]]", "")
      x[, "year"] <- rep(year, length(x$Species))
      append(area, x)
    }
  }
  return(area)
}

bodega_bay <- data_clean(bb, data_years)