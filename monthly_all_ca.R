# Script for analyzing monthly landings data from the California Department of
# Natural Resources, accessed here: https://wildlife.ca.gov/Fishing/Commercial/Landings

library(stringr)

# Read in and manipulate data -------------------------------------------------
# Read in each file with a unique name matching the year
names <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
           "11", "12", "13", "14", "15", "16", "17", "18", "19")

for(i in names) {
  name <- paste("m", i, sep = "")
  assign(name, read.csv(paste("DFW pdf data/updated/month", i, ".csv", sep = "")))
}

# Remove weird periods from the species name list
# TODO: figure out how to do this iteratively/functionally
m00$Species <- str_replace_all(m00$Species, "[[:punct:]]", "")
m01$Species <- str_replace_all(m01$Species, "[[:punct:]]", "")
m02$Species <- str_replace_all(m02$Species, "[[:punct:]]", "")
m03$Species <- str_replace_all(m03$Species, "[[:punct:]]", "")
m04$Species <- str_replace_all(m04$Species, "[[:punct:]]", "")
m05$Species <- str_replace_all(m05$Species, "[[:punct:]]", "")
m06$Species <- str_replace_all(m06$Species, "[[:punct:]]", "")
m07$Species <- str_replace_all(m07$Species, "[[:punct:]]", "")
m08$Species <- str_replace_all(m08$Species, "[[:punct:]]", "")
m09$Species <- str_replace_all(m09$Species, "[[:punct:]]", "")
m10$Species <- str_replace_all(m10$Species, "[[:punct:]]", "")
m11$Species <- str_replace_all(m11$Species, "[[:punct:]]", "")
m12$Species <- str_replace_all(m12$Species, "[[:punct:]]", "")
m13$Species <- str_replace_all(m13$Species, "[[:punct:]]", "")
m14$Species <- str_replace_all(m14$Species, "[[:punct:]]", "")
m15$Species <- str_replace_all(m15$Species, "[[:punct:]]", "")
m16$Species <- str_replace_all(m16$Species, "[[:punct:]]", "")
m17$Species <- str_replace_all(m17$Species, "[[:punct:]]", "")
m18$Species <- str_replace_all(m18$Species, "[[:punct:]]", "")
m19$Species <- str_replace_all(m19$Species, "[[:punct:]]", "")
