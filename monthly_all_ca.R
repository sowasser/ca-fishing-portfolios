# Script for analyzing monthly landings data from the California Department of
# Natural Resources, accessed here: https://wildlife.ca.gov/Fishing/Commercial/Landings

library(stringr)

# Read in each file with a unique name matching the year
names <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
           "11", "12", "13", "14", "15", "16", "17", "18", "19")

for(i in names) {
  name <- paste("m", i, sep = "")
  assign(name, read.csv(paste("DFW pdf data/updated/month", i, ".csv", sep = "")))
}

# Remove weird periods from the species name list
vectors <- c(m00$Species, m01$Species, m02$Species, m03$Species, m04$Species, 
             m05$Species, m06$Species, m07$Species, m08$Species, m09$Species, 
             m10$Species, m11$Species, m12$Species, m13$Species, m14$Species, 
             m15$Species, m16$Species, m17$Species, m18$Species, m19$Species)

lapply(vectors, function(x)x <- str_replace_all(x, "[[:punct:]]", ""))

for(i in vectors) {
  i <- str_replace_all(i, "[[:punct:]]", "")
}

m14$Species <- str_replace_all(m14$Species, "[[:punct:]]", "")
