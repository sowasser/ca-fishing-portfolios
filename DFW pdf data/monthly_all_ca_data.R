# Script for analyzing monthly landings data from the California Department of
# Natural Resources, accessed here: https://wildlife.ca.gov/Fishing/Commercial/Landings

library(stringr)

# Read in each file with a unique name matching the year
names <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
           "11", "12", "13", "14", "15", "16", "17", "18", "19")

for(n in names) {
  name <- paste("m", n, sep = "")
  assign(name, read.csv(paste("DFW pdf data/updated/month", n, ".csv", sep = "")))
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

# Add a column with the year of the data
# TODO: figure out how to do this iteratively/functionally
m00[, "year"] <- rep(2000, length(m00$Species))
m01[, "year"] <- rep(2001, length(m01$Species))
m02[, "year"] <- rep(2002, length(m02$Species))
m03[, "year"] <- rep(2003, length(m03$Species))
m04[, "year"] <- rep(2004, length(m04$Species))
m05[, "year"] <- rep(2005, length(m05$Species))
m06[, "year"] <- rep(2006, length(m06$Species))
m07[, "year"] <- rep(2007, length(m07$Species))
m08[, "year"] <- rep(2008, length(m08$Species))
m09[, "year"] <- rep(2009, length(m09$Species))
m10[, "year"] <- rep(2010, length(m10$Species))
m11[, "year"] <- rep(2011, length(m11$Species))
m12[, "year"] <- rep(2012, length(m12$Species))
m13[, "year"] <- rep(2013, length(m13$Species))
m14[, "year"] <- rep(2014, length(m14$Species))
m15[, "year"] <- rep(2015, length(m15$Species))
m16[, "year"] <- rep(2016, length(m16$Species))
m17[, "year"] <- rep(2017, length(m17$Species))
m18[, "year"] <- rep(2018, length(m18$Species))
m19[, "year"] <- rep(2019, length(m19$Species))

# Write new .csv files with these changes
dfs <- list(m00, m01, m02, m03, m04, m05, m06, m07, m08, m09, m10,
        m11, m12, m13, m14, m15, m16, m17, m18, m19)

for(d in dfs) {
  for(n in names) {
    write.csv(d, file=paste("Data/DFW all CA/m", n, ".csv", sep = ""), row.names = FALSE)
  }
}
