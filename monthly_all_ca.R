# Script for analyzing monthly landings data from the California Department of
# Natural Resources, accessed here: https://wildlife.ca.gov/Fishing/Commercial/Landings

# Read in each file
names <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
           "11", "12", "13", "14", "15", "16", "17", "18", "19")

for(i in names) {
  name <- paste("m", i, sep = "")
  assign(name, read.csv(paste("DFW pdf data/updated/month", i, ".csv", sep = "")))
}
