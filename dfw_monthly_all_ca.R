# Ananlysis of the monthly landings data for all of California, downloaded from
# the CA Department of Natural Resources 
# (https://wildlife.ca.gov/Fishing/Commercial/Landings).
# The data has been converted to .csv format and manipulated for analysis.

# Read in each file with a unique name matching the year
names <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
           "11", "12", "13", "14", "15", "16", "17", "18", "19")

for(n in names) {
  name <- paste("m", n, sep = "")
  assign(name, read.csv(paste("Data/DFW all CA/m", n, ".csv", sep = "")))
}

