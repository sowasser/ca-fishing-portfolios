# Analysis of the data from the nine areas within California, gathered from the
# California Department of Natural Resources, accessed here: 
# https://wildlife.ca.gov/Fishing/Commercial/Landings

# In this script, graphs of monthly landings for each of the species, faceted 
# by year, are created with the appropriate year labels demonstrating the 
# correct month/year combination.

library(dplyr)
library(reshape2)
library(ggplot2)
library(viridis)


# Import dataset for all species of interest
all_soi <- read.csv("Data/dfw_areas_all_soi.csv")

# Order of areas from North -> South
area_order <- c("Eureka", "Fort Bragg", "Bodega Bay", "San Francisco", 
                "Monterey", "Morro Bay", "Santa Barbara", "Los Angeles",
                "San Diego")

# Abbreviation of months for nice plots
months_abbrev <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                   "Sep", "Oct", "Nov", "Dec")


# Transform data into a useful form
soi_plots <- all_soi[, -14]  # create new df for plots & remove total landings
soi_plots$area <- factor(soi_plots$area, levels = area_order)
colnames(soi_plots) <- c("species", months_abbrev, "year", "area")
soi_plots <- soi_plots[, c(1, 15, 14, 12, 13, 2:11)]
soi_plots$year <- as.character(soi_plots$year)
soi_plots <- melt(soi_plots, id_vars = c("species", "area", "year"))
colnames(soi_plots) <- c("species", "area", "year", "month", "landings")


# Replace simple years with a year range --------------------------------------
new_year <- function(this_year) {
  # Function creates a new year label depending on the month so that the years
  # actually match up with the month order used for assessing the fisheries,
  # which composes November & December of one year, then January-October of the
  # next. 
  df <- soi_plots
  df <- within(df, year[year == this_year & month == "Nov" | 
                          year == this_year & month == "Dec"] 
               <- paste(this_year, "-", as.numeric(this_year) + 1, sep = ""))
  df <- within(df, year[year == this_year & month == "Jan" | 
                          year == this_year & month == "Feb" |
                          year == this_year & month == "Mar" | 
                          year == this_year & month == "Apr" | 
                          year == this_year & month == "May" | 
                          year == this_year & month == "Jun" | 
                          year == this_year & month == "Jul" | 
                          year == this_year & month == "Aug" |
                          year == this_year & month == "Sep" | 
                          year == this_year & month == "Oct"] 
               <- paste(as.numeric(this_year) - 1, "-", this_year, sep = ""))
  return(df)
}

# Call function for all years
years <- as.character(2000:2019)

for (y in years) {
  soi_plots <- new_year(y)
}

# Remove rows that include years without any data
soi_plots <- soi_plots %>% filter(!year == "2019-2020")

write.csv(soi_plots, "Data/DFW areas/fisheries_year_soi.csv", row.names = FALSE)


# Create plots in a function --------------------------------------------------
soi_fisheries <- levels(factor(soi_plots$species))

species_timeseries <- function(fishery) {
  # Function creates an plot for each of the species of interest with a unique
  # title and saves them all to one folder with unique name.
  df <- soi_plots %>% filter(species == fishery)
  
  plt <- ggplot(df, aes(x = month, y = landings, fill = area)) +
    geom_bar(position = "stack", stat = "identity") +
    ylab("mean landings (lbs)") + xlab(" ") +
    ggtitle(fishery) + 
    scale_fill_viridis(discrete = TRUE) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    facet_wrap(~year, ncol = 4, scale = "free")
  
  ggsave(filename=paste("Monthly pdf data/Figures/Species timeseries/", fishery, "_landings.pdf", 
                        sep=""), 
         plot=plt, width=400, height=250, units="mm", dpi=300)
}

# Function call for each species 
for (s in soi_fisheries) {
  species_timeseries(s)
}
