# Analysis of the data from the nine areas within California, gathered from the
# California Department of Natural Resources, accessed here: 
# https://wildlife.ca.gov/Fishing/Commercial/Landings

# In this script, graphs of monthly landings for each of the species, faceted 
# by year 

library(dplyr)
library(reshape2)
library(ggplot2)
library(viridis)


# Import datasets for all areas and for species of interest
all_areas <- read.csv("Data/DFW areas/all_areas.csv")
all_soi <- read.csv("Data/dfw_areas_all_soi.csv")
top_soi <- read.csv("Data/dfw_areas_top_soi.csv")

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
years <- c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", 
           "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015",
           "2016", "2017", "2018", "2019")
years <- as.character(2000:2019)

new_year <- function(this_year) {
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

for (y in years) {
  soi_plots <- new_year(y)
}


# Remove rows that include years without any data
df <- df %>% filter(!year == "1999-2000" & !year == "2019-2020")

# Rename temporary dataframe to match previous dataframe
soi_plots <- df

# Create plots in a loop ------------------------------------------------------
soi_fisheries <- levels(factor(soi_plots$species))

species_timeseries <- function(fishery) {
  df <- soi_plots %>% filter(species == fishery)
  
  plt <- ggplot(df, aes(x = month, y = landings, fill = area)) +
    geom_bar(position = "stack", stat = "identity") +
    ylab("mean landings (lbs)") + xlab(" ") +
    ggtitle(fishery) + 
    scale_fill_viridis(discrete = TRUE) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    facet_wrap(~year, ncol = 4, scale = "free")
  
  ggsave(filename=paste("DFW pdf data/Figures/Species timeseries/", fishery, "_revenue.pdf", 
                        sep=""), 
         plot=plt, width=400, height=250, units="mm", dpi=300)
}

# Function call for each species 
for (s in soi_fisheries) {
  species_timeseries(s)
}
