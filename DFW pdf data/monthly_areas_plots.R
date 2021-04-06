# Analysis of the data from the nine areas within California, gathered from the
# California Department of Natural Resources, accessed here: 
# https://wildlife.ca.gov/Fishing/Commercial/Landings

library(stringr)
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

months_abbrev <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                   "Sep", "Oct", "Nov", "Dec")


# All landings for the stable period ------------------------------------------
all_landings <- all_areas %>% group_by(area, year) %>% 
  summarize(across(January:Landings, sum))

all_stable <- all_landings %>% filter(between(year, 2009, 2014))
all_stable <- all_stable[, -2]  # remove year column

all_means <- all_stable %>%
  group_by(area) %>%
  summarize(across(January:Landings, mean))

colnames(all_means) <- c("species", months_abbrev)
all_means <- all_means[, c(1, 12, 13, 2:11)]  # Year starts in Nov.
all_means <- melt(all_means, id_vars = c("area"))
colnames(all_means) <- c("area", "month", "landings")
all_means$area <- factor(all_means$area, levels = area_order)

all_stable <- ggplot(all_means, aes(y = landings, x = month)) +
  geom_bar(stat = "identity") +
  ylab("mean landings (lbs)") + xlab("mean across 2009-2014") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~area, ncol = 3, scale = "free")

ggsave(filename="DFW pdf data/Figures/all_landings_stable.pdf", all_stable,
       width=400, height=250, units="mm", dpi=300)


# Monthly averages over the stable period for the top species of interest -----
# Filter for 2009-2014
top_soi_stable <- top_soi %>% filter(between(year, 2009, 2014))

# Find monthly mean for each year
top_soi_stable <- top_soi_stable[, -15]  # remove year column
top_soi_stable$Species <- str_trim(top_soi_stable$Species, side = "both")  # Remove extra white spaces

top_soi_means <- top_soi_stable %>% 
  group_by(Species, area) %>% 
  summarize(across(January:Landings, mean))

top_soi_means <- top_soi_means[, -15]  # Remove total landings

colnames(top_soi_means) <- c("species", "area", months_abbrev)  # Update to abbreviated months

# Change column & row order to match analyses
top_soi_means <- top_soi_means[, c(1, 2, 13, 14, 3:12)]  # Year starts in Nov.
top_soi_means <- melt(top_soi_means, id_vars = c("area", "species"))
colnames(top_soi_means) <- c("species", "area", "month", "landings")
top_soi_means$area <- factor(top_soi_means$area, levels = area_order)

# Plot monthly means for stable period
monthly_areas_stable <- ggplot(top_soi_means, aes(y = landings, x = month, fill = species)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("mean landings (lbs)") + xlab("mean across 2009-2014") +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~area, ncol = 3, scale = "free")

ggsave(filename="DFW pdf data/Figures/area_monthly_landings_stable.pdf", monthly_areas_stable,
       width=400, height=250, units="mm", dpi=300)


# Overall monthly trends for species of interest across the stable period -----
all_soi_stable <- all_soi %>% filter(between(year, 2009, 2014))
overall_means <- all_soi_stable %>% 
  group_by(Species) %>% 
  summarize(across(January:Landings, mean))

# Reorder columns & remove total landings
colnames(overall_means) <- c("species", months_abbrev)
overall_means <- overall_means[, c(1, 12, 13, 2:11)]
overall_means <- melt(overall_means, id_vars = c("species"))
colnames(overall_means) <- c("species", "month", "landings")

monthly_stable <- ggplot(overall_means, aes(y = landings, x = month)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("mean landings (lbs)") + xlab("mean across 2009-2014") +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~species, ncol = 4, scale = "free")

ggsave(filename="DFW pdf data/Figures/monthly_species_stable.pdf", monthly_stable,
       width=400, height=300, units="mm", dpi=300)


# Yearly trends for species of interest ---------------------------------------
soi_plots <- all_soi[, -14]  # create new df for plots & remove total landings
soi_plots$area <- factor(soi_plots$area, levels = area_order)
colnames(soi_plots) <- c("species", months_abbrev, "year", "area")
soi_plots <- soi_plots[, c(1, 15, 14, 12, 13, 2:11)]
soi_plots$year <- as.factor(soi_plots$year)
soi_plots <- melt(soi_plots, id_vars = c("species", "area", "year"))
colnames(soi_plots) <- c("species", "area", "year", "month", "landings")

soi_fisheries <- levels(factor(soi_plots$species))

species_timeseries <- function(fishery) {
  df <- soi_plots %>% filter(species == fishery)
  
  plt <- ggplot(df, aes(x = month, y = landings, fill = area)) +
    geom_bar(position = "stack", stat = "identity") +
    ylab("mean landings (lbs)") +
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
