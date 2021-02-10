# Exploring the California landings & fishery participation data for 1992-2014,
# downloaded from the California Natural Resources Agency:
# https://data.cnra.ca.gov/dataset/human-uses-and-socioeconomic-dimensions-ca-north-coast-mpa-baseline-study-1992-2014
# Specifically, this script is for the landings data by port, listed as:
# "Landing Receipt Records: Landings Data By Port 1992-2014"

library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(viridis)

port_landings <- read.csv("Data/port_landings_92-14.csv")

species <- levels(factor(port_landings$fishery))  # list of species

# Isolate stable period: 2009-2014
stable_years <- port_landings %>% filter(between(year, 2009, 2014))


# Timeseries of fisheries by port ---------------------------------------------
pl <- port_landings %>% drop_na()  # remove NAs
pl <- pl[order(pl$year, pl$port, -pl$ex.vessel_revenue), ]  # order by revenue
pl_stable <- pl %>% filter(between(year, 2009, 2014))  # osolate stable period
pl_stable <- pl %>% filter(port != "OTHER CA PORTS1")  # remove empty port category

pl_stable$year <- as.numeric(pl_stable$year)
pl_stable$fishery <- as.factor(pl_stable$fishery)
pl_stable$port <- as.factor(pl_stable$port)
pl_stable$gear <- as.factor(pl_stable$gear)

# Find total for each species, ignoring gear type
pl_species <- group_by(pl_stable, year, port, fishery) %>% 
  summarize(revenue = sum(ex.vessel_revenue))

# Create crazy graph with all species
all_ports <- ggplot(pl_species, aes(x = year, y = revenue, color = fishery)) + 
  theme_bw() +
  scale_color_viridis(discrete=TRUE) + #color of points from viridis
  geom_line() +  
  ylab("revenue") + xlab(" ") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~port, scale="free", ncol = 5)

ggsave(filename="Figures/all_ports.pdf", plot=all_ports,
       width=600, height=500, units="mm", dpi=300)

# Combine fishery & gear columns together to maintain all differences in fishery
pl_fishgear <- pl_stable %>% unite(fishgear, fishery, gear, sep = " - ")
pl_fishgear$fishgear <- as.factor(pl_fishgear$fishgear)

# Keep only top 5 highest revenue species per port per year
pl_highest <- pl_fishgear %>% 
  arrange(year, port, desc(ex.vessel_revenue)) %>% 
  group_by(year, port) %>%
  slice(1:3)

pl_highest <- as.data.frame(pl_highest)

# Plot top 5 revenue-producing species per port per year
top_revenue <- ggplot(pl_highest, aes(x = year, y = ex.vessel_revenue, color = fishgear)) + 
  theme_bw() +
  scale_color_viridis(discrete=TRUE) + #color of points from viridis
  geom_line() +  
  ylab("revenue") + xlab(" ") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~port, scale="free", ncol = 5)

ggsave(filename="Figures/top_revenue.pdf", plot=top_revenue,
       width=600, height=500, units="mm", dpi=300)


# General timeseries by port --------------------------------------------------
# Total revenue per port per year
pl_revenue <- group_by(pl, year, port) %>% summarize(revenue = sum(ex.vessel_revenue))
pl_revenue <- pl_revenue %>% filter(port != "OTHER CA PORTS1")  # remove empty port category

total_revenue <- ggplot(pl_revenue, aes(x = year, y = revenue)) + 
  theme_bw() +
  geom_line(size=1.5) +  
  ylab("overall revenue") + xlab(" ") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~port, ncol = 5)

ggsave(filename="Figures/total_revenue.pdf", plot=total_revenue,
       width=600, height=500, units="mm", dpi=300)


# Count of fisheries (species and gear) per port per year
pl_count <- count(pl_fishgear, year, port)

fisheries_number <- ggplot(pl_count, aes(x = year, y = n)) + 
  theme_bw() +
  geom_line(size=1.5) +  
  ylab("number of fisheries") + xlab(" ") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~port, ncol = 5)

ggsave(filename="Figures/fisheries_number.pdf", plot=fisheries_number,
       width=600, height=500, units="mm", dpi=300)

# Workflow for Dungeness Crab -------------------------------------------------
# Select only the dungeness crab data
crab_all <- port_landings %>% filter(fishery == "DUNGENESS CRAB")
crab <- crab_all %>% drop_na()

# NB: when finding the means, this aggregates different gear types.
# Find means by year
crab_yr_mean <- group_by(crab, year) %>% summarize(m = mean(ex.vessel_revenue))

# Find means by year & port
crab_yr_port_mean <- group_by(crab, year, port) %>% summarize(m = mean(ex.vessel_revenue))


# Workflow for salmon closure -------------------------------------------------
# Select salmon data
salmon_all <- port_landings %>% filter(fishery == "SALMON")
salmon_ports <- levels(factor(salmon_all$port))  # ports where salmon was fished

# Select all data for ports where salmon was fished at some point
sal_port_data <- filter(port_landings, port %in% salmon_ports)
sal_port_data <- sal_port_data %>% drop_na()

# Stable salmon port years 
stable_sal_ports <- filter(stable_years, port %in% salmon_ports)

# Remove NAs, salmon, and find mean revenue per year per port for stable years

# Find count of number of fisheries for salmon-catching ports by year
sal_port_count <- count(sal_port_data, year, port)
