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

# Read in all landings data
port_landings <- read.csv("Data/port_landings_updated.csv")

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
  geom_line() +  
  ylab("revenue") + xlab(" ") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~port, scale="free", ncol = 5)

ggsave(filename="Yearly port data/Figures/all_ports.pdf", plot=all_ports,
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
  geom_line() +  
  ylab("revenue") + xlab(" ") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~port, scale="free", ncol = 5)

ggsave(filename="Yearly port data/Figures/top_revenue.pdf", plot=top_revenue,
       width=600, height=500, units="mm", dpi=300)

# Most revenue-producing species per port per year
pl_max <- pl_fishgear %>% 
  arrange(year, port, desc(ex.vessel_revenue)) %>% 
  group_by(year, port) %>%
  slice(1:1)

pl_max <- as.data.frame(pl_max)
pl_max$year <- as.factor(pl_max$year)

max_revenue <- ggplot(pl_max, aes(y = ex.vessel_revenue, x = year, fill = fishgear)) +
  geom_bar(position = "dodge", stat = "identity") +
  ylab("revenue") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~port) 

ggsave(filename="Yearly port data/Figures/max_revenue.pdf", max_revenue,
       width=600, height=500, units="mm", dpi=300)

# Most revenue-producing species per year
pl_max_yr <- pl_fishgear %>% 
  arrange(year, desc(ex.vessel_revenue)) %>% 
  group_by(year) %>%
  slice(1:1)

max_revenue_yr <- ggplot(pl_max_yr, aes(y = ex.vessel_revenue, x = year, fill = fishgear)) +
  geom_bar(position = "dodge", stat = "identity") +
  ylab("revenue") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename="Yearly port data/Figures/max_revenue_yr.pdf", max_revenue_yr,
       width=200, height=100, units="mm", dpi=300)

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

ggsave(filename="Yearly port data/Figures/total_revenue.pdf", plot=total_revenue,
       width=600, height=500, units="mm", dpi=300)

# Count of fisheries (species and gear) per port per year
pl_count <- count(pl_fishgear, year, port)

fisheries_number <- ggplot(pl_count, aes(x = year, y = n)) + 
  theme_bw() +
  geom_line(size=1.5) +  
  ylab("number of fisheries") + xlab(" ") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~port, ncol = 5)

ggsave(filename="Yearly port data/Figures/fisheries_number.pdf", plot=fisheries_number,
       width=600, height=500, units="mm", dpi=300)
