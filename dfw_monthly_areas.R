# Analysis of the data from the nine areas within California, gathered from the
# California Department of Natural Resources, accessed here: 
# https://wildlife.ca.gov/Fishing/Commercial/Landings

library(stringr)
library(dplyr)
library(reshape2)
library(ggplot2)

# Import dataset of all areas
all_areas <- read.csv("Data/DFW areas/all_areas.csv")

# Column names for the initial columns of the overall dataframe
initial_cols <- c("Species", "January", "February", "March", "April", "May", "June",
                  "July", "August", "September", "October", "November", "December", 
                  "Landings", "year", "area")

# Update species names for the graph legend
sp_names <- c("Dungeness Crab", "Groundfish", "Spiny Lobster", 
              "Coastal Pelagics", "Spot Prawn", "Salmon", "Red Sea Urchin",
              "Market Squid", "Swordfish","Albacore Tuna")

# Order of areas from North -> South
area_order <- c("Eureka", "Fort Bragg", "Bodega Bay", "San Francisco", 
                "Monterey", "Morro Bay", "Santa Barbara", "Los Angeles",
                "San Diego")


# Islolate species of interest ------------------------------------------------
crab <- all_areas %>% filter(str_detect(Species, "Dungeness")) %>% bind_rows
lobster <- all_areas %>% filter(str_detect(Species, "Lobster")) %>% bind_rows
squid <- all_areas %>% filter(str_detect(Species, "Squid market")) %>% bind_rows
albacore <- all_areas %>% filter(str_detect(Species, "albacore")) %>% bind_rows
prawn <- all_areas %>% filter(str_detect(Species, "Prawn spot")) %>% bind_rows
urchin <- all_areas %>% filter(str_detect(Species, "Sea urchin red")) %>% bind_rows
swordfish <- all_areas %>% filter(str_detect(Species, "Swordfish")) %>% bind_rows

# Groundfish - from list here:
# https://wildlife.ca.gov/Conservation/Marine/Federal-Groundfish 
groundfish <- all_areas %>% filter(str_detect(Species, "Halibut|Rockfish|
                                              Thornyhead|Sablefish|Skate|
                                              Shark leopard|Shark soupfin|
                                              Shark spiny dogfish|Ratfish|
                                              Cabezon|Greenling|Lingcod|Cod|
                                              Whiting|Scorpionfish|Flounder|
                                              Sole|Sanddab")) %>% bind_rows
# Update dataframe with new species name
groundfish <- groundfish[, -1] %>% group_by(area, year) %>%
  summarize(across(January:Landings, sum))  
groundfish <- cbind(rep("Groundfish", length(groundfish$year)), groundfish[, c(3:15, 2, 1)])
colnames(groundfish) <- initial_cols

# Salmon
salmon <- all_areas %>% filter(str_detect(Species, "Salmon")) %>% bind_rows
salmon <- salmon %>% filter(!str_detect(Species, "Roe"))  # Remove salmon roe fishery
# Update dataframe with new species name
salmon <- salmon[, -1] %>%  group_by(area, year) %>% 
  summarize(across(January:Landings, sum))
salmon <- cbind(rep("Salmon", length(salmon$year)), salmon[, c(3:15, 2, 1)])
colnames(salmon) <- initial_cols

# Coastal pelagic species gathered from NOAA fisheries
pelagics <- all_areas %>% filter(str_detect(Species, "Sardine|Mackerel Pacific|
                                            Mackerel jack|Anchovy northern")) %>% bind_rows
# Update dataframe with new species name
pelagics <- pelagics[, -1] %>% group_by(area, year) %>% 
  summarize(across(January:Landings, sum))
pelagics <- cbind(rep("Pelagics coastal", length(pelagics$year)), pelagics[, c(3:15, 2, 1)])
colnames(pelagics) <- initial_cols

# Create dataframe of all species of interest
all_soi <- rbind(crab, lobster, squid, albacore, prawn, urchin, swordfish, 
                 groundfish, salmon, pelagics)

# Write a .csv file with just the species of interest
write.csv(all_soi, "Data/dfw_areas_soi.csv", row.names = FALSE)


# All landings for the stable period ------------------------------------------
all_landings <- all_areas %>% group_by(area, year) %>% 
  summarize(across(January:Landings, sum))

all_landings_stable <- all_landings %>% filter(between(year, 2009, 2014))
all_landings_stable <- all_landings_stable[, -2]  # remove year column

all_landings_means <- all_landings_stable %>%
  group_by(area) %>%
  summarize(across(January:Landings, mean))


# Monthly averages over the stable period of species of interest --------------
# Filter for 2009-2014
all_soi_stable <- all_soi %>% filter(between(year, 2009, 2014))

# Find monthly mean for each year
all_soi_stable <- all_soi_stable[, -15]  # remove year column
all_soi_stable$Species <- str_trim(all_soi_stable$Species, side = "both")  # Remove extra white spaces

all_soi_means <- all_soi_stable %>% 
  group_by(Species, area) %>% 
  summarize(across(January:Landings, mean))

all_soi_means <- all_soi_means[, -15]  # Remove total landings


# Update data for plotting ----------------------------------------------------
plot_cols <- c("species", "area", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

colnames(all_soi_means) <- plot_cols

# Change column & row order to match analyses
all_soi_means <- all_soi_means[, c(1, 2, 13, 14, 3:12)]  # Year starts in Nov.
all_soi_means2 <- melt(all_soi_means, id_vars = c("area", "species"))
colnames(all_soi_means2) <- c("species", "area", "month", "landings")
all_soi_means2$area <- factor(all_soi_means2$area, levels = area_order)

# Plot monthly means for stable period
monthly_areas_stable <- ggplot(all_soi_means2, aes(y = landings, x = month, fill = species)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("mean landings (lbs)") + xlab("mean across 2009-2014") +
  scale_fill_hue(labels = sp_names) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~area, ncol = 3, scale = "free")

ggsave(filename="DFW pdf data/Figures/area_monthly_landings_stable.pdf", monthly_areas_stable,
       width=400, height=250, units="mm", dpi=300)


# Yearly trends for species of interest ---------------------------------------
# Dungeness Crab
crab$year <- as.factor(crab$year)
crab$area <- factor(crab$area, levels = area_order)
crab2 <- crab[, c(15, 16, 12, 13, 2:11)]
colnames(crab2) <- plot_cols
crab_long <- melt(crab2, id_vars = c("area", "year"))
colnames(crab_long) <- c("year", "area", "month", "landings")

crab_monthly <- ggplot(crab_long, aes(y = landings, x = month, fill = area)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("mean landings (lbs)") +
  ggtitle("Dungeness Crab") + 
  # scale_fill_discrete(breaks = area_order) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~year, ncol = 4, scale = "free")

ggsave(filename="DFW pdf data/Figures/crab_monthly.pdf", crab_monthly,
       width=400, height=250, units="mm", dpi=300)

# Salmon
salmon$year <- as.factor(salmon$year)
salmon$area <- factor(salmon$area, levels = area_order)
salmon2 <- salmon[, c(15, 16, 12, 13, 2:11)]
colnames(salmon2) <- plot_cols
salmon_long <- melt(salmon2, id_vars = c("area", "year"))
colnames(salmon_long) <- c("year", "area", "month", "landings")

salmon_monthly <- ggplot(salmon_long, aes(y = landings, x = month, fill = area)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("mean landings (lbs)") +
  ggtitle("Salmon") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~year, ncol = 4, scale = "free")

ggsave(filename="DFW pdf data/Figures/salmon_monthly.pdf", salmon_monthly,
       width=400, height=250, units="mm", dpi=300)

# Groundfish
groundfish$year <- as.factor(groundfish$year)
groundfish$area <- factor(groundfish$area, levels = area_order)
groundfish2 <- groundfish[, c(15, 16, 12, 13, 2:11)]
colnames(groundfish2) <- plot_cols
groundfish_long <- melt(groundfish2, id_vars = c("area", "year"))
colnames(groundfish_long) <- c("year", "area", "month", "landings")

groundfish_monthly <- ggplot(groundfish_long, aes(y = landings, x = month, fill = area)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("mean landings (lbs)") +
  ggtitle("groundfish") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~year, ncol = 4, scale = "free")

ggsave(filename="DFW pdf data/Figures/groundfish_monthly.pdf", groundfish_monthly,
       width=400, height=250, units="mm", dpi=300)
