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

# Islolate species of interest ------------------------------------------------
halibut <- all_areas %>% filter(str_detect(Species, "Halibut California")) %>% bind_rows
crab <- all_areas %>% filter(str_detect(Species, "Dungeness")) %>% bind_rows
lobster <- all_areas %>% filter(str_detect(Species, "Lobster")) %>% bind_rows
squid <- all_areas %>% filter(str_detect(Species, "Squid market")) %>% bind_rows
albacore <- all_areas %>% filter(str_detect(Species, "albacore")) %>% bind_rows
prawn <- all_areas %>% filter(str_detect(Species, "Prawn spot")) %>% bind_rows
urchin <- all_areas %>% filter(str_detect(Species, "Sea urchin red")) %>% bind_rows
sablefish <- all_areas %>% filter(str_detect(Species, "Sablefish")) %>% bind_rows
swordfish <- all_areas %>% filter(str_detect(Species, "Swordfish")) %>% bind_rows

# Combine all rockfish groups together
rockfish <- all_areas %>% filter(str_detect(Species, "Rockfish")) %>% bind_rows
rockfish <- rockfish[, -1] %>%  # remove species column
  group_by(area, year) %>%  # group by area & year
  summarize(across(January:Landings, sum))  # find sum
rockfish <- rockfish[, c(3:15, 2, 1)]  # Move year & area columns to the end
rockfish <- cbind(rep("Rockfish", length(rockfish$year)), rockfish)
colnames(rockfish) <- initial_cols

# Combine all thornyhead groups together
thornyhead <- all_areas %>% filter(str_detect(Species, "Thornyhead")) %>% bind_rows
thornyhead <- thornyhead[, -1] %>%  # remove species column
  group_by(area, year) %>%  # group by area & year
  summarize(across(January:Landings, sum))  # find sum
thornyhead <- thornyhead[, c(3:15, 2, 1)]  # Move year & area columns to the end
thornyhead <- cbind(rep("Thornyhead", length(thornyhead$year)), thornyhead)
colnames(thornyhead) <- initial_cols

# Combine all salmon together
salmon <- all_areas %>% filter(str_detect(Species, "Salmon")) %>% bind_rows
salmon <- salmon %>% filter(!str_detect(Species, "Roe"))  # Remove salmon roe fishery
salmon <- salmon[, -1] %>%  # remove species column
  group_by(area, year) %>%  # group by area & year
  summarize(across(January:Landings, sum))  # find sum
salmon <- salmon[, c(3:15, 2, 1)]  # Move year & area columns to the end
salmon <- cbind(rep("Salmon", length(salmon$year)), salmon)
colnames(salmon) <- initial_cols

# Combine coastal pelagic species gathered from NOAA fisheries
sardine <- all_areas %>% filter(str_detect(Species, "Sardine")) %>% bind_rows
pac_mackerel <- all_areas %>% filter(str_detect(Species, "Mackerel Pacific")) %>% bind_rows
jack_mackerel <- all_areas %>% filter(str_detect(Species, "Mackerel jack")) %>% bind_rows
anchovy <- all_areas %>% filter(str_detect(Species, "Anchovy northern")) %>% bind_rows

pelagics <- rbind(sardine, pac_mackerel, jack_mackerel, anchovy)[, -1]  # remove species column
pelagics <- pelagics %>% group_by(area, year) %>%  # group by area & year
  summarize(across(January:Landings, sum))  #find sum
pelagics <- pelagics[, c(3:15, 2, 1)]  # Move year & area columns to the end
pelagics <- cbind(rep("Pelagics coastal", length(pelagics$year)), pelagics)
colnames(pelagics) <- initial_cols

# Create dataframe of all species of interest
all_soi <- rbind(halibut, crab, lobster, squid, albacore, prawn, urchin, sablefish,
                 rockfish, swordfish, thornyhead, salmon, pelagics)

# Write a .csv file with just the species of interest
write.csv(all_soi, "Data/dfw_areas_soi.csv", row.names = FALSE)


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

# Change column order to match other analyses - year starting in Nov
all_soi_means <- all_soi_means[, c(1, 2, 13, 14, 3:12)] 
all_soi_means2 <- melt(all_soi_means, id_vars = c("area", "species"))
colnames(all_soi_means2) <- c("species", "area", "month", "landings")

# Plot monthly means for stable period
monthly_areas_stable <- ggplot(all_soi_means2, aes(y = landings, x = month, fill = species)) +
  geom_bar(position = "dodge", stat = "identity") +
  ylab("mean landings (lbs)") + xlab("mean across 2009-2014") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~area, ncol = 3, scale = "free")

ggsave(filename="~/Desktop/area_monthly_landings_stable.pdf", monthly_areas_stable,
       width=400, height=250, units="mm", dpi=300)

# Yearly trends for species of interest ---------------------------------------

# Dungeness Crab
crab$year <- as.factor(crab$year)
crab2 <- crab[, c(15, 16, 12, 13, 2:11)]
colnames(crab2) <- plot_cols
crab_long <- melt(crab2, id_vars = c("area", "year"))
colnames(crab_long) <- c("year", "area", "month", "landings")

crab_monthly <- ggplot(crab_long, aes(y = landings, x = month, fill = area)) +
  geom_bar(position = "dodge", stat = "identity") +
  ylab("mean landings (lbs)") +
  ggtitle("Dungeness Crab") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~year, ncol = 4, scale = "free")

ggsave(filename="~/Desktop/crab_monthly.pdf", crab_monthly,
       width=400, height=250, units="mm", dpi=300)

# Salmon
salmon$year <- as.factor(salmon$year)
salmon2 <- salmon[, c(15, 16, 12, 13, 2:11)]
colnames(salmon2) <- plot_cols
salmon_long <- melt(salmon2, id_vars = c("area", "year"))
colnames(salmon_long) <- c("year", "area", "month", "landings")

salmon_monthly <- ggplot(salmon_long, aes(y = landings, x = month, fill = area)) +
  geom_bar(position = "dodge", stat = "identity") +
  ylab("mean landings (lbs)") +
  ggtitle("Salmon") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~year, ncol = 4, scale = "free")

ggsave(filename="~/Desktop/salmon_monthly.pdf", salmon_monthly,
       width=400, height=250, units="mm", dpi=300)
