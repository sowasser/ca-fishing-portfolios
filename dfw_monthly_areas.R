# Analysis of the data from the nine areas within California, gathered from the
# California Department of Natural Resources, accessed here: 
# https://wildlife.ca.gov/Fishing/Commercial/Landings

library(stringr)
library(dplyr)
library(reshape2)
library(ggplot2)

# Import dataset of all areas
all_areas <- read.csv("Data/DFW areas/all_areas.csv")


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
columns <- c("Species", "January", "February", "March", "April", "May", "June",
             "July", "August", "September", "October", "November", "December", 
             "Landings", "year", "area")
colnames(rockfish) <- columns

# Combine all thornyhead groups together
thornyhead <- all_areas %>% filter(str_detect(Species, "Thornyhead")) %>% bind_rows
thornyhead <- thornyhead[, -1] %>%  # remove species column
  group_by(area, year) %>%  # group by area & year
  summarize(across(January:Landings, sum))  # find sum
thornyhead <- thornyhead[, c(3:15, 2, 1)]  # Move year & area columns to the end
thornyhead <- cbind(rep("Thornyhead", length(thornyhead$year)), thornyhead)
columns <- c("Species", "January", "February", "March", "April", "May", "June",
             "July", "August", "September", "October", "November", "December", 
             "Landings", "year", "area")
colnames(thornyhead) <- columns

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
colnames(pelagics) <- columns

# Create dataframe of all species of interest
all_soi <- rbind(halibut, crab, lobster, squid, albacore, prawn, urchin, sablefish,
                 rockfish, swordfish, thornyhead, pelagics)


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
colnames(all_soi_means) <- c("species", "area", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

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

