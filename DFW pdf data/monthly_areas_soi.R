# Script for isolating the species of interest from the DFW dataset of monthly
# landings by area.

library(stringr)
library(stringi)
library(dplyr)
library(reshape2)
library(ggplot2)

# Import dataset of all areas
all_areas <- read.csv("Data/DFW areas/all_areas.csv")

# Find most-landed species ----------------------------------------------------
# Isolate total landings and find mean by year
high_landings <- all_areas[, c(1, 14:16)]
high_landings <- high_landings %>% group_by(area, Species) %>% 
  summarize(landings = mean(Landings))  

# Isolate each area & order by landings in descending order
total_e <- high_landings %>% filter(area == "Eureka")
total_e <- total_e[order(-total_e$landings), ]

total_fb <- high_landings %>% filter(area == "Fort Bragg")
total_fb <- total_fb[order(-total_fb$landings), ]

total_bb <- high_landings %>% filter(area == "Bodega Bay")
total_bb <- total_bb[order(-total_bb$landings), ]

total_sf <- high_landings %>% filter(area == "San Francisco")
total_sf <- total_sf[order(-total_sf$landings), ]

total_m <- high_landings %>% filter(area == "Monterey")
total_m <- total_m[order(-total_m$landings), ]

total_mb <- high_landings %>% filter(area == "Morro Bay")
total_mb <- total_mb[order(-total_mb$landings), ]

total_sb <- high_landings %>% filter(area == "Santa Barbara")
total_sb <- total_sb[order(-total_sb$landings), ]

total_la <- high_landings %>% filter(area == "Los Angeles")
total_la <- total_la[order(-total_la$landings), ]

total_sd <- high_landings %>% filter(area == "San Diego")
total_sd <- total_sd[order(-total_sd$landings), ]

# Create dataframe of highest landed species
high_landings <- rbind(total_e[1:11, 1:2], total_fb[1:11, 1:2], 
                       total_bb[1:11, 1:2], total_sf[1:11, 1:2], 
                       total_m[1:11, 1:2], total_mb[1:11, 1:2],
                       total_sb[1:11, 1:2], total_la[1:11, 1:2], 
                       total_sd[1:11, 1:2])

# Get list of unique species names (not fully unique :/ )
high_landings <- levels(factor(high_landings$Species))
print(high_landings)


# Fix known issues with species names -----------------------------------------
all_areas$Species <- str_replace_all(all_areas$Species, 
                                     c("Lobs ter California spiny" = "Lobster California spiny",
                                       "Anchovy  northern" = "Anchovy northern"))

# Isolate species of interest -------------------------------------------------
# Column names for the initial columns of the overall dataframe
initial_cols <- c("Species", "January", "February", "March", "April", "May", "June",
                  "July", "August", "September", "October", "November", "December", 
                  "Landings", "year", "area")

crab <- all_areas %>% filter(str_detect(Species, "Dungeness")) %>% bind_rows
lobster <- all_areas %>% filter(str_detect(Species, "Lobster")) %>% bind_rows
squid <- all_areas %>% filter(str_detect(Species, "Squid market")) %>% bind_rows
albacore <- all_areas %>% filter(str_detect(Species, "albacore")) %>% bind_rows
bigeye <- all_areas %>% filter(str_detect(Species, "Tuna bigeye")) %>% bind_rows
prawn <- all_areas %>% filter(str_detect(Species, "Prawn spot")) %>% bind_rows
swordfish <- all_areas %>% filter(str_detect(Species, "Swordfish")) %>% bind_rows
opah <- all_areas %>% filter(str_detect(Species, "Opah")) %>% bind_rows

# Herring roe -----------------------------------------------------------------
herring_roe <- all_areas %>% filter(str_detect(Species, 
                                               "Herring Pacific roe|Herring Roe")) %>% bind_rows
# Update dataframe with new species name
herring_roe <- herring_roe[, -1] %>% group_by(area, year) %>%
  summarize(across(January:Landings, sum))  
herring_roe <- cbind(rep("Herring Roe", length(herring_roe$year)), herring_roe[, c(3:15, 2, 1)])
colnames(herring_roe) <- initial_cols

# Red sea urchin --------------------------------------------------------------
urchin <- all_areas %>% filter(str_detect(Species, 
                                          "Sea urchin red|Urchin red")) %>% bind_rows
# Update dataframe with new species name
urchin <- urchin[, -1] %>% group_by(area, year) %>%
  summarize(across(January:Landings, sum))  
urchin <- cbind(rep("Red Sea Urchin", length(urchin$year)), urchin[, c(3:15, 2, 1)])
colnames(urchin) <- initial_cols

# Hagfish ---------------------------------------------------------------------
hagfish <- all_areas %>% filter(str_detect(Species, 
                                           "Hagfish|Hagfishes")) %>% bind_rows
# Update dataframe with new species name
hagfish <- hagfish[, -1] %>% group_by(area, year) %>%
  summarize(across(January:Landings, sum))  
hagfish <- cbind(rep("Hagfish", length(hagfish$year)), hagfish[, c(3:15, 2, 1)])
colnames(hagfish) <- initial_cols

# Shrimp ----------------------------------------------------------------------
shrimp <- all_areas %>% filter(str_detect(Species, 
                                          "Shrimp Pacific Ocean|Shrimp ocean pink")) %>% bind_rows
# Update dataframe with new species name
shrimp <- shrimp[, -1] %>% group_by(area, year) %>%
  summarize(across(January:Landings, sum))  
shrimp <- cbind(rep("Ocean Shrimp", length(shrimp$year)), shrimp[, c(3:15, 2, 1)])
colnames(shrimp) <- initial_cols

# Groundfish - from list here -------------------------------------------------
# https://wildlife.ca.gov/Conservation/Marine/Federal-Groundfish 
groundfish <- all_areas %>% filter(str_detect(Species, 
                                              "Halibut|Rockfish|Thornyhead|Sablefish|Skate|Shark leopard|Shark soupfin|Shark spiny dogfish|Ratfish|Cabezon|Greenling|Lingcod|Cod|Whiting|Scorpionfish|Flounder|Sole|Sanddab")) %>% bind_rows
# Update dataframe with new species name
groundfish <- groundfish[, -1] %>% group_by(area, year) %>%
  summarize(across(January:Landings, sum))  
groundfish <- cbind(rep("Groundfish", length(groundfish$year)), groundfish[, c(3:15, 2, 1)])
colnames(groundfish) <- initial_cols

# Salmon ----------------------------------------------------------------------
salmon <- all_areas %>% filter(str_detect(Species, "Salmon")) %>% bind_rows
salmon <- salmon %>% filter(!str_detect(Species, "Roe"))  # Remove salmon roe fishery
# Update dataframe with new species name
salmon <- salmon[, -1] %>%  group_by(area, year) %>% 
  summarize(across(January:Landings, sum))
salmon <- cbind(rep("Salmon", length(salmon$year)), salmon[, c(3:15, 2, 1)])
colnames(salmon) <- initial_cols

# Coastal pelagic species gathered from NOAA fisheries ------------------------
pelagics <- all_areas %>% filter(str_detect(Species, 
                                            "Sardine|Mackerel Pacific|Mackerel jack|Mackerel unspecified|Anchovy northern")) %>% bind_rows
# Update dataframe with new species name
pelagics <- pelagics[, -1] %>% group_by(area, year) %>% 
  summarize(across(January:Landings, sum))
pelagics <- cbind(rep("Pelagics", length(pelagics$year)), pelagics[, c(3:15, 2, 1)])
colnames(pelagics) <- initial_cols


# Category for everything not selected above ----------------------------------
other <- all_areas %>% filter(!str_detect(Species, "Dungeness"))
other <- other %>% filter(!str_detect(Species, "Lobster"))
other <- other %>% filter(!str_detect(Species, "Squid market"))
other <- other %>% filter(!str_detect(Species, "albacore"))
other <- other %>% filter(!str_detect(Species, "Tuna bigeye"))
other <- other %>% filter(!str_detect(Species, "Prawn spot"))
other <- other %>% filter(!str_detect(Species, "Swordfish"))
other <- other %>% filter(!str_detect(Species, "Opah"))
other <- other %>% filter(!str_detect(Species, "Herring Pacific roe"))
other <- other %>% filter(!str_detect(Species, "Sea urchin red"))
other <- other %>% filter(!str_detect(Species, "Urchin red"))
other <- other %>% filter(!str_detect(Species, "Hagfish"))
other <- other %>% filter(!str_detect(Species, "Hagfishes"))
other <- other %>% filter(!str_detect(Species, "Shrimp Pacific Ocean"))
other <- other %>% filter(!str_detect(Species, "Shrimp ocean pink"))
other <- other %>% filter(!str_detect(Species, "Halibut"))
other <- other %>% filter(!str_detect(Species, "Rockfish"))
other <- other %>% filter(!str_detect(Species, "Thornyhead"))
other <- other %>% filter(!str_detect(Species, "Sablefish"))
other <- other %>% filter(!str_detect(Species, "Skate"))
other <- other %>% filter(!str_detect(Species, "Shark leopard"))
other <- other %>% filter(!str_detect(Species, "Shark soupfin"))
other <- other %>% filter(!str_detect(Species, "Shark spiny dogfish"))
other <- other %>% filter(!str_detect(Species, "Ratfish"))
other <- other %>% filter(!str_detect(Species, "Cabezon"))
other <- other %>% filter(!str_detect(Species, "Greenling"))
other <- other %>% filter(!str_detect(Species, "Lingcod"))
other <- other %>% filter(!str_detect(Species, "Cod"))
other <- other %>% filter(!str_detect(Species, "Whiting"))
other <- other %>% filter(!str_detect(Species, "Scorpionfish"))
other <- other %>% filter(!str_detect(Species, "Flounder"))
other <- other %>% filter(!str_detect(Species, "Sole"))
other <- other %>% filter(!str_detect(Species, "Sanddab"))
other <- other %>% filter(!str_detect(Species, "Salmon"))
other <- other %>% filter(!str_detect(Species, "Sardine"))
other <- other %>% filter(!str_detect(Species, "Mackerel"))
other <- other %>% filter(!str_detect(Species, "Anchovy"))

other_species <- levels(factor(other$Species))
print(other_species)
other <- other[, -1] %>% group_by(area, year) %>% 
  summarize(across(January:Landings, sum))
other <- cbind(rep("other", length(other$year)), other[, c(3:15, 2, 1)])
colnames(other) <- initial_cols


# Create dataframe of all species of interest ---------------------------------
all_soi <- rbind(crab, lobster, squid, albacore, bigeye, prawn, swordfish, 
                 opah, herring_roe, urchin, hagfish, shrimp, groundfish, 
                 salmon, pelagics, other)

all_soi$Species <- trimws(all_soi$Species)  # trim white space from names

# Replace DFW species names with more understandable text
all_soi$Species <- str_replace_all(all_soi$Species, 
                                   c("Crab Dungeness" = "Dungeness Crab",
                                     "Lobster California spiny" = "Spiny Lobster",
                                     "Prawn spot" = "Spot Prawn",
                                     "Squid market" = "Market Squid",
                                     "Tuna albacore" = "Albacore Tuna",
                                     "Tuna bigeye" = "Bigeye Tuna"))
species <- levels(factor(all_soi$Species))  # list of species
print(species)  # check species list


# Check relative landings for the species of interest -------------------------
total_landings <- all_soi[, c(1, 14:16)]
total_landings <- total_landings %>% group_by(Species) %>% 
  summarize(landings = mean(Landings)) 

total_plot <- ggplot(total_landings, aes(y = landings, x = reorder(Species, -landings))) +
  geom_bar(position = "dodge", stat = "identity") +
  ylab("mean landings (lbs)") +
  ggtitle("data area") + 
  # scale_fill_discrete(breaks = area_order) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename="DFW pdf data/Figures/all_soi_landings.pdf", total_plot,
       width=300, height=100, units="mm", dpi=300)

# Write a .csv file with just the species of interest
write.csv(all_soi, "Data/dfw_areas_soi.csv", row.names = FALSE)
