# Ananlysis of the monthly landings data for all of California, downloaded from
# the CA Department of Natural Resources 
# (https://wildlife.ca.gov/Fishing/Commercial/Landings).
# The data has been converted to .csv format and manipulated for analysis.

library(stringr)
library(dplyr)
library(reshape2)
library(ggplot2)

# Read in each file with a unique name matching the year ----------------------
names <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
           "11", "12", "13", "14", "15", "16", "17", "18", "19")

for(n in names) {
  name <- paste("m", n, sep = "")
  assign(name, 
         read.csv(paste("Data/DFW all CA/m", n, ".csv", sep = ""), 
                  # Declare data types to make sure everything matches
                  stringsAsFactors = FALSE,
                  colClasses = c("character", rep("numeric", 14))))
}

# Subset dataframes by species of interest ------------------------------------
dfs <- list(m00, m01, m02, m03, m04, m05, m06, m07, m08, m09, m10, m11, m12,
            m13, m14, m15, m16, m17, m18, m19)

halibut <- lapply(dfs, function(x) x %>% 
                    filter(str_detect(Species, "Halibut California"))) %>% bind_rows
crab <- lapply(dfs, function(x) x %>% 
                 filter(str_detect(Species, "Dungeness"))) %>% bind_rows
lobster <- lapply(dfs, function(x) x %>% 
                    filter(str_detect(Species, "Lobster"))) %>% bind_rows
squid <- lapply(dfs, function(x) x %>% 
                  filter(str_detect(Species, "Squid market"))) %>% bind_rows
albacore <- lapply(dfs, function(x) x %>% 
                     filter(str_detect(Species, "albacore"))) %>% bind_rows
prawn <- lapply(dfs, function(x) x %>% 
                  filter(str_detect(Species, "Prawn spot"))) %>% bind_rows
urchin <- lapply(dfs, function(x) x %>% 
                   filter(str_detect(Species, "Sea urchin red"))) %>% bind_rows
sablefish <- lapply(dfs, function(x) x %>% 
                      filter(str_detect(Species, "Sablefish"))) %>% bind_rows
swordfish <- lapply(dfs, function(x) x %>% 
                      filter(str_detect(Species, "Swordfish"))) %>% bind_rows

# TODO: determine if rockfish & thornyhead should be further sub-sampled or combined
# Pull out and combine shelf-slope rockfish
rockfish_shelf <- lapply(dfs, function(x) x %>% 
                           filter(str_detect(Species, "Rockfish group shelf"))) %>% bind_rows
rockfish_slope <- lapply(dfs, function(x) x %>% 
                           filter(str_detect(Species, "Rockfish group slope"))) %>% bind_rows

rockfish <- rbind(rockfish_shelf, rockfish_slope)[, -1]  # remove species column to find sums
rockfish <- rockfish %>% group_by(year) %>% summarize(across(January:Landings, sum))  #find sum
rockfish <- rockfish[, c(2:14, 1)]  # Make year last column
rockfish <- cbind(rep("Rockfish shelf-slope", length(rockfish$year)), rockfish)
columns <- c("Species", "January", "February", "March", "April", "May", "June",
             "July", "August", "September", "October", "November", "December", 
             "Landings", "year")
colnames(rockfish) <- columns

thornyhead <- lapply(dfs, function(x) x %>% 
                       filter(str_detect(Species, "Thornyhead"))) %>% bind_rows

thornyhead <- thornyhead %>% group_by(year) %>% summarize(across(January:Landings, sum))  #find sum
thornyhead <- thornyhead[, c(2:14, 1)]  # Make year last column
thornyhead <- cbind(rep("Thornyhead", length(thornyhead$year)), thornyhead)
columns <- c("Species", "January", "February", "March", "April", "May", "June",
             "July", "August", "September", "October", "November", "December", 
             "Landings", "year")
colnames(thornyhead) <- columns


# Combine coastal pelagic species gathered from NOAA fisheries
sardine <- lapply(dfs, function(x) x %>% 
                    filter(str_detect(Species, "Sardine"))) %>% bind_rows
pac_mackerel <- lapply(dfs, function(x) x %>% 
                         filter(str_detect(Species, "Mackerel Pacific"))) %>% bind_rows
jack_mackerel <- lapply(dfs, function(x) x %>% 
                          filter(str_detect(Species, "Mackerel jack"))) %>% bind_rows
anchovy <- lapply(dfs, function(x) x %>% 
                    filter(str_detect(Species, "Anchovy northern"))) %>% bind_rows

pelagics <- rbind(sardine, pac_mackerel, jack_mackerel, anchovy)[, -1]  # remove species column
pelagics <- pelagics %>% group_by(year) %>% summarize(across(January:Landings, sum))  #find sum
pelagics <- pelagics[, c(2:14, 1)]  # Make year last column
pelagics <- cbind(rep("Pelagics coastal", length(pelagics$year)), pelagics)
colnames(pelagics) <- columns

# Create dataframe of all species of interest
soi <- rbind(halibut, crab, lobster, squid, albacore, prawn, urchin, sablefish,
             rockfish, swordfish, thornyhead, pelagics)

# Monthly means for years in the 'stable period' - 2009-2014 ------------------
# Filter for 2009-2014
soi_stable <- soi %>% filter(between(year, 2009, 2014))

# Find monthly mean for each year
soi_stable <- soi_stable[, -15]  # remove year column
soi_stable$Species <- str_trim(soi_stable$Species, side = "both")  # Remove extra white spaces

soi_means <- soi_stable %>% 
  group_by(Species) %>% 
  summarize(across(January:Landings, mean))

# Update species names
species <- c("dungeness crab", "halibut", "lobster", "coastal pelagics", 
             "spot prawn", "shelf-slope rockfish", "sablefish", "red urchin",
             "market squid", "swordfish", "thornyhead", "albacore tuna")
soi_means2 <- cbind(species, soi_means[, -1])

soi_means2 <- soi_means[, -14]  # Remove total landings

# Update data for plotting ----------------------------------------------------
colnames(soi_means2) <- c("species", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Change column order to match other analyses - year starting in Nov
soi_means2 <- soi_means2[, c(1, 12, 13, 2:11)] 
soi_means_long <- melt(soi_means2, id_vars = "species")
colnames(soi_means_long) <- c("species", "month", "landings")

# Plot monthly means for stable period
monthly_stable <- ggplot(soi_means_long, aes(y = landings, x = month)) +
  geom_bar(position = "dodge", stat = "identity") +
  ylab("mean landings (lbs)") + xlab("mean across 2009-2014") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~species, ncol = 4, scale = "free")

ggsave(filename="Figures/Monthly data/monthly_landings_stable.pdf", monthly_stable,
       width=400, height=250, units="mm", dpi=300)
