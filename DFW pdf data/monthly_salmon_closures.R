# Script for examining response in monthly landings around the salmon closures
# in 2008 & 2009, using the CDFW dataset downloaded from here: 
# https://wildlife.ca.gov/Fishing/Commercial/Landings

library(stringr)
library(dplyr)
library(reshape2)
library(PMCMRplus)

# Read in cleaned data with only the fisheries of interest
all_soi <- read.csv("Data/dfw_areas_all_soi.csv")
all_soi <- all_soi[, -14]  # remove total landings column

# Remove more southern areas where salmon aren't fished
salmon_areas <- all_soi %>% 
  filter(!area %in% c("Santa Barbara", "Los Angeles", "San Diego"))

# Separate closed & open years ------------------------------------------------
closed_years <- c(2008, 2009)
before_years <- c(2000:2007)
after_years <- c(2010:2019)

# Find mean across years in each period, but maintain areas
closed <- salmon_areas %>%
  filter(year %in% closed_years) %>%
  group_by(Species, area) %>% 
  summarize(across(January:December, mean, na.rm = TRUE))
closed <- melt(closed, id.vars = c("Species", "area"))

before <- salmon_areas %>% 
  filter(year %in% before_years) %>%
  group_by(Species, area) %>% 
  summarize(across(January:December, mean, na.rm = TRUE))
before <- melt(before, id.vars = c("Species", "area"))

after <- salmon_areas %>% 
  filter(year %in% after_years) %>%
  group_by(Species, area) %>% 
  summarize(across(January:December, mean, na.rm = TRUE))
after <- melt(after, id.vars = c("Species", "area"))

# Find mean across all years and areas
closed_means <- salmon_areas %>%
  filter(year %in% closed_years) %>%
  group_by(Species) %>% 
  summarize(across(January:December, mean, na.rm = TRUE))
closed_means <- melt(closed_means, id.vars = c("Species"))

before_means <- salmon_areas %>%
  filter(year %in% before_years) %>%
  group_by(Species) %>% 
  summarize(across(January:December, mean, na.rm = TRUE))
before_means <- melt(before_means, id.vars = c("Species"))

after_means <- salmon_areas %>%
  filter(year %in% after_years) %>%
  group_by(Species) %>% 
  summarize(across(January:December, mean, na.rm = TRUE))
after_means <- melt(after_means, id.vars = c("Species"))


# Compare fisheries of interest correlated with salmon across periods ---------
# Dungeness Crab
crab_closed <- closed_means %>% filter(Species == "Dungeness Crab")
crab_before <- before_means %>% filter(Species == "Dungeness Crab")
crab_after <- after_means %>% filter(Species == "Dungeness Crab")

crab <- cbind(crab_closed[, 3], crab_before[, 3], crab_after[, 3])
colnames(crab) <- c("closed", "before", "after")
crab <- melt(crab, varnames = c("observation", "period"))

kruskal.test(crab$value ~ crab$period)  # p = 1.605e-07****
kwAllPairsNemenyiTest(crab$value ~ crab$period)

crab_salmonclosure <- ggplot(crab, aes(x = observation, y = value, fill = period)) +
  theme_bw() +
  geom_area(position = "identity", alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE) +
  #scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7 ,8 ,9, 10, 11, 12), 
  #                   labels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", 
  #                              "May", "Jun", "Jul", "Aug", "Sep", "Oct")) +
  xlab(" ") + ylab("mean landings") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename = "DFW pdf data/Figures/Salmon closure/crab_salmonclosure.pdf", 
       plot = crab_salmonclosure, width = 200, height = 130, units = "mm", dpi = 300)


