# Script for examining response in monthly landings around the salmon closures
# in 2008 & 2009, using the CDFW dataset downloaded from here: 
# https://wildlife.ca.gov/Fishing/Commercial/Landings

library(stringr)
library(dplyr)
library(reshape2)
library(PMCMRplus)
library(vegan)

# Read in cleaned data with only the fisheries of interest
all_soi_original <- read.csv("Data/dfw_areas_all_soi.csv")
all_soi <- all_soi_original[, -14]  # remove total landings column

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

# Mean of total landings across all years & areas
closed_all <- all_soi_original %>%
  filter(year %in% closed_years)
closed_all <- closed_all[, c("Species", "Landings")]
closed_all <- closed_all %>%
  group_by(Species) %>%
  summarise_at(vars(Landings),
               list(landings = mean))

before_all <- all_soi_original %>%
  filter(year %in% before_years)
before_all <- before_all[, c("Species", "Landings")]
before_all <- before_all %>%
  group_by(Species) %>%
  summarise_at(vars(Landings),
               list(landings = mean))

after_all <- all_soi_original %>%
  filter(year %in% after_years)
after_all <- after_all[, c("Species", "Landings")]
after_all <- after_all %>%
  group_by(Species) %>%
  summarise_at(vars(Landings),
               list(landings = mean))


# Shannon index for species across all areas and years ------------------------
# These are unitless, so hard to compare. See:
# https://www.researchgate.net/post/Comparing-Shannon-Index-H-values-between-two-communities 
shannon_closed <- diversity(closed_all$landings)
shannon_before <- diversity(before_all$landings)
shannon_after <- diversity(after_all$landings)


# Compare fisheries of interest correlated with salmon across periods 
# Herring Roe -----------------------------------------------------------------
roe_closed <- closed_means %>% filter(Species == "Herring Roe")
roe_before <- before_means %>% filter(Species == "Herring Roe")
roe_after <- after_means %>% filter(Species == "Herring Roe")

roe <- cbind(roe_closed[, 3], roe_before[, 3], roe_after[, 3])
colnames(roe) <- c("closed", "before", "after")
roe <- melt(roe, varnames = c("observation", "period"))

kruskal.test(roe$value ~ roe$period)  # p = 0.2519
kwAllPairsNemenyiTest(roe$value ~ roe$period)

roe_salmonclosure <- ggplot(roe, aes(x = observation, y = value, fill = period)) +
  theme_bw() +
  geom_area(position = "identity", alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE) +
  #scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7 ,8 ,9, 10, 11, 12), 
  #                   labels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", 
  #                              "May", "Jun", "Jul", "Aug", "Sep", "Oct")) +
  xlab(" ") + ylab("mean landings") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename = "DFW pdf data/Figures/Salmon closure/roe_salmonclosure.pdf", 
       plot = roe_salmonclosure, width = 200, height = 130, units = "mm", dpi = 300)

# Ocean Shrimp ----------------------------------------------------------------
shrimp_closed <- closed_means %>% filter(Species == "Ocean Shrimp")
shrimp_before <- before_means %>% filter(Species == "Ocean Shrimp")
shrimp_after <- after_means %>% filter(Species == "Ocean Shrimp")

shrimp <- cbind(shrimp_closed[, 3], shrimp_before[, 3], shrimp_after[, 3])
colnames(shrimp) <- c("closed", "before", "after")
shrimp <- melt(shrimp, varnames = c("observation", "period"))

kruskal.test(shrimp$value ~ shrimp$period)  # p = 0.2536
kwAllPairsNemenyiTest(shrimp$value ~ shrimp$period)

shrimp_salmonclosure <- ggplot(shrimp, aes(x = observation, y = value, fill = period)) +
  theme_bw() +
  geom_area(position = "identity", alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE) +
  #scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7 ,8 ,9, 10, 11, 12), 
  #                   labels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", 
  #                              "May", "Jun", "Jul", "Aug", "Sep", "Oct")) +
  xlab(" ") + ylab("mean landings") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename = "DFW pdf data/Figures/Salmon closure/shrimp_salmonclosure.pdf", 
       plot = shrimp_salmonclosure, width = 200, height = 130, units = "mm", dpi = 300)

# Red Sea Urchin --------------------------------------------------------------
urchin_closed <- closed_means %>% filter(Species == "Red Sea Urchin")
urchin_before <- before_means %>% filter(Species == "Red Sea Urchin")
urchin_after <- after_means %>% filter(Species == "Red Sea Urchin")

urchin <- cbind(urchin_closed[, 3], urchin_before[, 3], urchin_after[, 3])
colnames(urchin) <- c("closed", "before", "after")
urchin <- melt(urchin, varnames = c("observation", "period"))

kruskal.test(urchin$value ~ urchin$period)  # p = 3.752e-06****
kwAllPairsNemenyiTest(urchin$value ~ urchin$period)

urchin_salmonclosure <- ggplot(urchin, aes(x = observation, y = value, fill = period)) +
  theme_bw() +
  geom_area(position = "identity", alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE) +
  #scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7 ,8 ,9, 10, 11, 12), 
  #                   labels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", 
  #                              "May", "Jun", "Jul", "Aug", "Sep", "Oct")) +
  xlab(" ") + ylab("mean landings") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename = "DFW pdf data/Figures/Salmon closure/urchin_salmonclosure.pdf", 
       plot = urchin_salmonclosure, width = 200, height = 130, units = "mm", dpi = 300)

# Dungeness Crab --------------------------------------------------------------
crab_closed <- closed_means %>% filter(Species == "Dungeness Crab")
crab_before <- before_means %>% filter(Species == "Dungeness Crab")
crab_after <- after_means %>% filter(Species == "Dungeness Crab")

crab <- cbind(crab_closed[, 3], crab_before[, 3], crab_after[, 3])
colnames(crab) <- c("closed", "before", "after")
crab <- melt(crab, varnames = c("observation", "period"))

kruskal.test(crab$value ~ crab$period)  # p = 0.4638
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

# Other Groundfish ------------------------------------------------------------
groundfish_closed <- closed_means %>% filter(Species == "Other Groundfish")
groundfish_before <- before_means %>% filter(Species == "Other Groundfish")
groundfish_after <- after_means %>% filter(Species == "Other Groundfish")

groundfish <- cbind(groundfish_closed[, 3], groundfish_before[, 3], groundfish_after[, 3])
colnames(groundfish) <- c("closed", "before", "after")
groundfish <- melt(groundfish, varnames = c("observation", "period"))

kruskal.test(groundfish$value ~ groundfish$period)  # p = 0.000138***
kwAllPairsNemenyiTest(groundfish$value ~ groundfish$period)

groundfish_salmonclosure <- ggplot(groundfish, aes(x = observation, y = value, fill = period)) +
  theme_bw() +
  geom_area(position = "identity", alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE) +
  #scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7 ,8 ,9, 10, 11, 12), 
  #                   labels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", 
  #                              "May", "Jun", "Jul", "Aug", "Sep", "Oct")) +
  xlab(" ") + ylab("mean landings") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename = "DFW pdf data/Figures/Salmon closure/groundfish_salmonclosure.pdf", 
       plot = groundfish_salmonclosure, width = 200, height = 130, units = "mm", dpi = 300)

# Pacific Whiting -------------------------------------------------------------
whiting_closed <- closed_means %>% filter(Species == "Pacific Whiting")
whiting_before <- before_means %>% filter(Species == "Pacific Whiting")
whiting_after <- after_means %>% filter(Species == "Pacific Whiting")

whiting <- cbind(whiting_closed[, 3], whiting_before[, 3], whiting_after[, 3])
colnames(whiting) <- c("closed", "before", "after")
whiting <- melt(whiting, varnames = c("observation", "period"))

kruskal.test(whiting$value ~ whiting$period)  # p = 0.005707***
kwAllPairsNemenyiTest(whiting$value ~ whiting$period)

whiting_salmonclosure <- ggplot(whiting, aes(x = observation, y = value, fill = period)) +
  theme_bw() +
  geom_area(position = "identity", alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE) +
  #scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7 ,8 ,9, 10, 11, 12), 
  #                   labels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", 
  #                              "May", "Jun", "Jul", "Aug", "Sep", "Oct")) +
  xlab(" ") + ylab("mean landings") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename = "DFW pdf data/Figures/Salmon closure/whiting_salmonclosure.pdf", 
       plot = whiting_salmonclosure, width = 200, height = 130, units = "mm", dpi = 300)

# Dover Sole / Thornyhead / Sablefish -----------------------------------------
dsts_closed <- closed_means %>% filter(Species == "Dover Sole_Thornyhead_Sablefish")
dsts_before <- before_means %>% filter(Species == "Dover Sole_Thornyhead_Sablefish")
dsts_after <- after_means %>% filter(Species == "Dover Sole_Thornyhead_Sablefish")

dsts <- cbind(dsts_closed[, 3], dsts_before[, 3], dsts_after[, 3])
colnames(dsts) <- c("closed", "before", "after")
dsts <- melt(dsts, varnames = c("observation", "period"))

kruskal.test(dsts$value ~ dsts$period)  # p = 0.0265*
kwAllPairsNemenyiTest(dsts$value ~ dsts$period)

dsts_salmonclosure <- ggplot(dsts, aes(x = observation, y = value, fill = period)) +
  theme_bw() +
  geom_area(position = "identity", alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE) +
  #scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7 ,8 ,9, 10, 11, 12), 
  #                   labels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", 
  #                              "May", "Jun", "Jul", "Aug", "Sep", "Oct")) +
  xlab(" ") + ylab("mean landings") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename = "DFW pdf data/Figures/Salmon closure/dsts_salmonclosure.pdf", 
       plot = dsts_salmonclosure, width = 200, height = 130, units = "mm", dpi = 300)

# Coastal Pelagics ------------------------------------------------------------
pelagics_closed <- closed_means %>% filter(Species == "Pelagics")
pelagics_before <- before_means %>% filter(Species == "Pelagics")
pelagics_after <- after_means %>% filter(Species == "Pelagics")

pelagics <- cbind(pelagics_closed[, 3], pelagics_before[, 3], pelagics_after[, 3])
colnames(pelagics) <- c("closed", "before", "after")
pelagics <- melt(pelagics, varnames = c("observation", "period"))

kruskal.test(pelagics$value ~ pelagics$period)  # p = 0.01297*
kwAllPairsNemenyiTest(pelagics$value ~ pelagics$period)

pelagics_salmonclosure <- ggplot(pelagics, aes(x = observation, y = value, fill = period)) +
  theme_bw() +
  geom_area(position = "identity", alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE) +
  #scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7 ,8 ,9, 10, 11, 12), 
  #                   labels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", 
  #                              "May", "Jun", "Jul", "Aug", "Sep", "Oct")) +
  xlab(" ") + ylab("mean landings") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename = "DFW pdf data/Figures/Salmon closure/pelagics_salmonclosure.pdf", 
       plot = pelagics_salmonclosure, width = 200, height = 130, units = "mm", dpi = 300)

# Market Squid ----------------------------------------------------------------
squid_closed <- closed_means %>% filter(Species == "Market Squid")
squid_before <- before_means %>% filter(Species == "Market Squid")
squid_after <- after_means %>% filter(Species == "Market Squid")

squid <- cbind(squid_closed[, 3], squid_before[, 3], squid_after[, 3])
colnames(squid) <- c("closed", "before", "after")
squid <- melt(squid, varnames = c("observation", "period"))

kruskal.test(squid$value ~ squid$period)  # p = 0.007092***
kwAllPairsNemenyiTest(squid$value ~ squid$period)

squid_salmonclosure <- ggplot(squid, aes(x = observation, y = value, fill = period)) +
  theme_bw() +
  geom_area(position = "identity", alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE) +
  #scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7 ,8 ,9, 10, 11, 12), 
  #                   labels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", 
  #                              "May", "Jun", "Jul", "Aug", "Sep", "Oct")) +
  xlab(" ") + ylab("mean landings") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename = "DFW pdf data/Figures/Salmon closure/squid_salmonclosure.pdf", 
       plot = squid_salmonclosure, width = 200, height = 130, units = "mm", dpi = 300)


