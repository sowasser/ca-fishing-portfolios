# Script for examining response in monthly landings around the salmon closures
# in 2008 & 2009, using the CDFW dataset downloaded from here: 
# https://wildlife.ca.gov/Fishing/Commercial/Landings

library(stringr)
library(dplyr)
library(reshape2)
library(PMCMRplus)
library(vegan)
library(iNEXT)
library(ggplot2)
library(viridis)

# Read in cleaned data with only the fisheries of interest
all_soi <- read.csv("Data/dfw_areas_all_soi.csv")

# Remove more southern areas where salmon aren't fished
salmon_areas <- all_soi %>% 
  filter(!area %in% c("Santa Barbara", "Los Angeles", "San Diego"))

# Find sum across all areas - represents all of California
salmon_all <- salmon_areas %>%
  group_by(Species, year) %>%
  summarize(across(January:Landings, sum, na.rm = TRUE))

# Separate closed & open years ------------------------------------------------
closed_years <- c(2008, 2009)
before_years <- c(2000:2007)
after_years <- c(2010:2019)

# Find mean across all years and areas
closed_means <- salmon_all[, -15] %>%  # remove total landings column %>%
  filter(year %in% closed_years) %>%
  group_by(Species) %>% 
  summarize(across(January:December, mean, na.rm = TRUE))
closed_means <- melt(closed_means, id.vars = c("Species"))

before_means <- salmon_all[, -15] %>%  # remove total landings column
  filter(year %in% before_years) %>%
  group_by(Species) %>% 
  summarize(across(January:December, mean, na.rm = TRUE))
before_means <- melt(before_means, id.vars = c("Species"))

after_means <- salmon_all[, -15] %>%  # remove total landings column
  filter(year %in% after_years) %>%
  group_by(Species) %>% 
  summarize(across(January:December, mean, na.rm = TRUE))
after_means <- melt(after_means, id.vars = c("Species"))

# Mean of total landings across all years & areas
closed_all <- salmon_all %>%
  filter(year %in% closed_years)
closed_all <- closed_all[, c("Species", "Landings")]
closed_all <- closed_all %>%
  group_by(Species) %>%
  summarise_at(vars(Landings),
               list(landings = mean))
closed_all <- cbind(rep("closed", length(closed_all$Species)), closed_all)
colnames(closed_all) <- c("period", "species", "landings")

before_all <- salmon_all %>%
  filter(year %in% before_years)
before_all <- before_all[, c("Species", "Landings")]
before_all <- before_all %>%
  group_by(Species) %>%
  summarise_at(vars(Landings),
               list(landings = mean))
before_all <- cbind(rep("before", length(before_all$Species)), before_all)
colnames(before_all) <- c("period", "species", "landings")

after_all <- salmon_all %>%
  filter(year %in% after_years)
after_all <- after_all[, c("Species", "Landings")]
after_all <- after_all %>%
  group_by(Species) %>%
  summarise_at(vars(Landings),
               list(landings = mean))
after_all <- cbind(rep("after", length(after_all$Species)), after_all)
colnames(after_all) <- c("period", "species", "landings")

# Standard deviation of total landings across all years & areas
closed_sd <- salmon_all %>%
  filter(year %in% closed_years)
closed_sd <- closed_sd[, c("Species", "Landings")]
closed_sd <- closed_sd %>%
  group_by(Species) %>%
  summarise_at(vars(Landings),
               list(stdev = sd))

before_sd <- salmon_all %>%
  filter(year %in% before_years)
before_sd <- before_sd[, c("Species", "Landings")]
before_sd <- before_sd %>%
  group_by(Species) %>%
  summarise_at(vars(Landings),
               list(stdev = sd))

after_sd <- salmon_all %>%
  filter(year %in% after_years)
after_sd <- after_sd[, c("Species", "Landings")]
after_sd <- after_sd %>%
  group_by(Species) %>%
  summarise_at(vars(Landings),
               list(stdev = sd))


# Diversity for species across all areas and years ----------------------------
# Shannon index is unitless, so hard to compare. See:
# https://www.researchgate.net/post/Comparing-Shannon-Index-H-values-between-two-communities 
shannon_closed <- diversity(closed_all$landings)
shannon_before <- diversity(before_all$landings)
shannon_after <- diversity(after_all$landings)


# Plot of species abundances in the different periods -------------------------
all_long <- rbind(before_all, closed_all, after_all)  # combine all

# Add standard deviations error bars & make sure periods are in correct order
sd_all <- rbind(before_sd, closed_sd, after_sd)
all_long <- cbind(all_long, sd_all$stdev)
colnames(all_long) <- c("period", "species","landings", "stdev")
all_long$period <- factor(all_long$period, levels = c("before", "closed", "after"))

all_species <- ggplot(all_long, aes(x = species, y = landings)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = landings-stdev, ymax = landings+stdev), 
                width = .2, position = position_dodge(.9)) +
  ylab("mean landings (lbs)") + xlab("fisheries of interest") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~period, ncol = 1)

ggsave(filename = "DFW pdf data/Figures/Salmon closure/all_species.pdf", 
       plot = all_species, width = 200, height = 320, units = "mm", dpi = 300)


# Compare fisheries of interest correlated with salmon across periods 
# Herring Roe -----------------------------------------------------------------
roe_closed <- closed_means %>% filter(Species == "Herring Roe")
roe_before <- before_means %>% filter(Species == "Herring Roe")
roe_after <- after_means %>% filter(Species == "Herring Roe")

roe <- cbind(roe_closed[, 3], roe_before[, 3], roe_after[, 3])
colnames(roe) <- c("closed", "before", "after")
roe <- melt(roe, varnames = c("observation", "period"))

kruskal.test(roe$value ~ roe$period)  # p = 0.2196
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

ggsave(filename = "DFW pdf data/Figures/Salmon closure/Species distributions/roe_salmonclosure.pdf", 
       plot = roe_salmonclosure, width = 200, height = 130, units = "mm", dpi = 300)

# Ocean Shrimp ----------------------------------------------------------------
shrimp_closed <- closed_means %>% filter(Species == "Ocean Shrimp")
shrimp_before <- before_means %>% filter(Species == "Ocean Shrimp")
shrimp_after <- after_means %>% filter(Species == "Ocean Shrimp")

shrimp <- cbind(shrimp_closed[, 3], shrimp_before[, 3], shrimp_after[, 3])
colnames(shrimp) <- c("closed", "before", "after")
shrimp <- melt(shrimp, varnames = c("observation", "period"))

kruskal.test(shrimp$value ~ shrimp$period)  # p = 0.2462
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

ggsave(filename = "DFW pdf data/Figures/Salmon closure/Species distributions/shrimp_salmonclosure.pdf", 
       plot = shrimp_salmonclosure, width = 200, height = 130, units = "mm", dpi = 300)

# Red Sea Urchin*** -----------------------------------------------------------
urchin_closed <- closed_means %>% filter(Species == "Red Sea Urchin")
urchin_before <- before_means %>% filter(Species == "Red Sea Urchin")
urchin_after <- after_means %>% filter(Species == "Red Sea Urchin")

urchin <- cbind(urchin_closed[, 3], urchin_before[, 3], urchin_after[, 3])
colnames(urchin) <- c("closed", "before", "after")
urchin <- melt(urchin, varnames = c("observation", "period"))

kruskal.test(urchin$value ~ urchin$period)  # p = 0.0007889****
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

ggsave(filename = "DFW pdf data/Figures/Salmon closure/Species distributions/urchin_salmonclosure.pdf", 
       plot = urchin_salmonclosure, width = 200, height = 130, units = "mm", dpi = 300)

# Dungeness Crab --------------------------------------------------------------
crab_closed <- closed_means %>% filter(Species == "Dungeness Crab")
crab_before <- before_means %>% filter(Species == "Dungeness Crab")
crab_after <- after_means %>% filter(Species == "Dungeness Crab")

crab <- cbind(crab_closed[, 3], crab_before[, 3], crab_after[, 3])
colnames(crab) <- c("closed", "before", "after")
crab <- melt(crab, varnames = c("observation", "period"))

kruskal.test(crab$value ~ crab$period)  # p = 0.4504
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

ggsave(filename = "DFW pdf data/Figures/Salmon closure/Species distributions/crab_salmonclosure.pdf", 
       plot = crab_salmonclosure, width = 200, height = 130, units = "mm", dpi = 300)

# Other Groundfish*** ---------------------------------------------------------
groundfish_closed <- closed_means %>% filter(Species == "Other Groundfish")
groundfish_before <- before_means %>% filter(Species == "Other Groundfish")
groundfish_after <- after_means %>% filter(Species == "Other Groundfish")

groundfish <- cbind(groundfish_closed[, 3], groundfish_before[, 3], groundfish_after[, 3])
colnames(groundfish) <- c("closed", "before", "after")
groundfish <- melt(groundfish, varnames = c("observation", "period"))

kruskal.test(groundfish$value ~ groundfish$period)  # p = 0.0001326***
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

ggsave(filename = "DFW pdf data/Figures/Salmon closure/Species distributions/groundfish_salmonclosure.pdf", 
       plot = groundfish_salmonclosure, width = 200, height = 130, units = "mm", dpi = 300)

# Pacific Whiting*** ----------------------------------------------------------
whiting_closed <- closed_means %>% filter(Species == "Pacific Whiting")
whiting_before <- before_means %>% filter(Species == "Pacific Whiting")
whiting_after <- after_means %>% filter(Species == "Pacific Whiting")

whiting <- cbind(whiting_closed[, 3], whiting_before[, 3], whiting_after[, 3])
colnames(whiting) <- c("closed", "before", "after")
whiting <- melt(whiting, varnames = c("observation", "period"))

kruskal.test(whiting$value ~ whiting$period)  # p = 0.005646***
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

ggsave(filename = "DFW pdf data/Figures/Salmon closure/Species distributions/whiting_salmonclosure.pdf", 
       plot = whiting_salmonclosure, width = 200, height = 130, units = "mm", dpi = 300)

# Dover Sole / Thornyhead / Sablefish* ----------------------------------------
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

ggsave(filename = "DFW pdf data/Figures/Salmon closure/Species distributions/dsts_salmonclosure.pdf", 
       plot = dsts_salmonclosure, width = 200, height = 130, units = "mm", dpi = 300)

# Coastal Pelagics* -----------------------------------------------------------
pelagics_closed <- closed_means %>% filter(Species == "Pelagics")
pelagics_before <- before_means %>% filter(Species == "Pelagics")
pelagics_after <- after_means %>% filter(Species == "Pelagics")

pelagics <- cbind(pelagics_closed[, 3], pelagics_before[, 3], pelagics_after[, 3])
colnames(pelagics) <- c("closed", "before", "after")
pelagics <- melt(pelagics, varnames = c("observation", "period"))

kruskal.test(pelagics$value ~ pelagics$period)  # p = 0.02973*
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

ggsave(filename = "DFW pdf data/Figures/Salmon closure/Species distributions/pelagics_salmonclosure.pdf", 
       plot = pelagics_salmonclosure, width = 200, height = 130, units = "mm", dpi = 300)

# Market Squid*** -------------------------------------------------------------
squid_closed <- closed_means %>% filter(Species == "Market Squid")
squid_before <- before_means %>% filter(Species == "Market Squid")
squid_after <- after_means %>% filter(Species == "Market Squid")

squid <- cbind(squid_closed[, 3], squid_before[, 3], squid_after[, 3])
colnames(squid) <- c("closed", "before", "after")
squid <- melt(squid, varnames = c("observation", "period"))

kruskal.test(squid$value ~ squid$period)  # p = 0.005244***
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

ggsave(filename = "DFW pdf data/Figures/Salmon closure/Species distributions/squid_salmonclosure.pdf", 
       plot = squid_salmonclosure, width = 200, height = 130, units = "mm", dpi = 300)


# Timeseries of mean landings per year for each fishery of interest -----------
years_mean <- all_ca[, c("Species", "year", "Landings")]

# Filter by specific fisheries of interest
soi <- c("Herring Roe", "Ocean Shrimp", "Red Sea Urchin", "Dungeness Crab",
         "Other Groundfish", "Pacific Whiting", 
         "Dover Sole_Thornyhead_Sablefish", "Pelagics", "Market Squid")
years_mean_soi <- years_mean %>% filter(Species %in% soi)
years_mean_soi$year <- as.factor(years_mean_soi$year)

# Create faceted plot with 
years_mean_plot <- ggplot(years_mean_soi, aes(x = year, y = Landings, 
                                              fill = factor(ifelse(year==2008 | year==2009, 
                                                                   "closed", "open")))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(name = "salmon fishery", values = c("black", "grey50")) +
  ylab("mean landings (lbs)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Species, scale = "free", ncol = 3)

ggsave(filename = "DFW pdf data/Figures/Salmon closure/years_mean.pdf", 
       plot = years_mean_plot, width = 300, height = 200, units = "mm", dpi = 300)
