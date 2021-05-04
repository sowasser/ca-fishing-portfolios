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

# Read in cleaned data with only the fisheries of interest & new year ranges
fishyears <- read.csv("Data/DFW areas/fisheries_year_soi.csv")
fishyears$month <- factor(fishyears$month, levels = c("Nov", "Dec", "Jan", 
                                                      "Feb", "Mar", "Apr", 
                                                      "May", "Jun", "Jul",
                                                      "Aug", "Sep", "Oct"))

# Find sum across all areas - represents all of California
all_ca <- fishyears %>%
  group_by(species, year, month) %>%
  summarize(landings = sum(landings, na.rm = TRUE))

month_order <- c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                 "Aug", "Sep", "Oct")

# Separate closed & open years ------------------------------------------------
closed_years <- c("2007-2008", "2008-2009")
before_years <- c("1999-2000", "2000-2001", "2001-2002", "2002-2003", 
                  "2004-2005", "2005-2006", "2006-2007")
after_years <- c("2010-2011", "2011-2012", "2012-2013", "2013-2014", 
                 "2014-2015", "2015-2016", "2016-2017", "2017-2018", 
                 "2018-2019", "2019-2020")

# Find mean across all years and areas
closed_means <- all_ca %>% 
  filter(year %in% closed_years) %>%
  group_by(species, month) %>% 
  summarize(landings = mean(landings, na.rm = TRUE))

before_means <- all_ca %>%
  filter(year %in% before_years) %>%
  group_by(species, month) %>% 
  summarize(landings = mean(landings, na.rm = TRUE))

after_means <- all_ca %>%
  filter(year %in% after_years) %>%
  group_by(species, month) %>% 
  summarize(landings = mean(landings, na.rm = TRUE))

# Mean of total landings across all years, areas, and months
closed_all <- all_ca %>%
  filter(year %in% closed_years) %>%
  group_by(species) %>%
  summarize(landings = mean(landings, na.rm = TRUE))
closed_all <- cbind(rep("closed", length(closed_all$species)), closed_all)
colnames(closed_all) <- c("period", "species", "landings")

before_all <- all_ca %>%
  filter(year %in% before_years) %>%
  group_by(species) %>%
  summarize(landings = mean(landings, na.rm = TRUE))
before_all <- cbind(rep("before", length(before_all$species)), before_all)
colnames(before_all) <- c("period", "species", "landings")

after_all <- all_ca %>%
  filter(year %in% after_years) %>%
  group_by(species) %>%
  summarize(landings = mean(landings, na.rm = TRUE))
after_all <- cbind(rep("after", length(after_all$species)), after_all)
colnames(after_all) <- c("period", "species", "landings")

# Standard deviation of total landings across all years, areas, and months
closed_sd <- all_ca %>%
  filter(year %in% closed_years) %>%
  group_by(species) %>%
  summarize(stdev = sd(landings, na.rm = TRUE))

before_sd <- all_ca %>%
  filter(year %in% before_years) %>%
  group_by(species) %>%
  summarize(stdev = sd(landings, na.rm = TRUE))

after_sd <- all_ca %>%
  filter(year %in% after_years) %>%
  group_by(species) %>%
  summarize(stdev = sd(landings, na.rm = TRUE))


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

ggsave(filename = "DFW pdf data/Figures/Closures/salmon_all_species.pdf", 
       plot = all_species, width = 200, height = 320, units = "mm", dpi = 300)


# Compare fisheries of interest correlated with salmon across periods ---------
# Herring Roe
roe_closed <- closed_means %>% filter(species == "Herring Roe")
roe_closed <- cbind(rep("closed", length(roe_closed$species)), roe_closed)
colnames(roe_closed) <- c("period", "species", "month", "landings")

roe_before <- before_means %>% filter(species == "Herring Roe")
roe_before <- cbind(rep("before", length(roe_before$species)), roe_before)
colnames(roe_before) <- c("period", "species", "month", "landings")

roe_after <- after_means %>% filter(species == "Herring Roe")
roe_after <- cbind(rep("after", length(roe_after$species)), roe_after)
colnames(roe_after) <- c("period", "species", "month", "landings")

roe <- rbind(roe_before, roe_closed, roe_after)
roe$period <- factor(roe$period, levels = c("before", "closed", "after"))

kruskal.test(roe$landings ~ roe$period)  # p = 0.1802
kwAllPairsNemenyiTest(roe$landings ~ roe$period)

# Ocean Shrimp
shrimp_closed <- closed_means %>% filter(species == "Ocean Shrimp")
shrimp_closed <- cbind(rep("closed", length(shrimp_closed$species)), shrimp_closed)
colnames(shrimp_closed) <- c("period", "species", "month", "landings")

shrimp_before <- before_means %>% filter(species == "Ocean Shrimp")
shrimp_before <- cbind(rep("before", length(shrimp_before$species)), shrimp_before)
colnames(shrimp_before) <- c("period", "species", "month", "landings")

shrimp_after <- after_means %>% filter(species == "Ocean Shrimp")
shrimp_after <- cbind(rep("after", length(shrimp_after$species)), shrimp_after)
colnames(shrimp_after) <- c("period", "species", "month", "landings")

shrimp <- rbind(shrimp_before, shrimp_closed, shrimp_after)
shrimp$period <- factor(shrimp$period, levels = c("before", "closed", "after"))

kruskal.test(shrimp$landings ~ shrimp$period)  # p = 0.2145
kwAllPairsNemenyiTest(shrimp$landings ~ shrimp$period)

# Red Sea Urchin*** 
urchin_closed <- closed_means %>% filter(species == "Red Sea Urchin")
urchin_closed <- cbind(rep("closed", length(urchin_closed$species)), urchin_closed)
colnames(urchin_closed) <- c("period", "species", "month", "landings")

urchin_before <- before_means %>% filter(species == "Red Sea Urchin")
urchin_before <- cbind(rep("before", length(urchin_before$species)), urchin_before)
colnames(urchin_before) <- c("period", "species", "month", "landings")

urchin_after <- after_means %>% filter(species == "Red Sea Urchin")
urchin_after <- cbind(rep("after", length(urchin_after$species)), urchin_after)
colnames(urchin_after) <- c("period", "species", "month", "landings")

urchin <- rbind(urchin_before, urchin_closed, urchin_after)
urchin$period <- factor(urchin$period, levels = c("before", "closed", "after"))

kruskal.test(urchin$landings ~ urchin$period)  # p = 0.0001412***
kwAllPairsNemenyiTest(urchin$landings ~ urchin$period)

# Dungeness Crab 
crab_closed <- closed_means %>% filter(species == "Dungeness Crab")
crab_closed <- cbind(rep("closed", length(crab_closed$species)), crab_closed)
colnames(crab_closed) <- c("period", "species", "month", "landings")

crab_before <- before_means %>% filter(species == "Dungeness Crab")
crab_before <- cbind(rep("before", length(crab_before$species)), crab_before)
colnames(crab_before) <- c("period", "species", "month", "landings")

crab_after <- after_means %>% filter(species == "Dungeness Crab")
crab_after <- cbind(rep("after", length(crab_after$species)), crab_after)
colnames(crab_after) <- c("period", "species", "month", "landings")

crab <- rbind(crab_before, crab_closed, crab_after)
crab$period <- factor(crab$period, levels = c("before", "closed", "after"))

kruskal.test(crab$landings ~ crab$period)  # p = 0.3162
kwAllPairsNemenyiTest(crab$landings ~ crab$period)

# Other Groundfish*** 
groundfish_closed <- closed_means %>% filter(species == "Other Groundfish")
groundfish_closed <- cbind(rep("closed", length(groundfish_closed$species)), groundfish_closed)
colnames(groundfish_closed) <- c("period", "species", "month", "landings")

groundfish_before <- before_means %>% filter(species == "Other Groundfish")
groundfish_before <- cbind(rep("before", length(groundfish_before$species)), groundfish_before)
colnames(groundfish_before) <- c("period", "species", "month", "landings")

groundfish_after <- after_means %>% filter(species == "Other Groundfish")
groundfish_after <- cbind(rep("after", length(groundfish_after$species)), groundfish_after)
colnames(groundfish_after) <- c("period", "species", "month", "landings")

groundfish <- rbind(groundfish_before, groundfish_closed, groundfish_after)
groundfish$period <- factor(groundfish$period, levels = c("before", "closed", "after"))

kruskal.test(groundfish$landings ~ groundfish$period)  # p = 0.0001226***
kwAllPairsNemenyiTest(groundfish$landings ~ groundfish$period)

# Pacific Whiting* 
whiting_closed <- closed_means %>% filter(species == "Pacific Whiting")
whiting_closed <- cbind(rep("closed", length(whiting_closed$species)), whiting_closed)
colnames(whiting_closed) <- c("period", "species", "month", "landings")

whiting_before <- before_means %>% filter(species == "Pacific Whiting")
whiting_before <- cbind(rep("before", length(whiting_before$species)), whiting_before)
colnames(whiting_before) <- c("period", "species", "month", "landings")

whiting_after <- after_means %>% filter(species == "Pacific Whiting")
whiting_after <- cbind(rep("after", length(whiting_after$species)), whiting_after)
colnames(whiting_after) <- c("period", "species", "month", "landings")

whiting <- rbind(whiting_before, whiting_closed, whiting_after)
whiting$period <- factor(whiting$period, levels = c("before", "closed", "after"))

kruskal.test(whiting$landings ~ whiting$period)  # p = 0.0295*
kwAllPairsNemenyiTest(whiting$landings ~ whiting$period)

# Dover Sole / Thornyhead / Sablefish* 
dsts_closed <- closed_means %>% filter(species == "Dover Sole_Thornyhead_Sablefish")
dsts_closed <- cbind(rep("closed", length(dsts_closed$species)), dsts_closed)
colnames(dsts_closed) <- c("period", "species", "month", "landings")

dsts_before <- before_means %>% filter(species == "Dover Sole_Thornyhead_Sablefish")
dsts_before <- cbind(rep("before", length(dsts_before$species)), dsts_before)
colnames(dsts_before) <- c("period", "species", "month", "landings")

dsts_after <- after_means %>% filter(species == "Dover Sole_Thornyhead_Sablefish")
dsts_after <- cbind(rep("after", length(dsts_after$species)), dsts_after)
colnames(dsts_after) <- c("period", "species", "month", "landings")

dsts <- rbind(dsts_before, dsts_closed, dsts_after)
dsts$period <- factor(dsts$period, levels = c("before", "closed", "after"))

kruskal.test(dsts$landings ~ dsts$period)  # p = 0.02973*
kwAllPairsNemenyiTest(dsts$landings ~ dsts$period)

# Coastal Pelagics**** 
pelagics_closed <- closed_means %>% filter(species == "Pelagics")
pelagics_closed <- cbind(rep("closed", length(pelagics_closed$species)), pelagics_closed)
colnames(pelagics_closed) <- c("period", "species", "month", "landings")

pelagics_before <- before_means %>% filter(species == "Pelagics")
pelagics_before <- cbind(rep("before", length(pelagics_before$species)), pelagics_before)
colnames(pelagics_before) <- c("period", "species", "month", "landings")

pelagics_after <- after_means %>% filter(species == "Pelagics")
pelagics_after <- cbind(rep("after", length(pelagics_after$species)), pelagics_after)
colnames(pelagics_after) <- c("period", "species", "month", "landings")

pelagics <- rbind(pelagics_before, pelagics_closed, pelagics_after)
pelagics$period <- factor(pelagics$period, levels = c("before", "closed", "after"))

kruskal.test(pelagics$landings ~ pelagics$period)  # p = 6.408e-05****
kwAllPairsNemenyiTest(pelagics$landings ~ pelagics$period)

# Market Squid 
squid_closed <- closed_means %>% filter(species == "Market Squid")
squid_closed <- cbind(rep("closed", length(squid_closed$species)), squid_closed)
colnames(squid_closed) <- c("period", "species", "month", "landings")

squid_before <- before_means %>% filter(species == "Market Squid")
squid_before <- cbind(rep("before", length(squid_before$species)), squid_before)
colnames(squid_before) <- c("period", "species", "month", "landings")

squid_after <- after_means %>% filter(species == "Market Squid")
squid_after <- cbind(rep("after", length(squid_after$species)), squid_after)
colnames(squid_after) <- c("period", "species", "month", "landings")

squid <- rbind(squid_before, squid_closed, squid_after)
squid$period <- factor(squid$period, levels = c("before", "closed", "after"))

kruskal.test(squid$landings ~ squid$period)  # p = 0.8848
kwAllPairsNemenyiTest(squid$landings ~ squid$period)


# Plot species distributions across periods -----------------------------------
all_period_dist <- rbind(roe, shrimp, urchin, crab, groundfish, whiting, dsts, pelagics, squid)
dist_plots <- ggplot(all_period_dist, aes(x = month, y = landings, fill = period)) +
  theme_bw() +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis(discrete = TRUE) +
  xlab(" ") + ylab("mean landings") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~species, ncol = 3, scale = "free")

ggsave(filename = "DFW pdf data/Figures/Closures/salmon_monthly_periods.pdf", 
       plot = dist_plots, width = 300, height = 200, units = "mm", dpi = 300)


# Timeseries of mean landings per year for each fishery of interest -----------
# Filter by specific fisheries of interest
soi <- c("Herring Roe", "Ocean Shrimp", "Red Sea Urchin", "Dungeness Crab",
         "Other Groundfish", "Pacific Whiting", 
         "Dover Sole_Thornyhead_Sablefish", "Pelagics", "Market Squid")

years_mean_soi <- fishyears %>% filter(species %in% soi) %>%
  group_by(species, year) %>%
  summarize(landings = mean(landings, na.rm = TRUE))

years_mean_soi$year <- as.factor(years_mean_soi$year)

# Create faceted plot with 
years_mean_plot <- ggplot(years_mean_soi, aes(x = year, y = landings, 
                                              fill = factor(ifelse(year=="2007-2008" | year=="2008-2009", 
                                                                   "closed", "open")))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(name = "salmon fishery", values = c("black", "grey50")) +
  ylab("mean landings (lbs)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~species, scale = "free", ncol = 3)

ggsave(filename = "DFW pdf data/Figures/Closures/salmon_years_mean.pdf", 
       plot = years_mean_plot, width = 400, height = 300, units = "mm", dpi = 300)
