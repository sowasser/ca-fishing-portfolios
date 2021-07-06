library(tidyverse)

# Read in cleaned data with only the fisheries of interest
fishyears <- read.csv("Data/DFW areas/fisheries_year_soi.csv")
fishyears$month <- factor(fishyears$month, levels = c("Nov", "Dec", "Jan", 
                                                      "Feb", "Mar", "Apr", 
                                                      "May", "Jun", "Jul",
                                                      "Aug", "Sep", "Oct"))

# Subset into salmon, dungeness crab, and groundfish --------------------------
sdcg <- fishyears %>%
  filter(str_detect(species, "Dover Sole_Thornyhead_Sablefish|Pacific Whiting|Other Groundfish|Dungeness Crab|Salmon")) %>%
  mutate(species = str_replace_all(species, c("Dover Sole_Thornyhead_Sablefish" = "Groundfish",
                                              "Pacific Whiting" = "Groundfish",
                                              "Other Groundfish" = "Groundfish"))) %>%
  group_by(species, area, year, month) %>%
  summarize(landings = sum(landings, na.rm = TRUE))


# Find sum across all areas - represents all of California
all_ca_sdcg <- sdcg %>%
  group_by(species, year, month) %>%
  summarize(landings = sum(landings, na.rm = TRUE))

month_order <- c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                 "Aug", "Sep", "Oct")

# Identify closed years -------------------------------------------------------
domoic <- c("2015-2016")
salmon <- c("2007-2008", "2008-2009")
closed_all <- c(salmon, domoic)
