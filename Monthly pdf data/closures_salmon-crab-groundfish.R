library(tidyverse)

# Read in cleaned data with only the fisheries of interest
fishyears <- read.csv("Data/DFW areas/fisheries_year_soi.csv")
fishyears$month <- factor(fishyears$month, levels = c("Nov", "Dec", "Jan", 
                                                      "Feb", "Mar", "Apr", 
                                                      "May", "Jun", "Jul",
                                                      "Aug", "Sep", "Oct"))

# Subset into salmon, dungeness crab, and groundfish --------------------------
cgs <- fishyears %>%
  filter(str_detect(species, "Dover Sole_Thornyhead_Sablefish|Pacific Whiting|Other Groundfish|Dungeness Crab|Salmon")) %>%
  mutate(species = str_replace_all(species, c("Dover Sole_Thornyhead_Sablefish" = "Groundfish",
                                              "Pacific Whiting" = "Groundfish",
                                              "Other Groundfish" = "Groundfish"))) %>%
  group_by(species, area, year, month) %>%
  summarize(landings = sum(landings, na.rm = TRUE))

# Find sum across all areas and all months & add colum with colors for graph
all_cgs <- cgs %>%
  group_by(species, year) %>%
  summarize(landings = sum(landings, na.rm = TRUE)) %>%
  mutate(fill_col = case_when(
    year == "2007-2008" | year == "2008-2009" ~ "blue",
    year == "2015-2016" ~ "red",
    TRUE ~ "grey50"  # all other years
  ))


# Plot mean catch over open & closed years ------------------------------------
closures_plot <- ggplot(all_cgs, aes(x = year, y = landings)) +
  geom_bar(position = "dodge", stat = "identity", fill = all_cgs$fill_col) +
  ylab("mean landings (lbs)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~species, scale = "free", ncol = 3)

ggsave(filename = "~/Desktop/closures.pdf", 
       plot = closures_plot, width = 400, height = 150, units = "mm", dpi = 300)
