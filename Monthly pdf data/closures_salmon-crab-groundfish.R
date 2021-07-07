library(tidyverse)
library(viridis)
library(ggsidekick)

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
  mutate(status = case_when(
    year == "2007-2008" | year == "2008-2009" ~ "salmon closure",
    year == "2015-2016" ~ "crab closure",
    TRUE ~ "all open"  # all other years
  ))

# Plot total catch over open & closed years -----------------------------------
closures_plot <- ggplot(all_cgs, aes(x = year, y = landings, fill = factor(status))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_viridis(name = "fishery status", discrete = TRUE) +
  guides(fill = guide_legend(reverse = TRUE)) +  # reverse legend order
  ylab("mean landings (lbs)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~species, scale = "free", ncol = 3)

ggsave(filename = "~/Desktop/closures.pdf", 
       plot = closures_plot, width = 400, height = 130, units = "mm", dpi = 300)


# Individual graphs for each species showing all areas ------------------------
cgs_yearly <- cgs %>%
  group_by(species, area, year) %>%
  summarize(landings = sum(landings, na.rm = TRUE)) %>%
  mutate(status = case_when(
    year == "2007-2008" | year == "2008-2009" ~ "salmon closure",
    year == "2015-2016" ~ "crab closure",
    TRUE ~ "all open"  # all other years
  ))

plot_area_closures <- function(fishery) {
  # Function creates an plot for each of the species of interest with a unique
  # title and saves them all to one folder with unique name.
  df <- cgs_yearly %>% filter(species == fishery)
  
  plt <- ggplot(df, aes(x = year, y = landings, fill = factor(status))) +
    geom_bar(position = "dodge", stat = "identity") +
    scale_fill_viridis(name = "fishery status", discrete = TRUE) +
    guides(fill = guide_legend(reverse = TRUE)) +  # reverse legend order
    ylab(paste(fishery, "landings (lbs)")) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~area, scale = "free_y", ncol = 3)  # only allow y-axis to vary
  
  ggsave(filename=paste("~/Desktop/", fishery, " closures.pdf", 
                        sep=""), 
         plot=plt, width=400, height=250, units="mm", dpi=300)
}

# Function call for each species 
plot_area_closures("Dungeness Crab")
plot_area_closures("Groundfish")
plot_area_closures("Salmon")


