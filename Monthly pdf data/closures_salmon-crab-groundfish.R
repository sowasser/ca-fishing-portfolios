library(tidyverse)
library(viridis)
library(ggsidekick)

# Read in cleaned data with only the fisheries of interest
fishyears <- read.csv("Data/DFW areas/fisheries_year_soi.csv")
fishyears$month <- factor(fishyears$month, levels = c("Nov", "Dec", "Jan", 
                                                      "Feb", "Mar", "Apr", 
                                                      "May", "Jun", "Jul",
                                                      "Aug", "Sep", "Oct"))

season_abbrev <- c("'99-'00", "'00-'01", "'01-'02", "'02-'03", "'03-'04", 
                   "'04-'05", "'05-'06", "'06-'07", "'07-'08", "'08-'09", 
                   "'09-'10", "'10-'11", "'11-'12", "'12-'13", "'13-'14", 
                   "'14-'15", "'15-'16", "'16-'17", "'17-'18", "'18-'19")

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
    year == "2007-2008" | year == "2008-2009" | year == "2016-2017" ~ "salmon closure",
    year == "2015-2016" ~ "crab closure",
    TRUE ~ "all open"  # all other years
  ))

# Plot total catch over open & closed years -----------------------------------
closures_plot <- ggplot(all_cgs, aes(x = year, y = landings, fill = factor(status))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_viridis(name = "fishery status", discrete = TRUE) +
  guides(fill = guide_legend(reverse = TRUE)) +  # reverse legend order
  xlab("season") + ylab("landings (lbs)") +
  theme_sleek() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = season_abbrev) +
  facet_wrap(~species, scale = "free", ncol = 3)

ggsave(filename = "Monthly pdf data/Figures/Closures/closures.pdf", 
       plot = closures_plot, width = 300, height = 80, units = "mm", dpi = 300)


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
    xlab("season") + ylab(paste(fishery, "landings (lbs)")) +
    theme_sleek() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete(labels = season_abbrev) +
    facet_wrap(~area, scale = "free_y", ncol = 3)  # only allow y-axis to vary
  
  ggsave(filename=paste("Monthly pdf data/Figures/Closures/", fishery, " closures.pdf", 
                        sep=""), 
         plot=plt, width=400, height=250, units="mm", dpi=300)
}

# Function call for each species 
plot_area_closures("Dungeness Crab")
plot_area_closures("Groundfish")
plot_area_closures("Salmon")
