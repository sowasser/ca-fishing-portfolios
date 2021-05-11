# Script for appending species timings to the California landings data, 
# organized by port from 1992-2014, downloaded from the California Natural 
# Resources Agency:
# https://data.cnra.ca.gov/dataset/human-uses-and-socioeconomic-dimensions-ca-north-coast-mpa-baseline-study-1992-2014

library(plyr)
library(tidyverse)
library(lubridate)
library(scales)

port_landings <- read.csv("Data/port_landings_updated.csv")


# Add season timings to the dataset -------------------------------------------
# Remove "smeltlike fish", where no season data are available
port_landings <- port_landings %>% filter(fishery != "SMELTLIKE FISH")

species <- levels(factor(port_landings$fishery))  # list of species
print(species)

opens <- c("July", "June 16", "January", "Year-round", "November 15", 
           "Year-round", "December", "Year-round", "early October", 
           "September", "Year-round", "June", "Year-round", "Year-round", 
           "Year-round", "Year-round", "Year-round", "May 1", "Year-round", 
           "Year-round", "Year-round", "Year-round", "late February", 
           "August 1", "Year-round", "Year-round", "July 1", "June")

closes <- c("August", "March 14", "October", "Year-round", "July 15", 
            "Year-round", "March", "Year-round", "mid-March", "March",
            "Year-round", "November", "Year-round", "Year-round", "Year-round",
            "Year-round", "Year-round", "October 31", "Year-round", 
            "Year-round", "Year-round", "Year-round", "September", "April 30",
            "Year-round", "Year-round", "March 22", "March")

# Number of days after Nov 15 - start of a new year for this project
# Calculations done here: https://www.timeanddate.com/date/duration.html?y1=2020&m1=11&d1=15&y2=2021&m2=3&d2=1
opens_count <- c(228, 213, 47, 1, 1, 1, 16, 1, 320, 290, 1, 198, 1, 1, 1, 1, 1,
                 167, 1, 1, 1, 1, 105, 259, 1, 1, 228, 198)

closes_count <- c(259, 119, 320, 364, 242, 364, 106, 364, 120, 106, 364, 351, 
                  364, 364, 364, 364, 364, 350, 364, 364, 364, 364, 290, 166,
                  364, 364, 127, 106)

# Combine into one dataframe to double check timings
seasons <- as.data.frame(cbind(species, opens, closes, opens_count, closes_count))

# Isolate the list of fisheries from all of the data to be replaced with the
# start and end dates (saved as new lists). 
fishery <- port_landings$fishery

start_day <- mapvalues(fishery, from = species, to = opens_count)
end_day <- mapvalues(fishery, from = species, to = closes_count)
# TODO: mapvalues is from plyr & a method from dplyr or elsewhere would be better

# Combine with original port landings data & export as .csv
pl_timings <- cbind(port_landings, start_day, end_day)
write.csv(pl_timings, file = "Data/port_landings_timings.csv", row.names = FALSE)

# Gantt chart for species of interest -----------------------------------------
tasks <- tribble(
  ~Start,       ~End,         ~Project,          ~Task,
  "2020-07-01", "2020-08-31", "Seasonal", "Albacore tuna",
  "2020-06-16", "2020-03-14", "Seasonal", "California halibut",
  "2020-01-01", "2020-10-31", "Seasonal", "Coastal pelagics",
  "2019-12-01", "2020-07-15", "Seasonal", "Dungeness Crab",
  "2019-10-01", "2020-03-15", "Seasonal", "Lobster",
  "2019-09-01", "2020-03-30", "Seasonal", "Market squid",
  "2019-11-15", "2020-11-14", "Year-round", "Red urchin",
  "2019-11-15", "2020-11-14", "Year-round", "Sablefish",
  "2020-05-01", "2020-10-31", "Seasonal", "Salmon",
  "2019-11-15", "2020-11-14", "Year-round", "Shelf-slope rockfish",
  "2020-02-20", "2020-09-01", "Seasonal", "Spot prawn",
  "2019-11-15", "2020-11-14", "Year-round", "Swordfish",
  "2019-11-15", "2020-11-14", "Year-round", "Thornyhead",
)

# Convert data too long for ggplot
tasks.long <- tasks %>%
  mutate(Start = ymd(Start),
         End = ymd(End)) %>%
  gather(date.type, task.date, -c(Project, Task)) %>%
  arrange(date.type, task.date) %>%
  mutate(Task = factor(Task, levels=rev(unique(Task)), ordered=TRUE))

# Custom theme for making a clean Gantt chart
theme_gantt <- function(base_size=11) {
  ret <- theme_bw()
  theme(panel.background = element_rect(fill="#ffffff", colour=NA),
        axis.title.x=element_text(vjust=-0.2), 
        axis.title.y=element_text(vjust=1.5),
        title=element_text(vjust=1.2),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.line=element_blank(),
        axis.ticks=element_blank(),
        legend.position="bottom", 
        axis.title=element_text(size=rel(0.8)),
        strip.text=element_text(size=rel(1)),
        strip.background=element_rect(fill="#ffffff", colour=NA),
        panel.spacing.y=unit(1.5, "lines"),
        legend.key = element_blank())
  
  ret
}

# Build plot
timeline <- ggplot(tasks.long, aes(x=Task, y=task.date, colour=Project)) + 
  geom_line(size=6) + 
  guides(colour=guide_legend(title=NULL)) +
  labs(x=NULL, y=NULL) + coord_flip() +
  scale_y_date(date_breaks="1 month", labels=date_format("%b")) +  # Change how the dates on x-axis display
  theme_gantt() + theme(axis.text.x=element_text(angle=45, hjust=1)) 
timeline

# TODO: Change units to make plot display well.
ggsave(timeline, filename = "Yearly port data/Figures/fisheries_gantt.png", 
       width = 250, height = 120, units = "mm", 
       dpi = 300, bg = "transparent")
