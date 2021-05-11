# Exploring overall California landings & fishery participation data for 
# 1992-2014, downloaded from the California Natural Resources Agency:
# https://data.cnra.ca.gov/dataset/human-uses-and-socioeconomic-dimensions-ca-north-coast-mpa-baseline-study-1992-2014
# Specifically: "Commercial Fishing - All Fisheries Summary - 1992-2014"

library(priceR)
library(reshape2)
library(ggplot2)
library(viridis)
library(patchwork)

ca_summary <- read.csv("Data/ca_summary_92-14.csv")

# Overall trends across timeseries --------------------------------------------
# Adjust revenue for inflation
revenue <- adjust_for_inflation(price = ca_summary$revenue, 
                                from_date = ca_summary$year, 
                                country = "US", 
                                to_date = 2014)

ca_adj <- cbind(ca_summary[, -3], revenue)

summary_long <- melt(ca_adj, id="year")  # convert to long form for faceted graph
summary_long$year <- as.numeric(summary_long$year)

# Create list to use as year labels in graph
years <- c("92", "93", "94", "95", "96", "97", "98", "99", "00", "01", "02", 
           "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14")

summary_plot <- ggplot(summary_long, aes(x = year, y = value)) + 
  theme_bw() +
  geom_line(size = 1.5) +  
  ylab(" ") + 
  scale_x_continuous(labels=years, breaks=ca_summary$year) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~variable, scale="free", ncol = 1)
# summary_plot  # view plot

ggsave(filename="yearly port data/Figures/summary_timeseries.pdf", plot=summary_plot,
       width=150, height=250, units="mm", dpi=300)

# Fisheries revenue vs. pounds landed -----------------------------------------
ca_summary$year <- factor(ca_summary$year)  # year as factor

pounds_revenue <- ggplot(ca_summary, aes(x = pounds, y = revenue, color = year)) + 
  theme_bw() +
  geom_point(size = 3.5) + 
  scale_color_viridis(discrete=TRUE) + #color of points from viridis
  xlab("landings (pounds)") + 
  ylab("revenue") + 
  theme(legend.title=element_blank()) +  # no legend title
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
# pounds_revenue  # view plot

summary(lm(ca_summary$revenue ~ ca_summary$pounds))

# Fisheries revenue vs. number of fishers -------------------------------------
fishers_revenue <- ggplot(ca_summary, aes(x = fishers, y = revenue, color = year)) + 
  theme_bw() +
  geom_point(size = 3.5) + 
  scale_color_viridis(discrete=TRUE) + 
  xlab("number of fishers") + 
  ylab("revenue") + 
  theme(legend.title=element_blank()) +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
# fishers_revenue  # view plot

summary(lm(ca_summary$revenue ~ ca_summary$fishers))

# Fisheries number of fishers vs. pounds landed -------------------------------
fishers_pounds <- ggplot(ca_summary, aes(x = fishers, y = pounds, color = year)) + 
  theme_bw() +
  geom_point(size = 3.5) + 
  scale_color_viridis(discrete=TRUE) + 
  xlab("number of fishers") + 
  ylab("landings (pounds)") + 
  theme(legend.title=element_blank()) +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
# fishers_pounds  # view plot

summary(lm(ca_summary$pounds ~ ca_summary$fishers))

# All scatter plots together --------------------------------------------------
# Combine plots with patchwork & "collect" legends together into one
all <- pounds_revenue + fishers_revenue + fishers_pounds + 
  plot_layout(guides="collect")

ggsave(filename="Yearly port data/Figures/summary_all.pdf", plot=all,
       width=350, height=100, units="mm", dpi=300)
