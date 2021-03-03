library(pdftools)
library(tidyverse)

# Read in pdf
pdf <- pdf_text("DFW pdf data/Table 8.pdf") %>% 
  str_split("\n")
  # readr::read_lines() %>%  # separate lines
  # read_fwf(fwf_empty(.)) %>%  # read as fixed-width file
  # mutate_at(-1, parse_number) %>%  # make numbers numbers
  # mutate(X1 = sub(',', '', X1, fixed = TRUE))  # remove commas in numbers NOT WORKING

columns <- c("species", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
             "Sep", "Oct", "Nov", "Dec", "landings")

# Remove page headers
for(i in 1:7) {
  pdf[[i]] <- pdf[[i]][-1:-5]
}


df2 <- as.data.frame(lapply(pdf, trimws), stringsAsFactors = FALSE)

# Remove lines you don't want
pdf2 <- pdf[-c(1:5, 41:45, 82:86, 94, 106, 123:127, 164:168, 204:208, 244:248, 254, 264, 278, 280:285), ]

# Attempt to break each row (an entire string) into separate strings
pdf3 <- pdf2 %>% str_squish() %>% strsplit(split = " ")

# Convert to dataframe
df <- plyr::ldply(pdf2)

library(tabulizer)
library(dplyr)

location <- "DFW pdf data/Table 8.pdf"
out <- extract_table(location)
