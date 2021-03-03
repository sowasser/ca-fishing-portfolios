library(pdftools)
library(tidyverse)

# Read in pdf
pdf <- pdf_text("DFW pdf data/Table 8.pdf") %>% readr::read_lines()

columns <- c("species", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
             "Sep", "Oct", "Nov", "Dec", "landings")

# Remove lines you don't want
pdf2 <- pdf[-c(1:5, 14,            # Page 1
               41:46, 62,          # Page 2
               83:88, 96, 108,     # Page 3
               125:130, 139, 162,  # Page 4
               167:172,            # Page 5
               208:213, 228,       # Page 6
               249:254, 260, 270, 282, 284, 286:292  # Page 7
               )]

# Attempt to break each row (an entire string) into separate strings
pdf3 <- pdf2 %>% str_squish() %>% strsplit(split = " ")

# Convert to dataframe
df <- plyr::ldply(pdf3)
