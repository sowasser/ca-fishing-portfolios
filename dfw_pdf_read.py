"""
Script for reading in data from PDFs downloaded from the California Department
of Fish and Wildlife: https://wildlife.ca.gov/Fishing/Commercial/Landings

Following the Tabula-py tutorial here:
https://aegis4048.github.io/parse-pdf-files-while-retaining-structure-with-tabula-py
"""

import tabula

# dfs = tabula.read_pdf("DFW pdf data/Table8_2019_ADA.pdf", pages="all")

tabula.convert_into("DFW pdf data/Table8_2019_ADA.pdf",
                    "DFW pdf data/table8.csv",
                    pages="all",
                    output_format="csv")
