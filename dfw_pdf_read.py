"""
Script for reading in data from PDFs downloaded from the California Department
of Fish and Wildlife: https://wildlife.ca.gov/Fishing/Commercial/Landings

Following the Tabula-py tutorial here:
https://aegis4048.github.io/parse-pdf-files-while-retaining-structure-with-tabula-py
"""

import tabula
import pandas as pd

# # Read pdf into a list of dataframes
# table8 = tabula.read_pdf("DFW pdf data/Table8_2019_ADA.pdf", pages="all")
# all = pd.concat(table8)

# Convert pdf into csv
# tabula.convert_into("DFW pdf data/Table8_2019_ADA.pdf",
#                     "DFW pdf data/table8.csv",
#                     pages=[1, 2, 3, 4, 5, 6, 7, 8],
#                     output_format="csv",
#                     stream=True,
#                     area=(10, 0, 100, 100), relative_area=True)

tabula.convert_into("DFW pdf data/Table 8.pdf",
                    "DFW pdf data/table8.csv",
                    pages=[1, 2, 3, 4, 5, 7],
                    output_format="csv",
                    stream=True,
                    area=(10, 0, 100, 100), relative_area=True)
