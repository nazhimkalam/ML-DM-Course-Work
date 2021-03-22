# --------------------------------------------------
# Name: Mohammed Nazhim Kalam
# Student ID: 2019281
# UoW ID: W1761265
# --------------------------------------------------

# FORECASTING PART 

# Installing package to read Excel Data-set
install.packages("readxl")     # used to read excel data files


# Loading the package
library(readxl)

# Reading the data-set "vehicles.xlsx"
df = read_excel("./GitHub/ML-DM-Course-Work/ExchangeUSD.xlsx")
View(df)