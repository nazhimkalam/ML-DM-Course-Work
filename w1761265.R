# --------------------------------------------------
# Name: Mohammed Nazhim Kalam
# Student ID: 2019281
# UoW ID: W1761265
# --------------------------------------------------

# CLUSTERING PART

# Installing package to read Excel Data-set
install.packages("readxl")

# Loading the package
library("readxl")

# Reading the data-set "vehicles.xlsx"
df = read_excel(file.choose())

# Viewing the head of the data-set
View(df)
