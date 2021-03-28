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
library(ggplot2)

# Reading the data-set "vehicles.xlsx"
df = read_excel("./GitHub/ML-DM-Course-Work/ExchangeUSD.xlsx")
View(df)
plot(df$`USD/EUR`)

# Code References Used
# https://github.com/alexsnow348/Exchange-Rate-Forecasting-Using-Ensemble-ANN-Model/blob/master/MLP.R
# https://github.com/EviSfn/MLP-neural-network/blob/master/neuralnetwork.R
# https://github.com/cran/nnfor/blob/master/R/mlp.R



