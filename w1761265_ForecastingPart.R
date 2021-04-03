# --------------------------------------------------
# Name: Mohammed Nazhim Kalam
# Student ID: 2019281
# UoW ID: W1761265
# --------------------------------------------------

# FORECASTING PART 

# Installing package to read Excel Data-set
install.packages("readxl")
install.packages("fpp")
install.packages("fpp2")
install.packages("MLmetrics")
install.packages("lubridate")


# Loading the package
library(fpp)
library(MASS)
library(readxl)
library(neuralnet)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(fpp2)
library(e1071)
library(openxlsx)
library(MLmetrics)
library(lubridate)

# Reading the data-set "vehicles.xlsx"
df = read_excel("./GitHub/ML-DM-Course-Work/ExchangeUSD.xlsx")
View(df)

# Dropping unwanted columns
df = subset(df, select = -c(Wdy))
View(df)

# Renaming the Columns of the Data-frame
df = setNames(df, c("Date", "Rate"))
View(df)

# Data Collection
rates = df[,2]
View(rates)

dates = df[,1]
View(dates)
  
# min max scalar normalization
normalize = function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Function for converting dates into a decimal of its year 
converting_date = function(date_input){
  return(as.numeric(as.Date(date_input)))
}

# normalized data
rates_normalized = lapply(rates, normalize)
summary(rates_normalized)

# Converting dates
all_dates_in_decimal = lapply(dates, converting_date)
all_dates_in_decimal = lapply(all_dates_in_decimal, normalize)
View(data.frame(all_dates_in_decimal))
summary(all_dates_in_decimal)

# Combining data to a data-frame
final_dataset = data.frame(all_dates_in_decimal, rates_normalized)
View(final_dataset)

# Creating the Training Data
training_data = final_dataset[1:400,]
View(training_data)

# Creating the Testing Data
testing_data = final_dataset[401:500,]
View(testing_data)

# Training a model on the data
set.seed(1234)
model <- neuralnet(Rate~.,hidden=c(3,5,3),act.fct = "logistic",learningrate = 0.1,
                   linear.output = TRUE, data = training_data) 
plot(model)

# Testing the model on the Test dataset
testing_data_date = testing_data[,1]
testing_data_date = data.frame(testing_data_date)
View(testing_data_date)
predict_result = predict(model, testing_data_date)
predict_result

# Evaluation model performance









# Code References Used
# https://github.com/EviSfn/MLP-neural-network/blob/master/neuralnetwork.R
# https://github.com/alexsnow348/Exchange-Rate-Forecasting-Using-Ensemble-ANN-Model/blob/master/MLP.R
# https://github.com/cran/nnfor/blob/master/R/mlp.R
# https://www.rdocumentation.org/packages/lubridate/versions/1.7.10/topics/decimal_date
# https://www.gormanalysis.com/blog/dates-and-times-in-r-without-losing-your-sanity/


