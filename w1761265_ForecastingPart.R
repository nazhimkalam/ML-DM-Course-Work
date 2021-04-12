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
library(Metrics)

# Reading the data-set "vehicles.xlsx"
df = read_excel("./GitHub/ML-DM-Course-Work/ExchangeUSD.xlsx")
# View(df)

# Dropping unwanted columns
df = subset(df, select = -c(Wdy))
# View(df)

# Renaming the Columns of the Data-frame
df = setNames(df, c("Date", "Rate"))
# View(df)

# Checking for null values present from the dataset
print(sum(is.na(df)))

# Checking for the summary of the data
print(summary(df))

# Plotting the rate vs data graph (without outliers present)
plot(df$Date, df$Rate, main = "Date VS Rate", xlab = "Date", ylab = "Rate",
     col = "blue", type = "l")

# Data Collection
rates = df[,2]
# View(rates)

dates = df[,1]
# View(dates)
  
# min max scalar normalization
min_rate = min(rates)
max_rate = max(rates)
normalize = function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Function for converting dates into decimal date format
converting_date = function(date_input){
  return(decimal_date(ymd(date_input)))
}

# normalized data
rates_normalized = data.frame(lapply(rates, normalize))
# View(rates_normalized)
summary(rates_normalized)

# Converting dates
all_dates_in_decimal = lapply(dates, converting_date)
all_dates_in_decimal = lapply(all_dates_in_decimal, normalize)
# View(data.frame(all_dates_in_decimal))
summary(all_dates_in_decimal)

# Combining data to a data-frame
final_dataset = data.frame(all_dates_in_decimal, rates_normalized)
View(final_dataset)

# Plotting the Date VS Rate after normalization
plot(final_dataset$Date, final_dataset$Rate, main = "Date VS Rate (after normalization)",
     xlab = "Date", ylab = "Rate", col = "purple", type = "l")

# Creating the Training Data
training_data = final_dataset[1:400,]
View(training_data)

# Creating the Testing Data
testing_data = final_dataset[401:500,]
# View(testing_data)

# Training a model on the data
set.seed(80)

# Loop from 1 to 10 node for one and two hidden layer to get the best node number for each layer
# for (x in 6:10) {
#   print("-----------------------------------------")
#   model <- neuralnet(Rate~.,
#                      # hidden=c(x),
#                      hidden=c(6,x),
#                      data = training_data,
#                      act.fct = "logistic",
#                      linear.output = TRUE,
#                      err.fct = "sse",
#                      lifesign = "full",
#                      rep = 10,
#                      learningrate = 0.1)
# }

# Looping to get the best learning rate which gives the least amount of error rate for the 
# model (this includes 1 layer and 2 layer model with 6 nodes for both layers since that 
# is the optimal number of nodes found previously)
# learning_rate = 0
# while (learning_rate <= 0.5) {
#   learning_rate = learning_rate + 0.01
#   print(learning_rate)
#   model <- neuralnet(Rate~.,
#                      # hidden=c(6), 
#                      hidden=c(6,6),
#                      data = training_data, 
#                      act.fct = "logistic", 
#                      linear.output = TRUE,
#                      err.fct = "sse",
#                      lifesign = "full",
#                      learningrate = learning_rate)
# }

# with one hidden layer and 6 nodes & two hidden layer with 6,6 nodes for the layers

# OPTIMUM RESULT FOR 1 HIDDEN LAYER MLP
# 6 NODES
# LEARNING RATE = 0.12
# ACTIVATION FUNCTION = LOGISTIC

# OPTIMUM RESULT FOR 2 HIDDEN LAYER MLP
# 6 6 NODES
# LEARNING RATE = 0.08
# ACTIVATION FUNCTION = LOGISTIC
for (index in 1:101) {
  model <- neuralnet(Rate~.,
                     # hidden=c(6),
                     hidden=c(6,6),
                     data = training_data, 
                     act.fct = "logistic", 
                     linear.output = TRUE,
                     err.fct = "sse",
                     # lifesign = "full",
                     # rep = 10,
                     learningrate = 0.08)
  
  if(index != 101){
    date_dataset = data.frame(final_dataset$Date)
    date_dataset = data.frame(date_dataset[1:(400+index),])
  }
 
  predict_result = predict(model, date_dataset)
  training_data = data.frame(date_dataset, predict_result)
  names(training_data)[1] = "Date"
  names(training_data)[2] = "Rate"

}

View(training_data)


# model <- neuralnet(Rate~.,
#                    hidden=c(6),
#                    # hidden=c(6,6),
#                    data = training_data, 
#                    act.fct = "logistic", 
#                    linear.output = TRUE,
#                    err.fct = "sse",
#                    lifesign = "full",
#                    # rep = 10,
#                    learningrate = 0.08)
plot(model)

# Testing the model on the Test dataset
# View(final_dataset)
# date_dataset = data.frame(final_dataset$Date)
# date_dataset = data.frame(date_dataset[1:400,])
# # View(date_dataset)
# predict_result = predict(model, date_dataset)
# View(predict_result)

# un-normalizing the result to get the output rates which can be displayed to the user clearly
unnormalize = function(x, min, max){
  return( (max - min)*x + min )
}

# un-normalized predicted rate result
unnormalized_predicted_result = unnormalize(predict_result, min_rate, max_rate)
# View(unnormalized_predicted_result)

# un-normalized actual rate result
unnormalized_actual_result = unnormalize(data.frame(final_dataset$Rate), min_rate, max_rate)
# View(unnormalized_actual_result)

# Combining the predicted rate dataset and actual rate dataset
combined_rates = data.frame(unnormalized_predicted_result, unnormalized_actual_result)
combined_rates = setNames(combined_rates, c("Predicted Rates", "Actual Rates"))
View(combined_rates)

# plotting Actual Rate VS Predicted Rate result
# View(date_dataset$date_dataset.1..400...index....)
plot(date_dataset$date_dataset.1..400...index...., combined_rates$`Actual Rates`,
     main = "Actual VS Predicted",
     xlab = "Date", ylab = "Rate", col = "orange", type="l")
lines(date_dataset$date_dataset.1..400...index....,combined_rates$`Predicted Rates`,col="green")

testing_data_date = date_dataset[401:500,]
predicted_data_testing_rate = unnormalized_predicted_result[401:500,]

lines(testing_data_date, predicted_data_testing_rate, col="red")

# Evaluation model performances

# Filtering the Actual testing rates and the predicted rates from the model
actual = data.frame(final_dataset$Rate)[401:500,]
predicted = predict_result[401:500,]

# Calculating the Mean Absolute Error
mae = mae(actual, predicted) * 100
print(paste("Mean Absolute Error: ", mae, " %", sep = ""))

# Calculating the Root Mean Squared Error
rmse = rmse(actual, predicted) * 100
print(paste("Root Mean Squared Error: ", rmse, " %", sep = ""))

# Calculating the Mean Absolute Percentage Error Loss
mape = MAPE(actual, predicted) * 100
print(paste("Mean Absolute Percentage Error Loss: ", mape, " %", sep = ""))


# Best total number of nodes for hidden layer 1 is 6
# Best total number nodes for hidden layer 2 is 6


# Code References Used
# https://github.com/EviSfn/MLP-neural-network/blob/master/neuralnetwork.R
# https://github.com/alexsnow348/Exchange-Rate-Forecasting-Using-Ensemble-ANN-Model/blob/master/MLP.R
# https://github.com/cran/nnfor/blob/master/R/mlp.R
# https://www.rdocumentation.org/packages/lubridate/versions/1.7.10/topics/decimal_date
# https://www.gormanalysis.com/blog/dates-and-times-in-r-without-losing-your-sanity/


