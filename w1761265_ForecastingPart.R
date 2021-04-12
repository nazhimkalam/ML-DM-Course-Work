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
View(df)

# Dropping unwanted columns
df = subset(df, select = -c(Wdy))

# Renaming the Columns of the Data-frame
df = setNames(df, c("Date", "Rate"))

# Checking for null values present from the dataset
print(sum(is.na(df)))

# Checking for the summary of the data
print(summary(df))

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

# Creating the Testing Data
testing_data = final_dataset[401:500,]

# Training a model on the data
set.seed(81)

# OPTIMUM RESULT FOR 1 HIDDEN LAYER MLP
# 6 NODES
# LEARNING RATE = 0.12
# ACTIVATION FUNCTION = LOGISTIC

# OPTIMUM RESULT FOR 2 HIDDEN LAYER MLP
# 6 6 NODES
# LEARNING RATE = 0.08
# ACTIVATION FUNCTION = LOGISTIC
training_data_copy = data.frame(training_data)
# View(training_data_copy)

for (index in 1:100) {
  model <- neuralnet(Rate~.,
                     # hidden=c(6),
                     hidden=c(6,6),
                     data = training_data_copy, 
                     act.fct = "logistic", 
                     linear.output = TRUE,
                     err.fct = "sse", 
                     learningrate = 0.08)
  
  date_dataset = data.frame(final_dataset$Date)
  date_dataset = data.frame(date_dataset[(200+index):(400+index),])
  
  predict_result = predict(model, date_dataset)
  
  training_data_copy = data.frame(date_dataset, predict_result) 
  training_data_copy[200:(400-index),] = training_data[(200+index):400,]
  
  names(training_data_copy)[1] = "Date"
  names(training_data_copy)[2] = "Rate"
}
view(training_data_copy)
plot(model)

# combine the first 1 to 400 date and rate with the training_data_copy
predict_dataset_final = rbind(training_data[1:200,], training_data_copy)
view(predict_dataset_final)

# un-normalizing the result to get the output rates which can be displayed to the user clearly
# unnormalize = function(x, min, max){
#   return( (max - min)*x + min )
# }

# un-normalized predicted rate result
# unnormalized_predicted_result = unnormalize(predict_result, min_rate, max_rate)
# View(unnormalized_predicted_result)
# 
# # un-normalized actual rate result
# unnormalized_actual_result = unnormalize(data.frame(final_dataset$Rate), min_rate, max_rate)
# unnormalized_actual_result = data.frame(unnormalized_actual_result[101:500,])
# View(unnormalized_actual_result)

# Combining the predicted rate dataset and actual rate dataset
# combined_rates = data.frame(unnormalized_predicted_result, unnormalized_actual_result)
# combined_rates = setNames(combined_rates, c("Predicted Rates", "Actual Rates"))
# # View(combined_rates)

# plotting Actual Rate VS Predicted Rate result
plot(final_dataset$Date, final_dataset$Rate,
     main = "Actual VS Predicted",
     xlab = "Date", ylab = "Rate", col = "orange", type="l")
lines(final_dataset$Date,predict_dataset_final$Rate,col="green")

testing_data_date = date_dataset[401:500,]
predicted_data_testing_rate = predicted_data_testing_rate[401:500,]

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

