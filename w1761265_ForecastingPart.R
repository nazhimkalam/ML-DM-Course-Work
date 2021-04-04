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

# Checking for null values present from the dataset
print(sum(is.na(df)))

# Checking for the summary of the data
print(summary(df))

# Plot for Date VS Rate (with outliers present)
plot(df$Date, df$Rate, main = "Date VS Rate (with outliers)", xlab = "Date",
     ylab = "Rate", col = "red", type = "l")

# Checking for outliers present 
boxplot(df$Rate, ylab = "Rate") # Rate has outliers
boxplot(as.Date(df$Date), ylab = "Date") # Data has no outliers

# # Removing outliers from the "Rate" column dataset
# 
# # Calculating the upper and lower limit for the data
# bench_mark_upper = quantile(df$Rate, 0.75) + (1.5 * IQR(df$Rate))
# bench_mark_lower = quantile(df$Rate, 0.25) - (1.5 * IQR(df$Rate))
# 
# # Replacing the outliers with the upper and lower limits
# df$Rate[df$Rate > bench_mark_upper] = bench_mark_upper
# df$Rate[df$Rate < bench_mark_lower] = bench_mark_lower
# 
# boxplot(df$Rate, ylab = "Rate") # Rate has outliers
# 
# # Plotting the rate vs data graph (without outliers present)
# plot(df$Date, df$Rate, main = "Date VS Rate (without outliers)", xlab = "Date", ylab = "Rate",
#      col = "blue", type = "l")

# Data Collection
rates = df[,2]
View(rates)

dates = df[,1]
View(dates)
  
# min max scalar normalization
min_rate = min(rates)
max_rate = max(rates)
normalize = function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Function for converting dates into numbers
# converting_date = function(date_input){
#   return(as.numeric(as.Date(date_input)))
# }

converting_date = function(date_input){
  return(decimal_date(ymd(date_input)))
}

# normalized data
rates_normalized = data.frame(lapply(rates, normalize))
View(rates_normalized)
summary(rates_normalized)

# Converting dates
all_dates_in_decimal = lapply(dates, converting_date)
all_dates_in_decimal = lapply(all_dates_in_decimal, normalize)
View(data.frame(all_dates_in_decimal))
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
View(testing_data)

# Training a model on the data
set.seed(1234)
model <- neuralnet(Rate~.,hidden=c(3),  
                   data = training_data, 
                   act.fct = "logistic", 
                   linear.output = TRUE) 
plot(model)

# Testing the model on the Test dataset
View(final_dataset)
testing_data_date = data.frame(final_dataset$Date)
View(testing_data_date)
predict_result = predict(model, testing_data_date)
View(predict_result)

# un-normalizing the result to get the output rates which can be displayed to the user clearly
unnormalize = function(x, min, max){
  return( (max - min)*x + min )
}

# un-normalized predicted rate result
unnormalized_predicted_result = unnormalize(predict_result, min_rate, max_rate)
View(unnormalized_predicted_result)

# un-normalized actual rate result
unnormalized_testing_dataset = unnormalize(data.frame(final_dataset$Rate), min_rate, max_rate)
View(unnormalized_testing_dataset)

# Combining the predicted rate dataset and actual rate dataset
combined_rates = data.frame(unnormalized_predicted_result, unnormalized_testing_dataset)
combined_rates = setNames(combined_rates, c("Predicted Rates", "Actual Rates"))
View(combined_rates)

# plotting Actual Rate VS Predicted Rate result
plot(testing_data_date$final_dataset.Date, unnormalized_testing_dataset$final_dataset.Rate,
     main = "Actual VS Predicted",
     xlab = "Date", ylab = "Rate", col = "orange", type="l")
lines(testing_data_date$final_dataset.Date,unnormalized_predicted_result,col="green")
x = testing_data_date[401:500,]
y = unnormalized_predicted_result[401:500,]
lines(x,y,col="red")

# Evaluation model performance









# Code References Used
# https://github.com/EviSfn/MLP-neural-network/blob/master/neuralnetwork.R
# https://github.com/alexsnow348/Exchange-Rate-Forecasting-Using-Ensemble-ANN-Model/blob/master/MLP.R
# https://github.com/cran/nnfor/blob/master/R/mlp.R
# https://www.rdocumentation.org/packages/lubridate/versions/1.7.10/topics/decimal_date
# https://www.gormanalysis.com/blog/dates-and-times-in-r-without-losing-your-sanity/


