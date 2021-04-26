# --------------------------------------------------
# Name: Mohammed Nazhim Kalam
# Student ID: 2019281
# UoW ID: W1761265
# --------------------------------------------------

# FORECASTING PART 
# Installing package to read Excel Data-set
# install.packages("fpp")
# install.packages("MASS")
# install.packages("readxl")
# install.packages("neuralnet")
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("gridExtra")
# install.packages("fpp2")
# install.packages("e1071")
# install.packages("openxlsx")
# install.packages("MLmetrics")
# install.packages("lubridate")
# install.packages("Metrics")
# install.packages("tidyr")
# install.packages("graphics")

# Loading the package
library(fpp)
library(MASS)
library(readxl)
library(neuralnet)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(tseries)
library(fpp2)
library(e1071)
library(openxlsx)
library(MLmetrics)
library(lubridate)
library(Metrics)
library(tidyr)
library(graphics)


# Reading the data-set "vehicles.xlsx"
df = read_excel("./ExchangeUSD.xlsx")
View(df)

# Checking for null values present from the dataset
print(sum(is.na(df)))

# Checking for the summary of the data
print(summary(df))

# Dropping unwanted columns
df = subset(df, select = -c(Wdy, `YYYY/MM/DD`))
df$Rate = df$`USD/EUR`
df_copy = df

# Renaming the Columns of the Data-frame
df = setNames(df, c("Rate_Original", "Rate_Lag"))
View(df)

# Performing 'pacf', Partial Autocorrelation Function
rate = df$Rate_Original
pacf (rate, lag = 10)

# Variables used for 1 hidden and 2 hidden layers for the neural network
# NOTE: One of the two sets have to be commented while the other set is used for the model training

# -------------------------------------------------------------------
# # THIS SET OF VARIABLES IS USED FOR 1 HIDDEN LAYER MLP
# HIDDEN_LAYERS = c(6)
# ACTIVATION_FUNCTION = "logistic"
# LEARNING_RATE = 0.1

# THIS SET OF VARIABLES ARE USED FOR THE 2 HIDDEN LAYER MLP
HIDDEN_LAYERS = c(6,6)
ACTIVATION_FUNCTION = "logistic"
LEARNING_RATE = 0.08
# -------------------------------------------------------------------

# Looping the AR Order from 1 to 10 to get the one which performs the best
for (index in 1:10) { 
  # Using the saved dataframe copy 
  df = df_copy
  
  # Renaming the Columns of the Data-frame
  df = setNames(df, c("Rate_Original", "Rate_Lag"))
  
  # Shifting the Rate_Lag column rows by one down below for every loop
  for (loop in 1:index) {
    df['Rate_Lag'] <- c(NA, head(df['Rate_Lag'], dim(df)[1] - 1)[[1]])
  }
  
  # Removing the first row from the dataframe because there is a null value present in the Rate_Lag column
  df = drop_na(df)
  
  # normalization
  normalize = function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }
  
  # normalized data
  df.normalized = data.frame(lapply(df, normalize))
  View(df.normalized)
  
  # Creating the Training Data
  training_data = df.normalized[1:400,]
  
  # Creating the Testing Data
  testing_data = df.normalized[401:500-index,]
  View(testing_data)
  
  # Training a model on the data
  set.seed(101)
  
  # Training the model
  model <- neuralnet(Rate_Original~Rate_Lag,
                     hidden = HIDDEN_LAYERS,
                     data = training_data,
                     act.fct = ACTIVATION_FUNCTION,
                     linear.output = TRUE,
                     err.fct = "sse",
                     learningrate = LEARNING_RATE)

  # testing_data_actual_rate = data.frame(testing_data)
  predict_result = predict(model, testing_data)
  View(predict_result)
  
  # Evaluating the model
  actual = data.frame(testing_data)
  predicted = predict_result
  
  # Evaluation for the AR order number
  print("------------------------------------------")
  print(paste("Evaluation for the AR Order:", index))
  
  # Calculating the Mean Absolute Error
  mae = round(mae(actual$Rate_Original, predicted) * 100, digits = 4)
  print(paste("Mean Absolute Error: ", mae, " %", sep = ""))
  
  # Calculating the Root Mean Squared Error
  rmse = round(rmse(actual$Rate_Original, predicted) * 100, digits = 4)
  print(paste("Root Mean Squared Error: ", rmse, " %", sep = ""))
  
  # Calculating the Mean Absolute Percentage Error Loss
  mape = round(MAPE(actual$Rate_Original, predicted) * 100, digits = 4)
  print(paste("Mean Absolute Percentage Error Loss: ", mape, " %", sep = ""))
}

# USED FOR TUNING THE NEURAL NET PARAMETERS IN ORDER TO GET THE BEST SET OF PARAMETERS FOR THE MODEL
# PLEASE DONT UN-COMMENT AND RUN THIS BECAUSE THIS WAS USED ONLY FOR GETTING THE BEST SET OF NODES
# Loop from 1 to 10 node for one and two hidden layer to get the best node number for each layer
# for (x in 1:10) {
#   print("-----------------------------------------")
#   model <- neuralnet(Rate_Original~Rate_Lag,
#                      hidden=c(x),
#                      # hidden=c(6,x),
#                      data = training_data,
#                      act.fct = "logistic",
#                      linear.output = TRUE,
#                      err.fct = "sse",
#                      learningrate = 0.1)
#   
#   predict_result = predict(model, testing_data)
#   View(predict_result)
#   
#   # Evaluating the model
#   actual = data.frame(testing_data)
#   predicted = predict_result
#   
#   print(paste("Nodes:", x))
#   # Calculating the Mean Absolute Error
#   mae = round(mae(actual$Rate_Original, predicted) * 100, digits = 4)
#   print(paste("Mean Absolute Error: ", mae, " %", sep = ""))
#   
#   # Calculating the Root Mean Squared Error
#   rmse = round(rmse(actual$Rate_Original, predicted) * 100, digits = 4)
#   print(paste("Root Mean Squared Error: ", rmse, " %", sep = ""))
#   
#   # Calculating the Mean Absolute Percentage Error Loss
#   mape = round(MAPE(actual$Rate_Original, predicted) * 100, digits = 4)
#   print(paste("Mean Absolute Percentage Error Loss: ", mape, " %", sep = ""))
# }

# PLEASE DONT UN-COMMENT AND RUN THIS BECAUSE THIS WAS USED ONLY TO GET THE BEST LEARNING RATE
# Looping to get the best learning rate which gives the least amount of error rate for the 
# model (this includes 1 layer and 2 layer model with 6 nodes for both layers since that 
# is the optimal number of nodes found previously)
# learning_rate = 0
# while (learning_rate <= 0.1) {
#   learning_rate = learning_rate + 0.02
#   print(learning_rate)
#   model <- neuralnet(Rate_Original~Rate_Lag,
#                      hidden=c(6),
#                      # hidden=c(6,6),
#                      data = training_data,
#                      act.fct = "logistic",
#                      linear.output = TRUE,
#                      err.fct = "sse",
#                      learningrate = learning_rate)
#   
#   predict_result = predict(model, testing_data)
#   View(predict_result)
#   
#   # Evaluating the model
#   actual = data.frame(testing_data)
#   predicted = predict_result
#   
#   print(paste("learning_rate:", learning_rate))
#   # Calculating the Mean Absolute Error
#   mae = round(mae(actual$Rate_Original, predicted) * 100, digits = 4)
#   print(paste("Mean Absolute Error: ", mae, " %", sep = ""))
#   
#   # Calculating the Root Mean Squared Error
#   rmse = round(rmse(actual$Rate_Original, predicted) * 100, digits = 4)
#   print(paste("Root Mean Squared Error: ", rmse, " %", sep = ""))
#   
#   # Calculating the Mean Absolute Percentage Error Loss
#   mape = round(MAPE(actual$Rate_Original, predicted) * 100, digits = 4)
#   print(paste("Mean Absolute Percentage Error Loss: ", mape, " %", sep = ""))
# }  

# PLEASE DONT UN-COMMENT AND RUN THIS BECAUSE THIS WAS USED ONLY TO CHECK WHICH ACTIVATION FUNCTION
# PERFORMS BEST WITH THE FOUND BEST NODES AND LEARNING RATE VALUES
# activationFunction = "logistic"
# activationFunction = "tanh"
# model <- neuralnet(Rate~.,
#            # hidden=c(6),
#            hidden=c(6,6),
#            data = training_data,
#            act.fct = activationFunction,
#            linear.output = TRUE,
#            err.fct = "sse",
#            lifesign = "full",
#            learningrate = 0.1)
# }

# with one hidden layer and 6 nodes & two hidden layer with 6,6 nodes for the layers

# OPTIMUM RESULT FOR 1 HIDDEN LAYER MLP
# 6 NODES
# LEARNING RATE = 0.1
# ACTIVATION FUNCTION = LOGISTIC

# OPTIMUM RESULT FOR 2 HIDDEN LAYER MLP
# 6 6 NODES
# LEARNING RATE = 0.08
# ACTIVATION FUNCTION = LOGISTIC

# Since AR1 gave the best result we will be using that
# Using the saved dataframe copy 
df = df_copy

# Renaming the Columns of the Data-frame
df = setNames(df, c("Rate_Original", "Rate_Lag"))

# Shifting the Rate_Lag column rows by one down below
df['Rate_Lag'] <- c(NA, head(df['Rate_Lag'], dim(df)[1] - 1)[[1]])

# Removing the first row from the dataframe because there is a null value present in the Rate_Lag column
df = drop_na(df)

# normalization
normalize = function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# normalized data
df.normalized = data.frame(lapply(df, normalize))
View(df.normalized)

# Creating the Training Data
training_data = df.normalized[1:400,]

# Creating the Testing Data
testing_data = df.normalized[401:499,]
View(testing_data)

# Training a model on the data
set.seed(101)

# Training the model
model <- neuralnet(Rate_Original~Rate_Lag,
                   hidden = HIDDEN_LAYERS,
                   data = training_data, 
                   act.fct = ACTIVATION_FUNCTION, 
                   linear.output = TRUE,
                   err.fct = "sse", 
                   learningrate = LEARNING_RATE)

# Plotting the model network structure
plot(model)

# testing_data_actual_rate = data.frame(testing_data)
predict_result = predict(model, testing_data)

# Adding the index column
testing_data$Index = seq.int(nrow(testing_data))

# Plotting the graph
plot(testing_data$Index,testing_data$Rate_Original, 
     main = "Actual VS Predicted", xlab = "Index",
     ylab = "Rate", col = "black", type = "l")
lines(testing_data$Index, predict_result, col="red") 
legend("bottomright",                    # Add legend to plot
       legend = c("ACTUAL", "PREDICTED"),
       col = 1:2, 
       lty = 1,
       cex = 0.50)

# Evaluating the model
actual = data.frame(testing_data)
predicted = predict_result

# Displaying the predicted and the desired output
predicted_desired_output = data.frame(testing_data$Rate_Original, predicted)
predicted_desired_output = setNames(predicted_desired_output, c("Desired_Output", "Predicted_Output"))
View(predicted_desired_output)

# Calculating the Mean Absolute Error
mae = round(mae(actual$Rate_Original, predicted) * 100, digits = 4)
print(paste("Mean Absolute Error: ", mae, " %", sep = ""))

# Calculating the Root Mean Squared Error
rmse = round(rmse(actual$Rate_Original, predicted) * 100, digits = 4)
print(paste("Root Mean Squared Error: ", rmse, " %", sep = ""))

# Calculating the Mean Absolute Percentage Error Loss
mape = round(MAPE(actual$Rate_Original, predicted) * 100, digits = 4)
print(paste("Mean Absolute Percentage Error Loss: ", mape, " %", sep = ""))



# Best total number of nodes for hidden layer 1 is 6
# Best total number nodes for hidden layer 2 is 6


# Code References Used
# https://github.com/EviSfn/MLP-neural-network/blob/master/neuralnetwork.R
# https://github.com/alexsnow348/Exchange-Rate-Forecasting-Using-Ensemble-ANN-Model/blob/master/MLP.R
# https://github.com/cran/nnfor/blob/master/R/mlp.R
# https://www.rdocumentation.org/packages/lubridate/versions/1.7.10/topics/decimal_date
# https://www.gormanalysis.com/blog/dates-and-times-in-r-without-losing-your-sanity/