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
library(tidyr)
library(graphics)

#---------------------------------------------------------------------------
# Checking with AR1

# Reading the data-set "vehicles.xlsx"
df = read_excel("./ExchangeUSD.xlsx")
View(df)

# Dropping unwanted columns
df = subset(df, select = -c(Wdy, `YYYY/MM/DD`))
df$Rate = df$`USD/EUR`
df_copy = df

# Renaming the Columns of the Data-frame
df = setNames(df, c("Rate_Original", "Rate_Lag"))
View(df)

# Looping the AR Order from 1 to 10 to get the one which performs the best
for (index in 1:10) { 
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
  testing_data = df.normalized[401:500-index,]
  View(testing_data)
  
  # Training a model on the data
  set.seed(101)
  
  # Training the model
  model <- neuralnet(Rate_Original~Rate_Lag,
                     # hidden=c(6),
                     hidden=c(6,6),
                     data = training_data, 
                     act.fct = "logistic", 
                     linear.output = TRUE,
                     err.fct = "sse", 
                     learningrate = 0.08)
  
  # testing_data_actual_rate = data.frame(testing_data)
  predict_result = predict(model, testing_data)
  View(predict_result)
  
  # Evaluating the model
  actual = data.frame(testing_data)
  predicted = predict_result
  
  # Evluation for the AR order number
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

# Adding the index column
testing_data$Index = seq.int(nrow(testing_data))

# Plotting the graph
plot(testing_data$Index,testing_data$Rate_Original, 
     main = "Actual VS Predicted (using AR1 on Testing Data)", xlab = "Index",
     ylab = "Rate", col = "black", type = "l")
lines(testing_data$Index, predict_result, col="red") 
legend("bottomright",                    # Add legend to plot
       legend = c("ACTUAL", "PREDICTED"),
       col = 1:2, 
       lty = 1,
       cex = 0.50)
