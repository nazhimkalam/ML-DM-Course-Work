# --------------------------------------------------
# Name: Mohammed Nazhim Kalam
# Student ID: 2019281
# UoW ID: W1761265
# --------------------------------------------------

# CLUSTERING PART 

# Installing package to read Excel Data-set
# install.packages("readxl")     # used to read excel data files
# install.packages("factoextra") # used to determine the optimal number clusters
# install.packages("NbClust")    # used to compute about multiple methods at once,
# # in order to find the optimal number of clusters.
# install.packages("factoextra") # used to plot the clusters out

# Loading the package
library(readxl)
library(knitr)
library(tidymodels)
library(janitor)
library(flexclust)
library(dplyr)
library(factoextra)
library(NbClust)
library(haven)
library(factoextra)

# Reading the data-set "vehicles.xlsx"
df = read_excel("./vehicles.xlsx")
View(df)

# converting the class column into factors because we are not able to get its count
df = mutate(df, Class = as_factor(df$Class))

# Displaying the types of unique classes present in the data-set
summary(df)

# Removing the Sample index column and the class column from the data-set
df.filtered = subset(df, select = -c(Samples, Class))

# Viewing the filtered data-set
View(df.filtered)

# [PRE-PROCESSING DATA] PERFORMING SCALING AND OUTLIERS REMOVAL

# Checking for any null values present in the data-set (returned 0 so no null values present)
print(sum(is.na(df.filtered)))

# Checking the Summary of the data-set (We can see the stats of the data columns eg: mean, median etc.)
df.summary = summary(df.filtered)

# Plotting a box plot graph (Box plots are useful to detect potential outliers from the data-set)
display.boxplot = function(data, column.name){
  boxplot(data, ylab = column.name)
}

# calling the display.boxplot function to display the boxplot data representation for each column.
display.boxplot(df.filtered$Comp, "Comp")
display.boxplot(df.filtered$Circ, "Circ")
display.boxplot(df.filtered$D.Circ, "D.Circ")
display.boxplot(df.filtered$Rad.Ra, "Rad.Ra")
display.boxplot(df.filtered$Pr.Axis.Ra, "Pr.Axis.Ra")
display.boxplot(df.filtered$Max.L.Ra, "Max.L.Ra")
display.boxplot(df.filtered$Scat.Ra, "Scat.Ra")
display.boxplot(df.filtered$Elong, "Elong")
display.boxplot(df.filtered$Pr.Axis.Rect, "Pr.Axis.Rect")
display.boxplot(df.filtered$Max.L.Rect, "Max.L.Rect")
display.boxplot(df.filtered$Sc.Var.Maxis, "Sc.Var.Maxis")
display.boxplot(df.filtered$Sc.Var.maxis, "Sc.Var.maxis")
display.boxplot(df.filtered$Ra.Gyr, "Ra.Gyr")
display.boxplot(df.filtered$Skew.Maxis, "Skew.Maxis")
display.boxplot(df.filtered$Skew.maxis, "Skew.maxis")
display.boxplot(df.filtered$Kurt.maxis, "Kurt.maxis")
display.boxplot(df.filtered$Kurt.Maxis, "Kurt.Maxis")
display.boxplot(df.filtered$Holl.Ra, "Holl.Ra")

# REMOVING THE OUTLIERS FROM THE DATASET
# Discarding the outliers from the data-set, 
# any value greater than bench.mark value will be replace with the bench mark value

remove.outliers = function(data, column.name){
  # filter with box plot and trimming out from 
  # "maximum": Q3 + 1.5*IQR
  # "minimum": Q1 -1.5*IQR 
  # where interquartile range (IQR): 25th to the 75th percentile.
  
  # Calculating the upper and lower limit for the data
  bench.mark.upper = quantile(data, 0.75) + (1.5 * IQR(data))
  bench.mark.lower = quantile(data, 0.25) - (1.5 * IQR(data))
  
  # Replacing the outliers with the upper and lower limits
  data[data > bench.mark.upper] = bench.mark.upper
  data[data < bench.mark.lower] = bench.mark.lower
  
  # Display the box-plot after removing the outlier
  display.boxplot(data, column.name)
  
}

# calling the remove.outliers function to remove all the outliers from each column of the data
remove.outliers(df.filtered$Comp, "Comp")
remove.outliers(df.filtered$Circ, "Circ")
remove.outliers(df.filtered$D.Circ, "D.Circ")
remove.outliers(df.filtered$Rad.Ra, "Rad.Ra")
remove.outliers(df.filtered$Pr.Axis.Ra, "Pr.Axis.Ra")
remove.outliers(df.filtered$Max.L.Ra, "Max.L.Ra")
remove.outliers(df.filtered$Scat.Ra, "Scat.Ra")
remove.outliers(df.filtered$Elong, "Elong")
remove.outliers(df.filtered$Pr.Axis.Rect, "Pr.Axis.Rect")
remove.outliers(df.filtered$Max.L.Rect, "Max.L.Rect")
remove.outliers(df.filtered$Sc.Var.Maxis, "Sc.Var.Maxis")
remove.outliers(df.filtered$Sc.Var.maxis, "Sc.Var.maxis")
remove.outliers(df.filtered$Ra.Gyr, "Ra.Gyr")
remove.outliers(df.filtered$Skew.Maxis, "Skew.Maxis")
remove.outliers(df.filtered$Skew.maxis, "Skew.maxis")
remove.outliers(df.filtered$Kurt.maxis, "Kurt.maxis")
remove.outliers(df.filtered$Kurt.Maxis, "Kurt.Maxis")
remove.outliers(df.filtered$Holl.Ra, "Holl.Ra")

# NORMALIZING THE DATASET (BRINGING ALL THE DATA INTO A SINGLE UNQIUE SCALE)
# Performing normalization using the Z-Score Standardization [STANDARD NORMALIZATION]
df.normalized = as.data.frame(scale(df.filtered))
View(df.normalized)

# THIS SECTION IS COMMENTED OUT BECAUSE THE CW SPECIFICATION MENTIONED THAT 
# ASSUME PCA IS NOT USED  FOR THIS PROBLEM (BY TAKING ALL THE INITAIL FEATURES 
# FOR PREDICTION).
#------------------------------BEGINING OF PERFORMING PCA-----------------------
# PERFORMING PCA (PRINCIPAL COMPONENT ANALYSIS) / DIMENSIONALITY REDUCTION
# df.pca = prcomp(df.normalized)
# summary(df.pca)
# 
# # Plotting the PCA data to find the best number of Principal Components.
# # Using the elbow method of the plot below we can get the number of components which
# # explain 85% or greater of the variation (BEST SET OF COMPONENTS TO TAKE)
# # In this case the first 4 components are the best, because it covers the greatest
# # area of the graph and has the sudden decrease after the 4th component
# plot(df.pca)
# plot(df.pca, type='l')
# 
# # comp.data contains the BEST PCA Component data extract
# comp.data = data.frame(df.pca$x[,1:4])
# View(comp.data)
#------------------------------END OF PERFORMING PCA----------------------------

# DETERMINE THE NUMBER OF CLUSTERS CENTERS (CENTROIDS) (via MANUAL and AUTOMATED TOOLS)

# AUTOMATED TOOLS TO FIND THE CENTROIDS

# Using NbClust()
# Using Euclidean for distance (Gave 2) using kmeans
cluster_euclidean = NbClust(df.normalized, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans",
                            index = "all")

# Using Manhattan for distance (Gave 2) using kmeans
cluster_manhattan = NbClust(df.normalized, distance = "manhattan", min.nc = 2, max.nc = 10, method = "kmeans",
                            index = "all")
# Using fviz_nbclust()
# USING ELBOW METHOD for kmeans (Gave 3)
# The below method points out that 3 is the optimal number of centroids/clusters to be taken
fviz_nbclust(df.normalized, kmeans, method = "wss") + 
  geom_vline(xintercept = 3, linetype = 2) + 
  labs(subtitle = "Elbow method")

# USING THE SILHOUETTE METHOD for kmeans (Gave 2)
# The below method points out that 2 is the optimal number of centroids/clusters to be taken
fviz_nbclust(df.normalized, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# USING GAP STATISTIC for kmeans ( nboot = 50 to keep the function speedy
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.)
# (Gave 3)
# The below method points out that 3 is the optimal number of centroids/clusters to be taken
fviz_nbclust(df.normalized, kmeans, nstart = 50,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")


# MANUALLY FIND THE CENTROIDS / CLUSTERS 

# USING ELBOW METHOD
tot.withinss = vector(mode = "character", length = 10)

# Classification Report Function
classification_report <- function(comparison_table, dp = 2) {
  #total counts
  counts <- sum(comparison_table)
  
  #total sums for each column
  column_sums <- colSums(comparison_table)
  
  #total sums for each row
  row_sums <- rowSums(comparison_table)
  
  #true positive value
  tp <- diag(comparison_table)
  
  #true negative
  tn <- counts - (column_sums + row_sums - tp)
  
  #false positive
  fp <- row_sums - tp
  
  #false negative
  fn <- column_sums - tp
  
  #precision
  pr <- tp / (tp + fp)
  
  #recall
  re <- tp / (tp + fn)
  
  #accuracy
  ac <- sum(tp) / counts
  
  # Displaying the Accuracy, Precision and Recall values
  print("---------------------")
  print("Accuracy")
  print(ac)
  
  print("---------------------")
  print("Precision")
  print(pr)
  
  print("---------------------")
  print("Recall")
  print(re)
  
}

# Manual Finding clusters
# Looping from 1 to the max optimal cluster to find its evaluation result
for (i in 1:10){
  cat("<=============== ", "Custer ", i, " ===============>\n", sep = "")
  set.seed(150)
  
  # Performing Kmeans clustering
  vehicleCluster = kmeans(df.normalized, centers = i, nstart = 20)
  
  set.seed(50)# This is the confusion matrix
  cm = as.matrix(table(Actual = df$Class, Predicted = vehicleCluster$cluster))
  print("Confusion Matrix")
  print(cm)
  
  # Display Classification report
  classification_report(comparison_table = cm)
  
  # Total within-cluster sum of squares
  tot.withinss[i] = vehicleCluster$tot.withinss
}

# plot to find the best number of clusters to be taken
plot(1:10, 
     tot.withinss,
     type="b",
     pch=19,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")

# from the accuracy result we can see that we got the highest accuracy result for
# 2 clusters (36%)
set.seed(150)
vehicleCluster = kmeans(df.normalized, centers = 2, nstart = 20)
fviz_cluster(vehicleCluster, data = df.normalized)

# Displaying the sizes(number of observations in each cluster) of each cluster 
vehicleCluster$size

# Displaying the cluster distribution
vehicleCluster$cluster

# Getting the centers (A matrix of cluster centers). 
vehicleCluster$centers 
View(vehicleCluster$centers)



# References used
# https://www.r-bloggers.com/2014/06/pca-and-k-means-clustering-of-delta-aircraft/
# https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/
# https://rpubs.com/Nitika/kmeans_Iris
# https://www.datanovia.com/en/lessons/k-means-clustering-in-r-algorith-and-practical-examples/
# https://uc-r.github.io/kmeans_clustering
# https://www.guru99.com/r-k-means-clustering.html
