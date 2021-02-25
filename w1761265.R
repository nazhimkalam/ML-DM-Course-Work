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
df = read_excel("D:/IIT/2nd Year/Data Mining & Machine Leanring/Coursework/vehicles.xlsx")
View(df)

# Displaying the types of unique classes present in the data-set
unique(df[["Class"]])

# Removing the Sample index column and the class column from the data-set
df.filtered = subset(df, select = -c(Samples, Class))

# Viewing the head of the data-set
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
# Discarding the outliers from the data-set (only for the column which has outliers)
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
# Performing normalization using the Z-Score Standardization
df.normalized = as.data.frame(scale(df.filtered))
View(df.normalized)

# PERFORMING PCA (PRINCIPAL COMPONENT ANALYSIS) / DIMENSIONALITY REDUCTION
df.pca = prcomp(df.normalized)
summary(df.pca)















