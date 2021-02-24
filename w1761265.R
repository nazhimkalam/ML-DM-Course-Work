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

# [PRE-PROCESSING DATA] PERFORMING SCALING AND OUTLIERS REMOVAL

# Checking for any null values present in the data-set (returned 0 so no null values present)
print(sum(is.na(df)))

# Checking the Summary of the data-set (We can see the stats of the data columns eg: mean, median etc.)
summary(df)

# Plotting a box plot graph (Box plots are useful to detect potential outliers from the data-set)
boxplot(df$Comp, ylab = "Comp", main = "Box plot for Compactness")
boxplot(df$Circ, ylab = "Circ", main = "Box plot for Circularity")
boxplot(df$D.Circ, ylab = "D.Circ", main = "Box plot for Distance Circularity")
boxplot(df$Rad.Ra, ylab = "Rad.Ra", main = "Box plot for Radius ratio")
boxplot(df$Pr.Axis.Ra, ylab = "Pr.Axis.Ra", main = "Box plot for pr.axis aspect ratio")
boxplot(df$Max.L.Ra, ylab = "Max.L.Ra", main = "Box plot for max.length aspect ratio")
boxplot(df$Scat.Ra, ylab = "Scat.Ra", main = "Box plot for scatter ratio")
boxplot(df$Elong, ylab = "Elong", main = "Box plot for elongatedness")
boxplot(df$Pr.Axis.Rect, ylab = "Pr.Axis.Rect", main = "Box plot for pr.axis rectangularity")
boxplot(df$Max.L.Rect, ylab = "Max.L.Rect", main = "Box plot for max.length rectangularity")
boxplot(df$Sc.Var.Maxis, ylab = "Sc.Var.Maxis", main = "Box plot for scaled variance along major axis")
boxplot(df$Sc.Var.maxis, ylab = "Sc.Var.maxis", main = "Box plot for scaled variance along minor axis")
boxplot(df$Ra.Gyr, ylab = "Ra.Gyr", main = "Box plot for scaled radius of gyration")
boxplot(df$Skew.Maxis, ylab = "Ra.Gyr", main = "Box plot for skewness about major axis")
boxplot(df$Skew.maxis, ylab = "Skew.maxis", main = "Box plot for skewness about minor axis")
boxplot(df$Kurt.maxis, ylab = "Kurt.maxis", main = "Box plot for kurtosis about minor axis")
boxplot(df$Kurt.Maxis, ylab = "Kurt.Maxis", main = "Box plot for kurtosis about major axis")
boxplot(df$Holl.Ra, ylab = "Holl.Ra", main = "Box plot for hollows ratio")

# https://www.youtube.com/watch?v=6hRKlZ8D_mk



























