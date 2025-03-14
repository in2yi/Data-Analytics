# Load necessary libraries
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(class)) install.packages("class")
if (!require(caret)) install.packages("caret")
library(ggplot2)
library(class)
library(caret)

# Load the wine dataset (assuming it's the UCI wine dataset)
wine <- read.csv("lab4/wine.data", header = FALSE)
colnames(wine) <- c("Type", "Alcohol", "Malic_Acid", "Ash", "Alcalinity", 
                    "Magnesium", "Phenols", "Flavanoids", "Nonflavanoid_Phenols", 
                    "Proanthocyanins", "Color_Intensity", "Hue", "OD280_OD315", "Proline")
