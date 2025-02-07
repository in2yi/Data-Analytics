library("ggplot2")
library("readr")

## read dataset
NY_House_Dataset <- read_csv("NY-House-Dataset.csv")

dataset <- NY_House_Dataset

## Data Cleaning
# Removing outliers in PRICE
dataset <- dataset[dataset$PRICE < 19500000,]

# Removing specific erroneous property size
dataset <- dataset[dataset$PROPERTYSQFT != 2184.207862,]

# Removing outliers in BEDS
dataset <- dataset[dataset$BEDS <= quantile(dataset$BEDS, 0.99),]

# Removing outliers in BATH
dataset <- dataset[dataset$BATH <= quantile(dataset$BATH, 0.99),]

# Removing rows with NA values
dataset <- na.omit(dataset)

## Fit three separate linear models
# Model 1: PRICE ~ PROPERTYSQFT
model1 <- lm(PRICE ~ PROPERTYSQFT, data = dataset)
summary(model1)

# Model 2: PRICE ~ BEDS
model2 <- lm(PRICE ~ BEDS, data = dataset)
summary(model2)

# Model 3: PRICE ~ BATH
model3 <- lm(PRICE ~ BATH, data = dataset)
summary(model3)

## Scatter plots with best fit line
# Model 1
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red") +
  ggtitle("Model 1: PRICE vs PROPERTYSQFT")

# Model 2
ggplot(dataset, aes(x = BEDS, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red") +
  ggtitle("Model 2: PRICE vs BEDS")

# Model 3
ggplot(dataset, aes(x = BATH, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red") +
  ggtitle("Model 3: PRICE vs BATH")

## Residual plots
# Model 1 Residuals
plot(model1$residuals, main="Residuals for Model 1", ylab="Residuals", xlab="Index")

# Model 2 Residuals
plot(model2$residuals, main="Residuals for Model 2", ylab="Residuals", xlab="Index")

# Model 3 Residuals
plot(model3$residuals, main="Residuals for Model 3", ylab="Residuals", xlab="Index")
