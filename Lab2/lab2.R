library(ggplot2)

# Load data
housing_data <- read.csv("NY-House-Dataset.csv")

# Data cleaning: Remove outliers (top 1% and bottom 1% for price and property size)
outlier_thresholds <- function(x) {
  q <- quantile(x, probs = c(0.1, 0.90), na.rm = TRUE)
  x[x < q[1] | x > q[2]] <- NA
  return(x)
}

housing_data$PRICE <- outlier_thresholds(housing_data$PRICE)
housing_data$PROPERTYSQFT <- outlier_thresholds(housing_data$PROPERTYSQFT)

# Remove rows with NA values
housing_data <- na.omit(housing_data)

# Fit three linear models
model1 <- lm(PRICE ~ PROPERTYSQFT, data = housing_data)
model2 <- lm(PRICE ~ PROPERTYSQFT + BEDS, data = housing_data)
model3 <- lm(PRICE ~ PROPERTYSQFT + BEDS + BATH, data = housing_data)

# Print model summaries and models
print(model1)
print(model2)
print(model3)
summary(model1)
summary(model2)
summary(model3)

# Function to generate plots
plot_model <- function(model, x_var, data, title) {
  ggplot(data, aes_string(x = x_var, y = "PRICE")) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, col = "red") +
    ggtitle(title)
}

# Scatter plot of residuals
plot_residuals <- function(model, title) {
  residuals <- resid(model)
  fitted_values <- fitted(model)
  ggplot(data.frame(Fitted = fitted_values, Residuals = residuals),
         aes(x = Fitted, y = Residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, col = "red") +
    ggtitle(title)
}

# Generate line graphs with all data points for each model
plot_model(model1, "PROPERTYSQFT", housing_data, "Model 1: Price vs PropertySqFt")
plot_model(model2, "BEDS", housing_data, "Model 2: Price vs Beds")
plot_model(model3, "BATH", housing_data, "Model 3: Price vs Bath")

# Generate residual plots
plot_residuals(model1, "Model 1 Residuals")
plot_residuals(model2, "Model 2 Residuals")
plot_residuals(model3, "Model 3 Residuals")
