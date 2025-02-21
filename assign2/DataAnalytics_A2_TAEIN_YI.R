# 1.1: Load libraries and read the dataset.
if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

data <- read_csv("epi2024results06022024.csv")

# Define the two countries to compare.
country1 <- "Afghanistan"
country2 <- "Albania"

# Create subsets for the two countries.
data_country1 <- data %>% filter(country == country1)
data_country2 <- data %>% filter(country == country2)

# 1.1: Plot histograms for a variable of your choice for both countries with density lines overlayed.
# Here we use "ECO.new" as the chosen variable.
ggplot() +
  geom_histogram(data = data_country1, aes(x = `ECO.new`, y = ..density..),
                 binwidth = 2, fill = "lightblue", color = "black", alpha = 0.5) +
  geom_density(data = data_country1, aes(x = `ECO.new`), color = "blue", size = 1) +
  geom_histogram(data = data_country2, aes(x = `ECO.new`, y = ..density..),
                 binwidth = 2, fill = "lightgreen", color = "black", alpha = 0.5) +
  geom_density(data = data_country2, aes(x = `ECO.new`), color = "darkgreen", size = 1) +
  labs(title = "1.1: Histogram with Density of ECO.new for Afghanistan & Albania",
       x = "ECO.new", y = "Density") +
  theme_minimal()

# 1.2: Plot QQ plots for both variables compared to a normal distribution.
# For Afghanistan we use ECO.new; for Albania we use BDH.new.
# QQ Plot for ECO.new in Afghanistan.
ggplot(data_country1, aes(sample = `ECO.new`)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(title = "1.2: QQ Plot of ECO.new (Afghanistan) vs. Normal Distribution") +
  theme_minimal()

# QQ Plot for BDH.new in Albania.
ggplot(data_country2, aes(sample = `BDH.new`)) +
  stat_qq() +
  stat_qq_line(color = "darkgreen") +
  labs(title = "1.2: QQ Plot of BDH.new (Albania) vs. Normal Distribution") +
  theme_minimal()
