library(class)   
library(caret)      
library(ggplot2)   

# Read dataset
abalone <- read.csv("abalone_dataset.csv")

# Create age group using rings
abalone$age.group <- cut(abalone$rings, breaks = c(0, 8, 11, 35),
                         labels = c("young", "adult", "old"))
# Alternatively, you can use:
abalone$age.group[abalone$rings <= 8] <- "young"
abalone$age.group[abalone$rings > 8 & abalone$rings <= 11] <- "adult"
abalone$age.group[abalone$rings > 11] <- "old"   # Note: upper bound set to all rings > 11

# Ensure age.group is a factor
abalone$age.group <- as.factor(abalone$age.group)


set.seed(123)  # for reproducibility
trainIndex <- sample(seq_len(nrow(abalone)), size = 0.7 * nrow(abalone))
train <- abalone[trainIndex, ]
test  <- abalone[-trainIndex, ]


# Model 1: Using size measurements (length, diameter, height)
features1 <- c("length", "diameter", "height")
train1 <- train[, features1]
test1  <- test[, features1]

# Model 2: Using weight measurements (whole_weight, shucked_wieght, viscera_wieght, shell_weight)
features2 <- c("whole_weight", "shucked_wieght", "viscera_wieght", "shell_weight")
train2 <- train[, features2]
test2  <- test[, features2]

# Choose an initial k (e.g., 5)
initial_k <- 5

# Train kNN for Model 1
pred_knn1 <- knn(train = train1, test = test1, cl = train$age.group, k = initial_k)
cm1 <- table(Predicted = pred_knn1, Actual = test$age.group)
print("Contingency Table for Model 1 (Size features):")
print(cm1)

# Train kNN for Model 2
pred_knn2 <- knn(train = train2, test = test2, cl = train$age.group, k = initial_k)
cm2 <- table(Predicted = pred_knn2, Actual = test$age.group)
print("Contingency Table for Model 2 (Weight features):")
print(cm2)

# Calculate accuracies for both models
acc1 <- sum(diag(cm1)) / sum(cm1)
acc2 <- sum(diag(cm2)) / sum(cm2)
cat("Accuracy Model 1:", round(acc1,3), "\n")
cat("Accuracy Model 2:", round(acc2,3), "\n")

# Assume the better performing model is the one with higher accuracy
if(acc1 >= acc2) {
  best_features <- features1
  cat("Using feature subset: Size features\n")
} else {
  best_features <- features2
  cat("Using feature subset: Weight features\n")
}

# Prepare training and testing data for the optimal subset
train_opt <- train[, best_features]
test_opt  <- test[, best_features]

# Evaluate k values over a range (e.g., from 1 to 20)
k_values <- 1:20
accuracies <- numeric(length(k_values))

for(i in seq_along(k_values)) {
  k_val <- k_values[i]
  pred <- knn(train = train_opt, test = test_opt, cl = train$age.group, k = k_val)
  cm   <- table(pred, test$age.group)
  accuracies[i] <- sum(diag(cm)) / sum(cm)
}

# Identify the optimal k
optimal_k <- k_values[which.max(accuracies)]
cat("Optimal k:", optimal_k, "with accuracy:", round(max(accuracies), 3), "\n")

# Optionally, plot accuracy vs k
plot(k_values, accuracies, type = "b", pch = 19, col = "blue",
     xlab = "k value", ylab = "Accuracy",
     main = "Accuracy vs. k for kNN Model")

cluster_data <- train_opt  # we use the training set for clustering
if(ncol(cluster_data) > 2) {
  plot_features <- best_features[1:2]  # choose the first two features for 2D plot
} else {
  plot_features <- best_features
}

# Determine the optimal number of clusters using the elbow method
wss <- numeric(10)
for (k in 1:10) {
  set.seed(123)
  km <- kmeans(cluster_data, centers = k, nstart = 20)
  wss[k] <- km$tot.withinss
}

# Plot the elbow curve
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters (K)",
     ylab = "Total within-clusters sum of squares",
     main = "Elbow Method for Optimal K")

# From the elbow plot, suppose the optimal number of clusters is chosen as, e.g., 3.
optimal_K <- 3

# Run final k-means with optimal_K clusters
set.seed(123)
final_km <- kmeans(cluster_data, centers = optimal_K, nstart = 20)

# Append cluster assignments to training data
train$cluster <- as.factor(final_km$cluster)

# Plot clusters for the two selected features
ggplot(train, aes_string(x = plot_features[1], y = plot_features[2], color = "cluster")) +
  geom_point(size = 2) +
  labs(title = paste("k-Means Clustering (K =", optimal_K, ")"),
       x = plot_features[1],
       y = plot_features[2]) +
  theme_minimal()
