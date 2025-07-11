#Mohammed Saif Wasay
#NUID: 002815958
#ALY6015 Intermediate Analytics
#House Price Prediction Analysis

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notation for entire R session

# Load necessary libraries
library(ggplot2)
library(GGally)
library(dplyr)
library(caret)
library(glmnet)
library(reshape2)

# Load dataset
data <- read.csv("C:/Users/Mohammed Saif Wasay/Documents/code/data/Housing.csv")

# Display the first few rows
head(data)

#EDA
# Check for missing values
missing_values <- colSums(is.na(data))
print(missing_values)

numerical_cols <- c("Area", "Bedrooms", "Bathrooms", "Age", "Price")
# Create the histogram with a density curve for Age
ggplot(data, aes(x = Age)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "orange", color = "black", alpha = 0.7) +
  geom_density(color = "orange", size = 1) +
  labs(title = "Distribution of Age", x = "Age", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create the boxplot for Age
ggplot(data, aes(y = Age)) +
  geom_boxplot(fill = "pink", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of Age", x = "", y = "Age") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create histogram with density curve for Price
ggplot(data, aes(x = Price)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "orange", color = "black", alpha = 0.7) +
  geom_density(color = "orange", size = 1) +
  labs(title = "Distribution of Price", x = "Price", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create the boxplot for Price
ggplot(data, aes(y = Price)) +
  geom_boxplot(fill = "pink", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of Price", x = "", y = "Price") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create the distribution plot for Bedrooms
ggplot(data, aes(x = Bedrooms)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "orange", color = "black", alpha = 0.7) +
  geom_density(color = "orange", size = 1) +
  labs(title = "Distribution of Bedrooms", x = "Bedrooms", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create the boxplot for Bedrooms
ggplot(data, aes(y = Bedrooms)) +
  geom_boxplot(fill = "pink", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of Bedrooms", x = "", y = "Bedrooms") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create histogram with density curve for Area
ggplot(data, aes(x = Area)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "orange", color = "black", alpha = 0.7) +
  geom_density(color = "orange", size = 1) +
  labs(title = "Distribution of Area", x = "Area", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create the boxplot for Area
ggplot(data, aes(y = Area)) +
  geom_boxplot(fill = "pink", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of Area", x = "", y = "Area") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot distribution for Bathrooms
ggplot(data, aes(x = Bathrooms)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "orange", color = "black", alpha = 0.7) +
  geom_density(color = "orange", size = 1) +
  labs(title = "Distribution of Bathrooms", x = "Bathrooms", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create the boxplot for Bathrooms
ggplot(data, aes(y = Bathrooms)) +
  geom_boxplot(fill = "pink", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of Bathrooms", x = "", y = "Bathrooms") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Correlation Matrix 
library(reshape2)
correlation_matrix <- cor(data[sapply(data, is.numeric)])
correlation_melt <- melt(correlation_matrix)

ggplot(correlation_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pairplot using GGally
ggpairs(data, columns = numerical_cols, 
        aes(color = Location, alpha = 0.7)) +
  theme_minimal()

# One-hot encode the categorical variable
data <- data %>%
  mutate(Location_Suburban = ifelse(Location == "Suburban", 1, 0),
         Location_Urban = ifelse(Location == "Urban", 1, 0)) %>%
  select(-Location)

# Split data into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(data$Price, p = 0.8, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

# Fit linear regression
linear_model <- lm(Price ~ ., data = train)

# Predict on test data
pred_linear <- predict(linear_model, test)
print(pred_linear)

# Evaluate model
mse_linear <- mean((test$Price - pred_linear)^2)
r2_linear <- 1 - (sum((test$Price - pred_linear)^2) / sum((test$Price - mean(test$Price))^2))

print(paste("Linear Regression MSE:", mse_linear))
print(paste("Linear Regression R2:", r2_linear))

# Prepare data for glmnet
x_train <- as.matrix(train %>% select(-Price))
y_train <- train$Price
x_test <- as.matrix(test %>% select(-Price))
y_test <- test$Price

# Fit Lasso regression
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1)

# Optimal alpha (lambda)
best_lambda_lasso <- lasso_model$lambda.min

# Predict on test data
pred_lasso <- predict(lasso_model, s = best_lambda_lasso, newx = x_test)

# Evaluate model
mse_lasso <- mean((y_test - pred_lasso)^2)
r2_lasso <- 1 - (sum((y_test - pred_lasso)^2) / sum((y_test - mean(y_test))^2))

print(paste("Lasso Regression MSE:", mse_lasso))
print(paste("Lasso Regression R2:", r2_lasso))

# Fit Ridge regression
ridge_model <- cv.glmnet(x_train, y_train, alpha = 0)

# Optimal alpha (lambda)
best_lambda_ridge <- ridge_model$lambda.min

# Predict on test data
pred_ridge <- predict(ridge_model, s = best_lambda_ridge, newx = x_test)

# Evaluate model
mse_ridge <- mean((y_test - pred_ridge)^2)
r2_ridge <- 1 - (sum((y_test - pred_ridge)^2) / sum((y_test - mean(y_test))^2))

print(paste("Ridge Regression MSE:", mse_ridge))
print(paste("Ridge Regression R2:", r2_ridge))

# Lasso Coefficient Path
lasso_fit <- glmnet(x_train, y_train, alpha = 1)
plot(lasso_fit, xvar = "lambda", label = TRUE, main = "Lasso Coefficient Path")

# Ridge Coefficient Path
ridge_fit <- glmnet(x_train, y_train, alpha = 0)
plot(ridge_fit, xvar = "lambda", label = TRUE, main = "Ridge Coefficient Path")

# Fit Linear Regression
linear_model <- lm(Price ~ ., data = train)

# Predict on test data
pred_linear <- predict(linear_model, test)

# Ensure y_test and pred_linear match in dimensions
print(length(test$Price))
print(length(pred_linear))

# Visualization of Predicted vs Actual Prices for Linear Regression
ggplot(data.frame(Actual = test$Price, Predicted = pred_linear), aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Linear Regression: Predicted vs Actual", x = "Actual Prices", y = "Predicted Prices") +
  theme_minimal()

# Fit Lasso regression
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1)

# Optimal alpha (lambda)
best_lambda_lasso <- lasso_model$lambda.min

# Predict on test data
pred_lasso <- as.vector(predict(lasso_model, s = best_lambda_lasso, newx = x_test))

# Ensure y_test and pred_lasso match in dimensions
print(length(y_test))
print(length(pred_lasso))

# Visualization of Predicted vs Actual Prices
ggplot(data.frame(Actual = y_test, Predicted = pred_lasso), aes(x = Actual, y = Predicted)) +
  geom_point(color = "green", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Lasso Regression: Predicted vs Actual", x = "Actual Prices", y = "Predicted Prices") +
  theme_minimal()

# Fit Ridge regression
ridge_model <- cv.glmnet(x_train, y_train, alpha = 0)

# Optimal lambda for Ridge
best_lambda_ridge <- ridge_model$lambda.min

# Predict on test data
pred_ridge <- as.vector(predict(ridge_model, s = best_lambda_ridge, newx = x_test))

# Ensure y_test and pred_ridge match in dimensions
print(length(y_test))
print(length(pred_ridge))

# Visualization of Predicted vs Actual Prices for Ridge Regression
ggplot(data.frame(Actual = y_test, Predicted = pred_ridge), aes(x = Actual, y = Predicted)) +
  geom_point(color = "purple", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Ridge Regression: Predicted vs Actual", x = "Actual Prices", y = "Predicted Prices") +
  theme_minimal()
