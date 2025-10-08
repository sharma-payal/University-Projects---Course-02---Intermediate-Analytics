# Load necessary libraries
install.packages("caret")
install.packages("glmnet")
install.packages("gridExtra")
library(tidyverse)
library(ggplot2)
library(caret)
library(dplyr)
library(corrplot)
library(glmnet)
library(gridExtra)
library(randomForest)

# Step 1: Load Data
# Set file path and load the Ames Housing dataset
data <- read.csv("C:/Users/ravin/Downloads/AmesHousing.csv")

# Step 2: Initial Exploration
# View the structure of the dataset
str(data)

# Check for missing values
missing_values <- colSums(is.na(data))
missing_values[missing_values > 0]

# Summary statistics for continuous variables
summary(data)


# Step 3: Data Cleaning
# Handle missing values by imputing or removing
# Example: Impute Lot Frontage with median value
if("Lot.Frontage" %in% colnames(data)) {
  data$Lot.Frontage[is.na(data$Lot.Frontage)] <- median(data$Lot.Frontage, na.rm = TRUE)
}


# Remove houses with more than 4000 sq ft (as per documentation recommendation)
data <- data[data$Gr.Liv.Area <= 4000, ]


# Step 4: Exploratory Data Analysis
# Distribution of the target variable (SalePrice)
ggplot(data, aes(x = SalePrice)) +
  geom_histogram(binwidth = 10000, fill = "pink", color = "black") +
  labs(title = "Distribution of Sale Prices", x = "Sale Price", y = "Count")

# Correlation heatmap of numerical variables
# Select numerical variables
num_vars <- data %>% select_if(is.numeric)

# Calculate the correlation matrix
corr_matrix <- cor(num_vars, use = "complete.obs")

# Plot the correlation heatmap
corrplot(corr_matrix, method = "color", tl.cex = 0.6)

# Scatter plot for Gr Liv Area vs SalePrice
ggplot(data, aes(x = Gr.Liv.Area, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",  fill = "black", color = "orange") +
  labs(title = "Gr Liv Area vs Sale Price", x = "Ground Living Area", y = "Sale Price")

# Step 5: Feature Engineering
# Convert categorical variables to factors
data <- data %>% mutate(across(where(is.character), as.factor))

# Encode ordinal variables if needed (example below for OverallQual)
data$Overall.Qual <- as.numeric(data$Overall.Qual)


# Step 6: Linear Regression Analysis
# Split data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(data$SalePrice, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]


# Build a simple linear regression model
lm_model <- lm(SalePrice ~ Gr.Liv.Area + Overall.Qual + Garage.Cars, data = trainData)
summary(lm_model)


# Diagnostic Plots for Linear Model
par(mfrow = c(2, 2))
plot(lm_model)


# Explaination on the two graphs.
# Residuals vs Fitted plot to check for homoscedasticity
ggplot(lm_model, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "orange") +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")

# Q-Q plot for normality of residuals
ggplot(lm_model, aes(sample = .resid)) +
  geom_qq() +
  geom_qq_line(color = "brown") +
  labs(title = "Q-Q Plot of Residuals")


# Pairplot equivalent (scatterplots with histograms)
top_features <- c("SalePrice", "Overall.Qual", "Gr.Liv.Area", "Garage.Cars", "Garage.Area", "Total.Bsmt.SF")
pairs(df_cleaned[, top_features], main = "Scatterplot Matrix of Top Features")



# Load additional libraries
library(randomForest)


# Train the random forest model
set.seed(123)
rf_model <- randomForest(SalePrice ~ ., data = trainData, importance = TRUE, ntree = 500)
print(rf_model)
importance(rf_model)
varImpPlot(rf_model)


# Model Evaluation
# Predict on the test set with Random Forest
#rf_predictions <- predict(rf_model, newdata = testData)
#rf_rmse <- sqrt(mean((rf_predictions - testData$SalePrice)^2))
#cat("RMSE for Random Forest Regression:", rf_rmse, "\n")



# Linear Regression Model
lm_model <- lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + Garage.Cars + Garage.Area + Total.Bsmt.SF, data = trainData)
lm_pred <- predict(lm_model, testData)
lm_rmse <- sqrt(mean((lm_pred - testData$SalePrice)^2))

# Install the rpart package if you haven't already
install.packages("rpart")

# Load the rpart package
library(rpart)

# Decision Tree Model
tree_model <- rpart(SalePrice ~ Overall.Qual + Gr.Liv.Area + Garage.Cars + Garage.Area + Total.Bsmt.SF, data = trainData, method = "anova")
tree_pred <- predict(tree_model, testData)
tree_rmse <- sqrt(mean((tree_pred - testData$SalePrice)^2))

# Install the randomForest package if you haven't already
install.packages("randomForest")

# Load the randomForest package
#library(randomForest)

# Random Forest Model
#rf_model <- randomForest(SalePrice ~ Overall.Qual + Gr.Liv.Area + Garage.Cars + Garage.Area + Total.Bsmt.SF, data = trainData, ntree = 500)
#rf_pred <- predict(rf_model, testData)
#rf_rmse <- sqrt(mean((rf_pred - testData$SalePrice)^2))



# Build a simple linear regression model (only for demonstration of best-fit equation)
simple_lm_model <- lm(SalePrice ~ Gr.Liv.Area, data = trainData)

# Extract coefficients
intercept <- coef(simple_lm_model)[1]
slope <- coef(simple_lm_model)[2]

# Construct the best-fit line equation
best_fit_equation <- paste("SalePrice =", round(intercept, 2), "+", round(slope, 2), "* Gr.Liv.Area")
cat("Best-fit line equation: ", best_fit_equation, "\n")

# Plot the data and the best-fit line
ggplot(trainData, aes(x = Gr.Liv.Area, y = SalePrice)) +
  geom_point(alpha = 0.5, color = "lightsteelblue") +
  geom_smooth(method = "lm", formula = y ~ x, color = "pink") +
  labs(title = "Best Fit Line for SalePrice vs Gr.Liv.Area",
       subtitle = best_fit_equation,
       x = "Ground Living Area",
       y = "Sale Price") +
  theme_minimal()

print(model_comparison)

# Visualize the comparison
ggplot(model_comparison, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Model Comparison: RMSE",
       x = "Model",
       y = "Root Mean Squared Error (RMSE)")

