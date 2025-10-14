# Load necessary libraries
library(ISLR)
library(glmnet)
library(caret)
library(ggplot2)
library(gridExtra)

# Load the College dataset
data(College)

# Convert categorical variables to numeric
College$Private <- as.numeric(College$Private)  # Convert factor to numeric

# Set seed for reproducibility
set.seed(123)

# Split data into training and testing sets (70% train, 30% test)
trainIndex <- createDataPartition(College$Grad.Rate, p = 0.7, list = FALSE)
trainData <- College[trainIndex, ]
testData <- College[-trainIndex, ]

# Prepare model matrices (excluding response variable)
x_train <- model.matrix(Grad.Rate ~ ., trainData)[, -1]  # Remove intercept
y_train <- trainData$Grad.Rate
x_test <- model.matrix(Grad.Rate ~ ., testData)[, -1]
y_test <- testData$Grad.Rate

# ---------------- RIDGE REGRESSION ----------------
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0)  # alpha = 0 for Ridge
lambda_min_ridge <- cv_ridge$lambda.min
lambda_1se_ridge <- cv_ridge$lambda.1se

# Plot Ridge cross-validation error
plot(cv_ridge, main = "Ridge Regression: Cross-Validation")

# Fit Ridge regression model using lambda.min
ridge_model <- glmnet(x_train, y_train, alpha = 0)

# Ridge Coefficient Paths
plot(ridge_model, xvar = "lambda", label = TRUE, main = "Ridge Regression Coefficient Paths")

# Get coefficients for Ridge at lambda.min
ridge_coeffs <- coef(glmnet(x_train, y_train, alpha = 0, lambda = lambda_min_ridge))

# Convert to a dataframe for visualization
ridge_coef_df <- data.frame(Feature = rownames(ridge_coeffs), Coefficient = as.vector(ridge_coeffs))[-1, ] # Remove intercept
ridge_coef_df <- ridge_coef_df[order(abs(ridge_coef_df$Coefficient), decreasing = TRUE), ]

# Ridge Coefficient Bar Chart

print(ridge_coef_df)
ridge_coef_df$Coefficient <- as.numeric(ridge_coef_df$Coefficient)
library(ggplot2)
ridge_coef_plot <- ggplot2::ggplot(ridge_coef_df, aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "pink", color = "black") +
  coord_flip() +
  ggtitle("Ridge Regression Coefficients") +
  theme_minimal()
print(ridge_coef_plot)


# Predict on training and test data


ridge_train_pred <- predict(glmnet(x_train, y_train, alpha = 0, lambda = lambda_min_ridge), newx = x_train)
ridge_test_pred <- predict(glmnet(x_train, y_train, alpha = 0, lambda = lambda_min_ridge), newx = x_test)

print(head(ridge_train_pred))
print(head(ridge_test_pred))

# Compute RMSE for Ridge
ridge_rmse_train <- sqrt(mean((y_train - ridge_train_pred)^2))
ridge_rmse_test <- sqrt(mean((y_test - ridge_test_pred)^2))

print(ridge_rmse_train)
print(ridge_rmse_test)
# ---------------- LASSO REGRESSION ----------------
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)  # alpha = 1 for LASSO
lambda_min_lasso <- cv_lasso$lambda.min
lambda_1se_lasso <- cv_lasso$lambda.1se

# Plot LASSO cross-validation error
plot(cv_lasso, main = "LASSO Regression: Cross-Validation")

# Fit LASSO regression model using lambda.min
lasso_model <- glmnet(x_train, y_train, alpha = 1)

# LASSO Coefficient Paths
plot(lasso_model, xvar = "lambda", label = TRUE, main = "LASSO Regression Coefficient Paths")


# Get coefficients for LASSO at lambda.min
lasso_coeffs <- coef(glmnet(x_train, y_train, alpha = 1, lambda = lambda_min_lasso))

# Convert to a dataframe for visualization
lasso_coef_df <- data.frame(Feature = rownames(lasso_coeffs), Coefficient = as.vector(lasso_coeffs))[-1, ] # Remove intercept
lasso_coef_df <- lasso_coef_df[order(abs(lasso_coef_df$Coefficient), decreasing = TRUE), ]

# LASSO Coefficient Bar Chart
lasso_coef_plot <- ggplot(lasso_coef_df, aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "red", color = "black") +
  coord_flip() +
  ggtitle("LASSO Regression Coefficients") +
  theme_minimal()
print(lasso_coef_plot)

# Predict on training and test data
lasso_train_pred <- predict(glmnet(x_train, y_train, alpha = 1, lambda = lambda_min_lasso), newx = x_train)
lasso_test_pred <- predict(glmnet(x_train, y_train, alpha = 1, lambda = lambda_min_lasso), newx = x_test)
print(lasso_train_pred)
print(lasso_test_pred)

# Compute RMSE for LASSO

lasso_rmse_train <- sqrt(mean((y_train - lasso_train_pred)^2))
lasso_rmse_test <- sqrt(mean((y_test - lasso_test_pred)^2))

print(lasso_rmse_train)
print(lasso_rmse_test)

# ---------------- Prediction Visualization ----------------
ridge_pred_plot <- ggplot(data.frame(Actual = y_test, Predicted = as.vector(ridge_test_pred)), aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggtitle("Ridge: Predicted vs. Actual") +
  theme_minimal()
print(ridge_pred_plot)

lasso_pred_plot <- ggplot(data.frame(Actual = y_test, Predicted = as.vector(lasso_test_pred)), aes(x = Actual, y = Predicted)) +
  geom_point(color = "red") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggtitle("LASSO: Predicted vs. Actual") +
  theme_minimal()
print(lasso_pred_plot)

# ---------------- Residual Plots ----------------
ridge_residuals <- data.frame(Residuals = y_test - as.vector(ridge_test_pred))
lasso_residuals <- data.frame(Residuals = y_test - as.vector(lasso_test_pred))

ridge_residual_plot <- ggplot(ridge_residuals, aes(x = Residuals)) +
  geom_histogram(binwidth = 5, fill = "lavender", color = "black") +
  ggtitle("Ridge Residuals Distribution") +
  theme_minimal()
print(ridge_residual_plot)

lasso_residual_plot <- ggplot(lasso_residuals, aes(x = Residuals)) +
  geom_histogram(binwidth = 5, fill = "cyan", color = "black") +
  ggtitle("LASSO Residuals Distribution") +
  theme_minimal()
print(lasso_residual_plot)

# ---------------- Display All Plots ----------------
grid.arrange(ridge_coef_plot, lasso_coef_plot, ncol = 2)
grid.arrange(ridge_pred_plot, lasso_pred_plot, ncol = 2)
grid.arrange(ridge_residual_plot, lasso_residual_plot, ncol = 2)

# ---------------- Print RMSE ----------------
print(paste("Ridge RMSE (Train):", ridge_rmse_train))
print(paste("Ridge RMSE (Test):", ridge_rmse_test))
print(paste("LASSO RMSE (Train):", lasso_rmse_train))
print(paste("LASSO RMSE (Test):", lasso_rmse_test))






