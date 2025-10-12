## MODULE :03 

# STEP 1: Install and Load ISLR Package

install.packages("ISLR")
library(ISLR)


# STEP 2: Load the dataset

data("College")


# STEP 3: Verifying the dataset

head(College) #View frist few rows of dataset
str(College) #Check Structure of dataset 
summary(College) #To get summary of dataset( Descriptive Statistic)
sum(is.na(College)) #Check for missing values


#STEP 4: EDA --> Descriptive Statistics and Plots

#Descriptive Statistics for numeric variables 
summary(College)

#Histogram for numeric variables 
hist(College$Apps, main = "Histogram of Applications", xlab = "Applications", col = "pink")

#Boxplot for Out-of-State Tuition by Private Status
boxplot(Outstate ~ Private, data = College, main = " Out-of-State Tuition by Private Status", xlab = "Private", ylab = "Out-of-State Tuition", col = "peachpuff")

#Scatterplot: Room & Board vs Graduation Rate
plot(College$Room.Board, College$Grad.Rate, main = "Room & Board vs Graduation Rate", xlab = "Room & Board", ylab = "Graduation Rate", col = "orange")




##STEP 5: Split the Data into Train and Test Sets

set.seed(123) # Set seeds for reproducibility

#Split the data
sample_size <- floor(0.7 * nrow(College))
train_indices <- sample(seq_len(nrow(College)), size = sample_size)

train_data <- College[train_indices, ]
test_data <- College[-train_indices,]


##STEP 6: Fit a multivariate Logistic Regression Model
# NOTE: We will use glm() function to fit a logistic regression model on the training set. Outstate and Grad.Rate as predictors.

logistic_model <- glm(Private ~ Outstate + Grad.Rate, data = train_data, family = binomial)
summary(logistic_model) #To summarize the model


##STEP 6 : Create a confusion Matrix for the Train set

#Predict probabilites on the train set
train_data$predicted_prob <- predict(logistic_model, type = "response")

#Convert probabilities to binary predictions (0 = Public, 1 = Private)
train_data$predicted_class <- ifelse(train_data$predicted_prob > 0.5, "Yes", "No")

#Create confusion matrix
confusion_matrix_train <- table(Actual = train_data$Private, Predicted = train_data$predicted_class)
print(confusion_matrix_train)


##STEP 7: Extract values from confusion matrix

TP <- confusion_matrix_train["Yes", "Yes"]
TN <- confusion_matrix_train["No","No"]
FP <-confusion_matrix_train["No","Yes"]
FN <- confusion_matrix_train["Yes","No"]

# Calculate metrices 
accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
specifically <- TN / (TN + FP)

#print metrices
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("Specifically:", specifically, "\n")


##STEP 8: Evaluate the Model on the Test Set

#Predict probabilities on the test set
test_data$predicted_prob <- predict(logistic_model, newdata = test_data, type = "response")

# Convert probabilities to binary predictions
test_data$predicted_class <- ifelse(test_data$predicted_prob > 0.5, "Yes", "No")

# Create confusion matrix
Confusion_matrix_test <- table(Actual = test_data$Private, Predicted = test_data$predicted_class)
print(Confusion_matrix_test)


##STEP 9: Plot and Interpret the ROC Curve

install.packages # install and load the pROC package
library(pROC)

#Create ROC curve
roc_curve <- roc(test_data$Private, test_data$predicted_prob, 
                 levels = c("No", "Yes"),  # Define levels
                 direction = "<")          # Define direction
plot(roc_curve, main = "ROC Curve", col = "brown")


##STEP 10: Calculate and Interpret AUC(Area under the curve)

auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")


##STEP 11:

# Install and Load Required Packages
install.packages("pROC") 
library(pROC)

# Create ROC Curve for Training Set
roc_curve_train <- roc(train_data$Private, train_data$predicted_prob, 
                       levels = c("No", "Yes"),  
                       direction = "<")          
plot(roc_curve_train, main = "ROC Curve - Training/Testing Set", col = "blue")
print(roc_curve_train)

# Create ROC Curve for Test Set
roc_curve_test <- roc(test_data$Private, test_data$predicted_prob, 
                      levels = c("No", "Yes"),  
                      direction = "<")          
plot(roc_curve_test, add = TRUE, col = "red")
print(roc_curve_test)

# Add legend to differentiate training and test ROC curves
legend("bottomright", legend = c("Training Set", "Test Set"), col = c("blue", "red"), lwd = 2)

# Calculate and Compare AUC values
auc_train <- auc(roc_curve_train)
auc_test <- auc(roc_curve_test)

# Print AUC values
cat("AUC for Training Set:", auc_train, "\n")
cat("AUC for Test Set:", auc_test, "\n")
