# Installing necessary libraries
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("pROC")
#install.packages("ggplot2")
#install.packages("caret")
#install.packages("corrplot")
#install.packages("gridExtra")
#install.packages("caTools")

# Load required libraries
library(tidyr)
library(dplyr)
library(pROC)
library(ggplot2)
library(caret)
library(corrplot)
library(gridExtra)
library(caTools)

# Load the dataset
cardiac_data <- read.csv("cardiac.csv")
head(cardiac_data)

# Exploratory Data Analysis (EDA)
# Descriptive Statistics
summary(cardiac_data)
str(cardiac_data)

# Checking the class of each variable for level of Measurement 
class_variable <- sapply(cardiac_data, class)
print(class_variable)

get_measurement_level <- function(variable) {
  if (is.numeric(variable) & !is.integer(variable)) "Interval"
  else if (is.numeric(variable) & is.integer(variable)) "Ratio"
  else if (is.factor(variable) | is.character(variable)) {
    if (length(unique(variable)) == length(variable)) "Nominal"
    else "Ordinal"
  } else "Not determined"
}
# checking for the level of measurement 
measurement_levels <- sapply(cardiac_data, get_measurement_level)
print(data.frame(Variable = names(cardiac_data), Level_of_Measurement = measurement_levels))

# Checking for missing values
any(is.na(cardiac_data))
# Display missing value counts for each columns
colSums(is.na(cardiac_data))


# Visualize the data

## Example: Boxplot of Age by Cardiac Condition
ggplot(cardiac_data, aes(x = cardiac_condition, y = age, fill = cardiac_condition)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age by Cardiac Condition")

# Boxplot for each numerical variable
boxplot(cardiac_data[, c("age", "weight", "fitness_score")])
# Add title and labels
title("Boxplots of Numerical Columns")


# Extracting numerical features excluding 'caseno'
numerical_features <- cardiac_data[sapply(cardiac_data, is.numeric) & names(cardiac_data) != "caseno"]
# Plotting histograms for all numerical variables
par(mfrow = c(3, 1))  
for (col in colnames(numerical_features)) {
  hist(numerical_features[[col]], main = col, xlab = col, col = "skyblue", border = "black")
}
par(mfrow = c(1, 1))  


# Create a histogram for age variable with different colors for cardiac_condition
pl <- ggplot(cardiac_data, aes(x = age, fill = cardiac_condition)) +
  geom_histogram(binwidth = 5, position = "dodge", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Ages by Cardiac Condition",
       x = "Age",
       y = "Count") +
  theme_minimal()
print(pl)

# Creating a pie chart for cardiac_condition
pie_chart <- ggplot(cardiac_data, aes(x = "", fill = cardiac_condition)) +
  geom_bar(stat = "count", width = 1) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat = "count", position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme_void()
print(pie_chart)


#Data Preprocessing 
#Applying One-Hot Encoding to the gender variable 
# Convert 'gender' to a factor variable
cardiac_data$gender <- as.factor(cardiac_data$gender)
encoded_gender <- model.matrix(~ gender - 1, data = cardiac_data)
# Add the encoded columns to the dataset
cardiac_data <- cbind(cardiac_data, encoded_gender)
# Remove the original 'gender' column
cardiac_data <- cardiac_data[, -which(names(cardiac_data) %in% c("gender"))]
str(cardiac_data)

#Removing the the caseno from the dataset
cardiac_data <- select(cardiac_data,-caseno)
head(cardiac_data)


#Applying Logistic Regression Model- Initial Model

# Set the random seed based on your student number
set.seed(22182918)
# Convert "Present" to 1 and "Absent" to 0
cardiac_data$cardiac_condition_binary <- ifelse(cardiac_data$cardiac_condition == "Present", 1, 0)
# Remove 'cardiac_condition' variables
cardiac_data <- subset(cardiac_data, select = -c(cardiac_condition))
#splitting the data into test and train subset
sample <- sample.split(cardiac_data$cardiac_condition_binary, SplitRatio = 0.70) 
train <- subset(cardiac_data, sample == TRUE)
test <- subset(cardiac_data, sample == FALSE)
# Train the logistic regression model
model <- glm(cardiac_condition_binary ~ ., family = binomial(logit), data = train)
summary(model)

# Make predictions on the test data
test$predicted.cardiac <- predict(model, newdata = test, type = "response")
# Converting to binary predictions
test$predicted.cardiac_binary <- ifelse(test$predicted.cardiac > 0.5, 1, 0)
test$predicted.cardiac_binary <- factor(test$predicted.cardiac_binary, levels = c("0", "1"))
test$cardiac_condition_binary<- factor(test$cardiac_condition_binary, levels = c("0", "1"))

# Evaluate the logistic regression model
conf_matrix_lr <- confusionMatrix(test$predicted.cardiac_binary, test$cardiac_condition_binary)
print("Confusion Matrix (Logistic Regression):")
print(conf_matrix_lr)

conf_matrix <- confusionMatrix(test$predicted.cardiac_binary, test$cardiac_condition_binary)
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
accuracy <- conf_matrix$overall["Accuracy"]
f1 <- conf_matrix$byClass["F1"]

print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("Accuracy:", accuracy))
print(paste("F1 Score:", f1))

# Creating ROC curve for Logistic Regression 
roc_curve <- roc(as.numeric(test$cardiac_condition_binary) - 1, test$predicted.cardiac)
cat("AUC:", round(roc_curve$auc, 4), "\n")
# Plotting ROC curve for Logistics Regression 
plot(roc_curve, main = "ROC Curve - Logistic Regression", col = "blue", lwd = 2)



#Removing outliers 
# Calculate median and IQR
median_age <- median(cardiac_data$age)
iqr_age <- IQR(cardiac_data$age)
# Identify outliers using median and IQR
outliers <- which(cardiac_data$age > median_age + 1.5 * iqr_age | cardiac_data$age < median_age - 1.5 * iqr_age)
# Removing outliers
cardiac_data_no_outliers <- cardiac_data[-outliers, ]
#  After removing outliers Plot boxplot
boxplot(cardiac_data_no_outliers$age, main = "Boxplot of Age (No Outliers)", col = "skyblue", border = "black")

#Applying the normalization on the data
# Select the numeric columns to normalize (excluding the target variable)
numeric_columns <- c("age", "weight", "fitness_score")
cardiac_data <- cardiac_data_no_outliers
cardiac_data[numeric_columns] <- scale(cardiac_data_no_outliers[numeric_columns])
head(cardiac_data)


#Applying Logistic Regression Model- Intermediate Model
# Split the normalized data into training and testing sets
spl_normalized <- sample.split(cardiac_data$cardiac_condition_binary, 0.7)
train_data_normalized <- subset(cardiac_data, spl_normalized == TRUE)
test_data_normalized <- subset(cardiac_data, spl_normalized == FALSE)

# Build the logistic regression model on the normalized data
model <- glm(
  cardiac_condition_binary ~ .,
  data = train_data_normalized,
  family = binomial(logit)
)
summary(model)

# Make predictions on the test data
test_data_normalized$predicted.cardiac <- predict(model, newdata = test_data_normalized, type = "response")

# Converting to binary predictions
test_data_normalized$predicted.cardiac_binary <- ifelse(test_data_normalized$predicted.cardiac > 0.5, 1, 0)
test_data_normalized$predicted.cardiac_binary <- factor(test_data_normalized$predicted.cardiac_binary, levels = c("0", "1"))
test_data_normalized$cardiac_condition_binary <- factor(test_data_normalized$cardiac_condition_binary, levels = c("0", "1"))

# Evaluate the logistic regression model
conf_matrix_lr <- confusionMatrix(test_data_normalized$predicted.cardiac_binary, test_data_normalized$cardiac_condition_binary)
print("Confusion Matrix (Logistic Regression):")
print(conf_matrix_lr)

# Calculate evaluation metrics
accuracy_lr <- sum(diag(as.matrix(conf_matrix_lr))) / sum(as.matrix(conf_matrix_lr))
precision_lr <- conf_matrix_lr$byClass["Pos Pred Value"]
recall_lr <- conf_matrix_lr$byClass["Sensitivity"]
roc_curve <- roc(as.numeric(test$cardiac_condition_binary) - 1, test$predicted.cardiac)
roc_auc_lr <- auc(roc_curve)
f1_score_lr <- 2 * (precision_lr * recall_lr) / (precision_lr + recall_lr)

# Print the results
cat("Accuracy (Logistic Regression):", accuracy_lr, "\n")
cat("Precision (Logistic Regression):", precision_lr, "\n")
cat("Recall (Logistic Regression):", recall_lr, "\n")
cat("ROC AUC (Logistic Regression):", roc_auc_lr, "\n")
cat("F1 Score (Logistic Regression):", f1_score_lr, "\n")

# Creating ROC curve for Logistic Regression 
roc_curve <- roc(as.numeric(test_data_normalized$cardiac_condition_binary) - 1, test_data_normalized$predicted.cardiac)
cat("AUC:", round(roc_curve$auc, 4), "\n")

# Plotting ROC curve for Logistic Regression 
plot(roc_curve, main = "ROC Curve - Logistic Regression", col = "blue", lwd = 2)



# Calculate the correlation matrix for the specified variables
cor_matrix <- cor(train_data_normalized[, c("age", "weight", "fitness_score", "genderFemale", "genderMale")])
# Print the correlation matrix
print(cor_matrix)
# Calculating the correlation matrix for all numeric column
corrplot(cor_matrix, method = "color",
         tl.col = "black", tl.srt = 45, tl.cex = 0.7, addCoef.col = "black", number.cex = 0.7)


# Check for multicollinearity using variance inflation factor (VIF)
# Check for aliased coefficients
alias_summary <- alias(model)
print(alias_summary)

model <- glm(
  cardiac_condition_binary ~ age + weight + genderFemale + fitness_score,
  data = train_data_normalized,
  family = "binomial"
)

# Check for multicollinearity using variance inflation factor (VIF)
vif_values <- car::vif(model)
# Print VIF values
print("VIF values:")
print(vif_values)


#Applying Logistic Regression Model- Final Model
# Split the normalized data into training and testing sets
set.seed(22182918)
spl_cardiac_data_final <- sample.split(cardiac_data$cardiac_condition_binary, 0.7)
train_data_final <- subset(cardiac_data, spl_cardiac_data_final == TRUE)
test_data_final <- subset(cardiac_data, spl_cardiac_data_final == FALSE)
str(train_data_final)
str(test_data_final)
#Final model 
final_model <- glm(
  cardiac_condition_binary ~ age + weight + genderFemale + fitness_score,
  data = train_data_final,
  family = "binomial"
)
# Summarize the normalized model
summary(final_model)
# Assuming model_2 is the trained logistic regression model
logit_prob <- predict(final_model, newdata = test_data_final, type = "response")

# Create predicted.cardiac based on probabilities
test_data_final$predicted.cardiac <- logit_prob

# Verify that the column now exists and is not NULL
str(test_data_final)
# Converting to binary predictions
test_data_final$predicted.cardiac_binary <- ifelse(test_data_final$predicted.cardiac > 0.5, 1, 0)
test_data_final$predicted.cardiac_binary <- factor(test_data_final$predicted.cardiac_binary, levels = c("0", "1"))
test_data_final$cardiac_condition_binary <- factor(test_data_final$cardiac_condition_binary, levels = c("0", "1"))

# Evaluate the logistic regression model
conf_matrix_lr <- confusionMatrix(test_data_final$predicted.cardiac_binary, test_data_final$cardiac_condition_binary)
print("Confusion Matrix (Logistic Regression):")
print(conf_matrix_lr)

# Calculate evaluation metrics
accuracy_lr <- sum(diag(as.matrix(conf_matrix_lr))) / sum(as.matrix(conf_matrix_lr))
precision_lr <- conf_matrix_lr$byClass["Pos Pred Value"]
recall_lr <- conf_matrix_lr$byClass["Sensitivity"]
roc_curve <- roc(as.numeric(test_data_final$cardiac_condition_binary) - 1, test_data_final$predicted.cardiac)
roc_auc_lr <- auc(roc_curve)
f1_score_lr <- 2 * (precision_lr * recall_lr) / (precision_lr + recall_lr)

# Print the results
cat("Accuracy (Logistic Regression):", accuracy_lr, "\n")
cat("Precision (Logistic Regression):", precision_lr, "\n")
cat("Recall (Logistic Regression):", recall_lr, "\n")
cat("ROC AUC (Logistic Regression):", roc_auc_lr, "\n")
cat("F1 Score (Logistic Regression):", f1_score_lr, "\n")
# Creating ROC curve for Logistic Regression 
roc_curve <- roc(as.numeric(test_data_final$cardiac_condition_binary) - 1, test_data_final$predicted.cardiac)
cat("AUC:", round(roc_curve$auc, 4), "\n")

# Plotting ROC curve for Logistic Regression 
plot(roc_curve, main = "ROC Curve - Logistic Regression", col = "blue", lwd = 2)