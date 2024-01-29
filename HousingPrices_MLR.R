install.packages("ggmap")
install.packages("sp")
install.packages("psych")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("car")
install.packages("ggplot2")
install.packages("scales")

# Load necessary libraries
library(tidyverse)
library(corrplot)  #For correlation matrix visualization
library(ggmap)     #For geospatial visualization
library(car)  
library(psych)
library(ggplot2)
library(scales)  

#loading the Housing Data Set
stat_dataset <- read.csv('housing.csv')
head(stat_dataset)

# Exploratory Data Analysis (EDA)
# Descriptive Statistics
summary(stat_dataset)
# Figure 1
str(stat_dataset)
#Figure 2
describe(stat_dataset)
# Checking the class of each variable for level of Measurement 
class_variable <- sapply(stat_dataset, class)
# Print the results
print(class_variable)

get_measurement_level <- function(variable) {
  if (is.numeric(variable) & !is.integer(variable)) "Interval"
  else if (is.numeric(variable) & is.integer(variable)) "Ratio"
  else if (is.factor(variable) | is.character(variable)) {
    if (length(unique(variable)) == length(variable)) "Nominal"
    else "Ordinal"
  } else "Not determined"
}

#Figure 3
# checking for the level of measurement 
measurement_levels <- sapply(stat_dataset, get_measurement_level)
print(data.frame(Variable = names(stat_dataset), Level_of_Measurement = measurement_levels))

# Figure 5
# Checking for missing values
any(is.na(stat_dataset))
# Display missing value counts for each columns
colSums(is.na(stat_dataset))

#Visualization of Distribution
# Histogram for Sale Price
# Figure 4
ggplot(stat_dataset, aes(x = Sale_Price)) +
  geom_histogram(binwidth = 20000, fill = "blue", color = "black") +
  labs(title = "Distribution of Sale Prices", x = "Sale Price") +
  scale_x_continuous(labels = scales::comma)  # Format x-axis labels

# Histogram for Lot Area
ggplot(stat_dataset, aes(x = Lot_Area)) +
  geom_histogram(binwidth = 3000, fill = "green", color = "black") +
  labs(title = "Distribution of Lot Area", x = "Lot Area")

# Histogram for Lot Frontage
ggplot(stat_dataset, aes(x = Lot_Frontage)) +
  geom_histogram(binwidth = 10, fill = "purple", color = "black") +
  labs(title = "Distribution of Lot Frontage", x = "Lot Frontage")

# Histogram for Year Built
ggplot(stat_dataset, aes(x = Year_Built)) +
  geom_histogram(binwidth = 10, fill = "red", color = "black") +
  labs(title = "Distribution of Year Built", x = "Year_Built")

# Histogram for Total Bsmt SF
ggplot(stat_dataset, aes(x = Total_Bsmt_SF)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black") +
  labs(title = "Distribution of Total Bsmt SF", x = "Total_Bsmt_SF")


# Histogram for First Flr SF
ggplot(stat_dataset, aes(x = First_Flr_SF)) +
  geom_histogram(binwidth = 100, fill = "pink", color = "black") +
  labs(title = "Distribution of First Flr SF", x = "First_Flr_SF")


# Histogram for Second Flr SF
ggplot(stat_dataset, aes(x = Second_Flr_SF)) +
  geom_histogram(binwidth = 100, fill = "orange", color = "black") +
  labs(title = "Distribution of Second Flr SF", x = "Second_Flr_SF")

# Histogram for Longitude
ggplot(stat_dataset, aes(x = Longitude)) +
  geom_histogram(binwidth = 0.01, fill = "red", color = "black") +
  labs(title = "Distribution of Longitude", x = "Longitude")

# Histogram for Latitude
ggplot(stat_dataset, aes(x = Latitude)) +
  geom_histogram(binwidth = 0.01, fill = "blue", color = "black") +
  labs(title = "Distribution of Latitude", x = "Latitude")


# Box Plots
#Box Plot for Sale Price
# Figure 4
ggplot(stat_dataset, aes(y = Sale_Price)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Box Plot of Sale Price", y = "Sale Price") +
  scale_y_continuous(labels = scales::comma)

# Box plot for Bldg Type
ggplot(stat_dataset, aes(x = Bldg_Type, y = Sale_Price)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Sale Price by Building Type", x = "Building Type", y = "Sale Price") +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels

# Box plot for Lot Frontage
ggplot(stat_dataset, aes(x = Lot_Frontage, y = Sale_Price)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Sale Price by Lot_Frontage ", x = "Lot_Frontage", y = "Sale Price") +
  scale_y_continuous(labels = scales::comma)

# Box plot for Lot Area
ggplot(stat_dataset, aes(x = Lot_Area, y = Sale_Price)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Sale Price by Lot_Area", x = "Lot_Area", y = "Sale Price") +
  scale_y_continuous(labels = scales::comma)

# Box plot for Latitude
ggplot(stat_dataset, aes(x = Latitude, y = Sale_Price)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Sale Price by Latitude", x = "Latitude", y = "Sale Price") +
  scale_y_continuous(labels = scales::comma)

# Box plot for Longitude
ggplot(stat_dataset, aes(x = Longitude, y = Sale_Price)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Sale Price by Longitude", x = "Longitude", y = "Sale Price") +
  scale_y_continuous(labels = scales::comma)

# Box plot for Year Built
ggplot(stat_dataset, aes(x = Year_Built, y = Sale_Price)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Sale Price by Year_Built", x = "Year_Built", y = "Sale Price") +
  scale_y_continuous(labels = scales::comma)

# Box plot for Total Bsmt Sf
ggplot(stat_dataset, aes(x = Total_Bsmt_SF, y = Sale_Price)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Sale Price by Total_Bsmt_SF", x = "Total_Bsmt_SF", y = "Sale Price") +
  scale_y_continuous(labels = scales::comma)

# Box plot for First Flr SF
ggplot(stat_dataset, aes(x = First_Flr_SF, y = Sale_Price)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Sale Price by First_Flr_SF", x = "First_Flr_SF", y = "Sale Price") +
  scale_y_continuous(labels = scales::comma)

# Box plot for Second Flr SF
ggplot(stat_dataset, aes(x = Second_Flr_SF, y = Sale_Price)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Sale Price by Second_Flr_SF", x = "Second_Flr_SF", y = "Sale Price") +
  scale_y_continuous(labels = scales::comma)


#count plot for Overall Cond
ggplot(stat_dataset, aes(x = Overall_Cond)) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Count of Houses by Overall Condition", x = "Overall Condition", y = "Count")

#count plot for Bldg Type
#Figure 4
ggplot(stat_dataset, aes(x = Bldg_Type)) +
  geom_bar(fill = "red", color = "black") +
  labs(title = "Count of Houses by Building Type", x = "Building Type", y = "Count")

#count plot for House Style
ggplot(stat_dataset, aes(x = House_Style)) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Count of Houses by House Style", x = "House Style", y = "Count")

#count plot for Exterior condition
ggplot(stat_dataset, aes(x = Exter_Cond)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Count of Houses by Exterior Condition", x = "Exterior Condition", y = "Count")

# Figure 8
# Correlation Matrix - Heatmap
cor_matrix <- cor(stat_dataset[, c("Lot_Frontage", "Lot_Area", "Total_Bsmt_SF", "Sale_Price","Latitude","Longitude")])
corrplot(cor_matrix, method = "circle")

#Figure 4
#Geospatial Distribution of Sale Prices - scatter plot 
ggplot(stat_dataset, aes(x = Longitude, y = Latitude, color = Sale_Price)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red", labels = scales::comma) +
  labs(title = "Geospatial Distribution of Sale Prices", x = "Longitude", y = "Latitude")


#Data Preprocessing 
#Performing Label Encoding on Categorical Variables

# Check the unique values in Overall Condition
unique(stat_dataset$Overall_Cond)
# Performing Label Encoding - Replacing  character levels with numeric labels
stat_dataset$Overall_Cond <- as.numeric(factor(stat_dataset$Overall_Cond, levels = c(
  "Very_Poor", "Poor", "Below_Average", "Average", "Above_Average", 
  "Good", "Very_Good","Fair", "Excellent"
)))
# Checking the unique values after label encoding
unique(stat_dataset$Overall_Cond)
head(stat_dataset$Overall_Cond)


# Check the unique values in Exter Condition
unique(stat_dataset$Exter_Cond)
# Performing Label Encoding - Replace character levels with numeric labels
stat_dataset$Exter_Cond <- as.numeric(factor(stat_dataset$Exter_Cond, levels = c("Poor", "Fair", "Typical", "Good", "Excellent")))
# Check the unique values after label encoding
unique(stat_dataset$Exter_Cond)
head(stat_dataset$Exter_Cond)


# Applying one-hot encoding to Bldg Type
bldg_type_encoded <- model.matrix(~Bldg_Type - 1, data = stat_dataset)
colnames(bldg_type_encoded) <- gsub("Bldg_Type", "", colnames(bldg_type_encoded))
# Applying one-hot encoding to House Style
house_style_encoded <- model.matrix(~House_Style - 1, data = stat_dataset)
colnames(house_style_encoded) <- gsub("House_Style", "", colnames(house_style_encoded))
# Concatenate the encoded variables to the original data set
stat_dataset_encoded <- cbind(stat_dataset, bldg_type_encoded, house_style_encoded)
# Remove the original categorical variables from the data set
stat_dataset_encoded <- stat_dataset_encoded[, !(names(stat_dataset) %in% c("Bldg_Type", "House_Style"))]
head(stat_dataset_encoded)

#Figure 6
# Creating a copy of the encoded data set for normalization
stat_dataset_normalized <- stat_dataset_encoded
#Normalizing whole data set except Sale Price
numeric_columns <- setdiff(names(stat_dataset_encoded), c('Sale_Price'))

# Normalizing numeric columns
for (name in numeric_columns) {
  stat_dataset_normalized[[name]] <- (stat_dataset_encoded[[name]] - min(stat_dataset_encoded[[name]])) /
    (max(stat_dataset_encoded[[name]]) - min(stat_dataset_encoded[[name]]))
}
# Print the normalized data
print(stat_dataset_normalized)
head(stat_dataset_normalized)

#Applying Multiple Linear Regression Model- Initial Model
# Splitting Data for modelling

#setting random seed as my student number 
set.seed(22182918)  

train_index <- sample(1:nrow(data), 0.7 * nrow(data)) 

train_data <- stat_dataset_normalized[train_index, ] 

test_data <- stat_dataset_normalized[-train_index, ] 

#Figure 10
lm_model <- lm(Sale_Price ~ ., data = stat_dataset_normalized)
summary(lm_model)


#Finding outliers
# Figure 7
# Finding outliers for Sale Price using z-scores
z_scores <- scale(stat_dataset_normalized$Sale_Price)
outliers <- abs(z_scores) > 3  
# Printing the indices of outliers
cat("Indices of Outliers:\n")
print(which(outliers))
# Displaying the values of Sale Price for outliers
cat("Values of Sale_Price for Outliers:\n")
print(stat_dataset_normalized$Sale_Price[outliers])

summary(stat_dataset_normalized$Sale_Price)
# Removing outliers
stat_dataset_normalized <- stat_dataset_normalized[!outliers, ]
# Print the summary after removing outliers
summary(stat_dataset_normalized$Sale_Price)


# Finding outliers for Lot frontage using z-scores
z_scores <- scale(stat_dataset_normalized$Lot_Frontage)
outliers <- abs(z_scores) > 3 
# Printing the indices of outliers
cat("Indices of Outliers:\n")
print(which(outliers))
# Displaying the values of Lot frontage for outliers
cat("Values of Lot_Frontage for Outliers:\n")
print(stat_dataset_normalized$Lot_Frontage[outliers])
summary(stat_dataset_normalized$Lot_Frontage)
# Removing outliers
stat_dataset_normalized <- stat_dataset_normalized[!outliers, ]
# Print the summary after removing outliers
summary(stat_dataset_normalized$Lot_Frontage)


# Finding outliers for Lot Area using z-scores
z_scores <- scale(stat_dataset_normalized$Lot_Area)
outliers <- abs(z_scores) > 3 
# Printing the indices of outliers
cat("Indices of Outliers:\n")
print(which(outliers))
# Displaying the values of Lot Area for outliers
cat("Values of Lot Area for Outliers:\n")
print(stat_dataset_normalized$Lot_Area[outliers])
summary(stat_dataset_normalized$Lot_Area)
# Removing outliers
stat_dataset_normalized <- stat_dataset_normalized[!outliers, ]
# Printing the summary after removing outliers
summary(stat_dataset_normalized$Lot_Area)


# Finding outliers for Year Built using z-scores
z_scores <- scale(stat_dataset_normalized$Year_Built)
outliers <- abs(z_scores) > 3 
# Printing the indices of outliers
cat("Indices of Outliers:\n")
print(which(outliers))
# Displaying the values of Year Built for outliers
cat("Values of Year Built for Outliers:\n")
print(stat_dataset_normalized$Year_Built[outliers])
summary(stat_dataset_normalized$Year_Built)
# Removing outliers
stat_dataset_normalized <- stat_dataset_normalized[!outliers, ]
# Printing the summary after removing outliers
summary(stat_dataset_normalized$Year_Built)


# Finding outliers for  Total Basement SF  using z-scores
z_scores <- scale(stat_dataset_normalized$Total_Bsmt_SF)
outliers <- abs(z_scores) > 3 
# Printing the indices of outliers
cat("Indices of Outliers:\n")
print(which(outliers))
# Displaying the values of Total Bsmt SF for outliers
cat("Values of Total Bsmt SF for Outliers:\n")
print(stat_dataset_normalized$Total_Bsmt_SF[outliers])
summary(stat_dataset_normalized$Total_Bsmt_SF)
# Removing outliers
stat_dataset_normalized <- stat_dataset_normalized[!outliers, ]
# Printing the summary after removing outliers
summary(stat_dataset_normalized$Total_Bsmt_SF)

# Finding outliers for 'First Flr SF using z-scores
z_scores <- scale(stat_dataset_normalized$First_Flr_SF)
outliers <- abs(z_scores) > 3 
# Printing the indices of outliers
cat("Indices of Outliers:\n")
print(which(outliers))
# Displaying the values of First Flr SF for outliers
cat("Values of First Flr SF for Outliers:\n")
print(stat_dataset_normalized$First_Flr_SF[outliers])
summary(stat_dataset_normalized$First_Flr_SF)
# Removing outliers
stat_dataset_normalized <- stat_dataset_normalized[!outliers, ]
# Printing the summary after removing outliers
summary(stat_dataset_normalized$First_Flr_SF)

# Finding outliers for 'Second Flr SF using z-scores
z_scores <- scale(stat_dataset_normalized$Second_Flr_SF)
outliers <- abs(z_scores) > 3 
# Printing the indices of outliers
cat("Indices of Outliers:\n")
print(which(outliers))
# Displaing the values of Second Flr SF for outliers
cat("Values of Second Flr SF for Outliers:\n")
print(stat_dataset_normalized$Second_Flr_SF[outliers])
summary(stat_dataset_normalized$Second_Flr_SF)
# Removing outliers
stat_dataset_normalized<- stat_dataset_normalized[!outliers, ]
# Printing the summary after removing outliers
summary(stat_dataset_normalized$Second_Flr_SF)


# Finding outliers for Longitude using z-scores
z_scores_longitude <- scale(stat_dataset_normalized$Longitude)
threshold <- 2
outliers_longitude <- abs(z_scores_longitude) > threshold
# Printing the indices of outliers
cat("Indices of Outliers for Longitude:\n")
print(which(outliers_longitude))
# Removing outliers
stat_dataset_normalized <- stat_dataset_normalized[!outliers, ]
# Printing the summary after removing outliers
summary(stat_dataset_normalized$Longitude)
# Removing missing values
stat_dataset_normalized <- stat_dataset_normalized[complete.cases(stat_dataset_normalized$Longitude), ]
# Printing the summary statistics after handling missing values 
summary(stat_dataset_normalized$Longitude)


# Finding outliers for Latitude using z-scores
z_scores_latitude <- scale(stat_dataset_normalized$Latitude)
threshold <- 3
outliers_latitude <- abs(z_scores_latitude) > threshold
# Printing the indices of outliers
cat("\nIndices of Outliers for Latitude:\n")
print(which(outliers_latitude))
# Check for missing values
sum(is.na(stat_dataset_normalized))
colSums(is.na(stat_dataset_normalized))

#Figure 11
#Applying Intermediate model-1 after removing outliers
lm_model <- lm(Sale_Price ~ ., data = stat_dataset_normalized)
summary(lm_model)

#Figure 9
# Selecting numeric variables
numeric_variables <- stat_dataset_encoded[, sapply(stat_dataset_normalized, is.numeric)]
# Calculate the correlation matrix
correlation_matrix <- cor(numeric_variables)
# Printing the correlation matrix
print(correlation_matrix)
# Creating a heatmap of the correlation matrix
corrplot(correlation_matrix, method = "color")
str(stat_dataset_normalized)

#Apply Multicollinearity 
lm_model <- lm(Sale_Price ~ . - Two_Story, data = stat_dataset_normalized)
summary(lm_model)
#Figure 12
vif_values <- car::vif(lm_model)
print(vif_values)

#Figure 13
#Appying Intermediate model 2 
lm_model <- lm(Sale_Price ~ Lot_Frontage + Lot_Area + Overall_Cond + Year_Built + 
                 Exter_Cond + First_Flr_SF +Total_Bsmt_SF +
                 Full_Bath + Half_Bath + Bedroom_AbvGr + Kitchen_AbvGr + 
                 Fireplaces + Longitude + Latitude + Duplex + OneFam + TwoFmCon 
               + One_and_Half_Fin + One_and_Half_Unf + SFoyer + 
                 SLvl + Two_and_Half_Fin + Two_and_Half_Unf , 
               data = stat_dataset_normalized)

summary(lm_model)




#Appying Feature selection 
#Appying Final Model 
#Figure 14
lm_model <- lm(Sale_Price ~ Lot_Frontage + Lot_Area + Overall_Cond + Year_Built + 
                 First_Flr_SF +Total_Bsmt_SF  + 
                 Full_Bath + Half_Bath + Bedroom_AbvGr + Kitchen_AbvGr + 
                 Fireplaces + Longitude + Latitude + Duplex + OneFam + TwoFmCon 
               + One_and_Half_Fin + One_and_Half_Unf  + SFoyer + Two_Story,
               data = stat_dataset_normalized)

summary(lm_model)

#Figure 15
# Diagnostic plots for assumptions for Homoscedasticity
par(mfrow = c(2, 2))
plot(lm_model)

#Figure 16
# Predict on the test data
y_pred <- predict(lm_model, newdata = test_data)
# Creating a data frame with actual and predicted values
plot_data <- data.frame(Actual = test_data$Sale_Price, Predicted = y_pred)
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted Sale Price",
       x = "Actual Sale Price",
       y = "Predicted Sale Price") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma) +  
  scale_y_continuous(labels = scales::comma)

#Figure 17
#Checking RMSE and MAE Values
rmse <- sqrt(mean((y_pred - test_data$Sale_Price)^2))
mae <- mean(abs(y_pred - test_data$Sale_Price))
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")




