# Installing and loading necessary libraries
# install.packages(c("ggplot2","forecast", "TSA"))
# install.packages("lubridate")
# Install and load the zoo package if not already installed
#install.packages("zoo")
#install.packages("TSA")
library(ggplot2)
library(forecast)
library(TSA)
library(lubridate)
library(stats)
library(dplyr)
library(zoo) 

# Loading the Weather dataset into the weather_data variable
weather_data <- read.csv("weather_revised.csv")

# Exploratory Data Analysis (EDA)
# Descriptive Statistics
head(weather_data)
str(weather_data)
dim(weather_data)

# Keeping only the mean wind speed variable and date variable
weather_data <- weather_data[, c("date", "wdsp.Mean.Wind.Speed...knot.")]
head(weather_data)

# Checking Null values 
print(colSums(is.na(weather_data)))
summary(weather_data)
str(weather_data)

# Converting 'date' to Date type as it is in chr Date Type
# Extracting day and month from the original date
month_day <- sub("(\\d+-\\w+)-\\d\\d", "\\1", weather_data$date)
# Adjusting the year part
year <- as.numeric(sub(".*-(\\d\\d)$", "\\1", weather_data$date))
cutoff_year <- 23
adjusted_year <- ifelse(year <= cutoff_year, 2000 + year, 1900 + year)

# Combining day, month, and adjusted year to create a full date string
full_date_str <- paste(month_day, adjusted_year, sep = "-")
# Converting to date
weather_data$date <- as.Date(full_date_str, format = "%d-%b-%Y")
head(weather_data)

# Checking the maximum date in the dataset
max_date <- max(weather_data$date)
max_date

# Decomposing the time series to observe components
ts_wind_speed <- ts(weather_data$`wdsp.Mean.Wind.Speed...knot.`, start = c(1942, 1), frequency = 365)
decomposed <- stl(ts_wind_speed, s.window = "periodic")

# Plotting the original time series plot with a smoother trend line
ggplot(weather_data, aes(x = date, y = `wdsp.Mean.Wind.Speed...knot.`)) + 
  geom_line(color = "black", alpha = 0.5) +  
  geom_smooth(method = "loess", span = 0.2, color = "blue") +  # Smoothing line for trend
  labs(title = "Time Series Analysis of Mean Wind Speed (knots)",
       x = "Date", 
       y = "Mean Wind Speed (knots)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plotting Seasonal decomposition plot of mean wind speed
autoplot(decomposed) + 
  labs(title = "Decomposed Time Series of Mean Wind Speed (knots)",
       x = "Date",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Splitting the data into train and test set
train <- subset(weather_data, format(date, "%Y") %in% c("2019", "2020", "2021", "2022"))
test<- subset(weather_data, format(date, "%Y") == "2023")
str(train)
str(test)

#Checking train and test subset
head(train)
head(test)


# Applying Simple Moving Average model on Test Data and Training Data
window_size <- 12
train$sma_wind_speed <- stats::filter(train$`wdsp.Mean.Wind.Speed...knot.`, rep(1/window_size, window_size), sides = 2)

# To plot the SMA predictions with actual data
plot(train$date, train$`wdsp.Mean.Wind.Speed...knot.`, type = "l", col = "grey", xlab = "Date", ylab = "Wind Speed (knot)", main = "SMA vs Actual Data", cex = 1, pch = 50)
lines(train$date, train$sma_wind_speed, col = "blue", lty = 2, lwd = 1)  # Dashed line for SMA
# Adding a legend to the plot
legend("topright", legend = c("Actual Data", "SMA"), col = c("grey", "blue"), lty = c(1, 2))

# To Calculate Mean Absolute Error (MAE) for SMA on Test Data
test$sma_wind_speed <- stats::filter(test$`wdsp.Mean.Wind.Speed...knot.`, rep(1/window_size, window_size), sides = 2)
mae_sma <- mean(abs(test$`wdsp.Mean.Wind.Speed...knot.` - test$sma_wind_speed), na.rm = TRUE)
cat("Mean Absolute Error (MAE) of SMA on Test Data:", mae_sma, "\n")

# To Calculate Mean Squared Error (MSE) for SMA on Test Data
mse_sma <- mean((test$`wdsp.Mean.Wind.Speed...knot.` - test$sma_wind_speed)^2, na.rm = TRUE)
cat("Mean Squared Error (MSE) of SMA on Test Data:", mse_sma, "\n")


# To Calculate Root Mean Squared Error (RMSE)
rmse_sma <- sqrt(mse_sma)

calculate_mape <- function(actual, forecast) {
  actual_non_zero <- actual[actual != 0]
  forecast_non_zero <- forecast[actual != 0]
  mape <- mean(abs((actual_non_zero - forecast_non_zero) / actual_non_zero), na.rm = TRUE) * 100
  return(mape)
}

# To Calculate MAPE for SMA
mape_sma <- calculate_mape(test$`wdsp.Mean.Wind.Speed...knot.`, - test$sma_wind_speed)

# To Calculate Mean Absolute Error (MAE)
mae_sma_test <- mean(abs(test$`wdsp.Mean.Wind.Speed...knot.` - test$sma_wind_speed), na.rm = TRUE)

# Printing the results
cat("Mean Squared Error (MSE) for SMA:", mse_sma, "\n")
cat("Root Mean Squared Error (RMSE) for SMA:", rmse_sma, "\n")
cat("Mean Absolute Percentage Error (MAPE) for SMA:", mape_sma, "%\n")
cat("Mean Absolute Error (MAE) for SMA:", mae_sma_test, "\n")

# Dropping sma_wind_speed variable from the train set
train <- train[ , !(names(train) %in% c("sma_wind_speed"))]
str(train)

# Checking for missing values
any(is.na(test$`wdsp.Mean.Wind.Speed...knot.`))
# dropping null values if there are any
test <- na.omit(test)
# Checking the lengths of actual and forecasted data
length(test$`wdsp.Mean.Wind.Speed...knot.`)

# Aggregating Training Data by month
train <- train %>%
  mutate(month = format(date, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(mean_wind_speed = mean(`wdsp.Mean.Wind.Speed...knot.`, na.rm = TRUE))

# Aggregating Test Data by month
test <- test %>%
  mutate(month = format(date, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(mean_wind_speed = mean(`wdsp.Mean.Wind.Speed...knot.`, na.rm = TRUE))

head(train)
head(test)

# Converting test and Training data to time series objects
ts_train <- ts(train$mean_wind_speed, start = c(2019, 1), frequency = 12)
ts_test <- ts(test$mean_wind_speed, start = c(2023, 1), frequency = 12)

# Fitting an ETS model on the train data set
ets_model <- ets(ts_train)
# Forecasting using the fitted model
ets_forecast <- forecast(ets_model, h = length(ts_test))
# Extracting the forecast values from the data
ets_predictions <- ets_forecast$mean

# Evaluating the model
mae_ets <- mean(abs(ts_test - ets_predictions), na.rm = TRUE)
cat("Mean Absolute Error (MAE) of ETS:", mae_ets, "\n")


#  To Plot the forecast along with the actual data 
plot(ets_forecast, main = "ETS Model Forecast vs Actual")
lines(ts_test, col = "red", lty = 2) 
legend("bottomleft", legend = c("ETS Forecast", "Actual Data"), col = c("black", "red"), lty = 1:2)

# Evaluating the ETS model of the data
mse_ets <- mean((ts_test - ets_predictions)^2, na.rm = TRUE)
cat("Mean Squared Error (MSE) of ETS:", mse_ets, "\n")

mape_ets <- mean(abs((ts_test - ets_predictions) / ts_test)) * 100
cat("Mean Absolute Percentage Error (MAPE) of ETS:", mape_ets, "%\n")

# Summary of the ETS 
summary(ets_model)

# Checking for stationarity in the train data set
adf_test <- adf.test(ts_train)
p_value <- adf_test$p.value

print(adf_test)
print(p_value)

# Performing differencing as the data currently is non stationary
if (p_value > 0.05) {
  ts_diff <- diff(ts_train)
} else {
  ts_diff <- ts_train
}

# Checking for stationarity again with the diffrenced train data
adf_test <- adf.test(ts_diff)
p_value <- adf_test$p.value

print(adf_test)
print(p_value)

# Ploting ACF 
acf(ts_diff, main = 'Autocorrelation Function (ACF)')

# Ploting PACF
pacf(ts_diff, main = 'Partial Autocorrelation Function (PACF)')

# Converting to time series object
ts_diff <- ts(ts_diff, frequency = 12, start = c(2019, 2))

# Fitting SARIMA model
sarima_model <- Arima(ts_diff, order = c(1, 0, 1), seasonal = c(1, 1, 0))

# Summary of the model
summary(sarima_model)

# To Check Diagnostics 
checkresiduals(sarima_model)

# Generating diagnostic plots
sarima_forecast <- forecast(sarima_model)
plot(sarima_forecast)


# Converting 'month' to a Date object
test$month <- as.yearmon(test$month)

# Converting test data to a time series object
ts_test <- ts(test$mean_wind_speed, frequency = 12, start = c(2023, 1))

# Making forecasts on test data
sarima_forecast <- forecast(sarima_model, h = length(ts_test))

# Plotting the forecasts of the data
plot(sarima_forecast, main = 'SARIMA Forecast')
lines(ts_test, col = 'red', lty = 2)  # Add the actual values in red
legend('topright', legend = c('Forecast', 'Actual'), col = c('black', 'red'), lty = 1:2)

# Printing the forecast summary
print(sarima_forecast)
