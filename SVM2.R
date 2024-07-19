# Install necessary packages
install.packages("forecast")
install.packages("e1071")
install.packages("tseries")

# Load libraries
library(forecast)
library(e1071)
library(tseries)

# Load data
da <- read.table(file.choose(), header = TRUE, sep = "\t")

# Extract PM 2.5 data
pm25 <- da[1:62,2]
data_ts <- ts(pm25, frequency = 12, start = c(2018, 1))

# Plot the data
plot(data_ts, type = "l")
title(main="PM 2.5 in Tambon Wiang Amphoe Mueang Chiang Mai")

# Decompose the time series
de <- decompose(data_ts)
plot(de)

# Perform Augmented Dickey-Fuller Test
adf_test <- adf.test(data_ts)
print(adf_test)

# Fit ARIMA model
auto_arima_model <- auto.arima(data_ts)
summary(auto_arima_model)

# Forecast using ARIMA
arima_forecast <- forecast(auto_arima_model, h=12)
plot(arima_forecast)

# Extract residuals from ARIMA model
arima_residuals <- residuals(auto_arima_model)

# Define parameter grid for tuning
tune_result <- tune(svm, arima_residuals ~ time(arima_residuals), 
                    ranges = list(cost = c(0.1, 1, 10), 
                                  gamma = c(0.01, 0.1, 1)))

# Print best parameters
print(tune_result$best.parameters)

# Fit the best SVM model
svm_model <- tune_result$best.model

# Create a new time vector for SVM prediction
new_time <- data.frame(time = seq(length(arima_residuals) + 1, length(arima_residuals) + 12))

# Forecast residuals using SVM
svm_forecast <- predict(svm_model, newdata = new_time)

# Adjust the length of SVM forecast
if(length(svm_forecast) < 12) {
  svm_forecast <- c(svm_forecast, rep(NA, 12 - length(svm_forecast)))
} else if(length(svm_forecast) > 12) {
  svm_forecast <- svm_forecast[1:12]
}

# Combine ARIMA and SVM forecasts
hybrid_forecast <- arima_forecast$mean + svm_forecast

# Print hybrid forecast
print(hybrid_forecast)
