library(tidyverse)
library(quantmod)
setwd("C:/Users/pc/Desktop/Case_Study_II/ALL IN ONE")
################################################################################
# MODELING #####################################################################
################################################################################

Data_set <- read_csv("FOREX_MAJOR_PAIRS.csv")
View(Data_set)

# Dataset split
train_df <- Data_set[0:as.integer(nrow(Data_set)*0.8),]
test_df <- Data_set[as.integer(nrow(Data_set)*0.8):nrow(Data_set),]


#-----------------------------------------------------------------------------#
# Check stationarity-----------------------------------------------------------
#-----------------------------------------------------------------------------#
data_train <- train_df$`EURUSD=X`
data_test <- test_df$`EURUSD=X`


library(tseries)
Close_data <- ts(data_train , frequency = 5)


# Perform the Augmented Dickey-Fuller Test
adf_result <- adf.test(Close_data)
# Print ADF Test results
cat("Augmented Dickey-Fuller Test:\n")
print(adf_result)

# Time series is non stationary according to adf_test

# Perform the KPSS Test
kpss_result <- kpss.test(Close_data)
# Print KPSS Test results
cat("\nKwiatkowski-Phillips-Schmidt-Shin Test:\n")
print(kpss_result)

# Time series is stationary according to KPSS test

# Perform the PP Test
PP_result <- pp.test(Close_data)
# Print KPSS Test results
cat("\nPhillips-Perron (PP) test:\n")
print(PP_result)

# The time series isn't stationary.

#-----------------------------------------------------------------------------#
# Exploratory analysis --------------------------------------------------------
#-----------------------------------------------------------------------------#

# Plot the data
ggplot(train_df, aes(x = Date, y = `EURUSD=X`)) +
  geom_line(color = "blue") +
  labs( x = "Date", y = "Close Price") +
  theme_minimal()

#------------------------------------------------------------------------------#
# Decompose Data----------------------------------------------------------------
#------------------------------------------------------------------------------#

# Multiplicative model ---------------------------------------------------------
Decom_data_mul <- decompose(Close_data,type = "multiplicative")
plot(Decom_data_mul)
M_re <- Decom_data_mul$trend*Decom_data_mul$seasonal*Decom_data_mul$random
original_M <- Close_data[!is.na(M_re)]
M_re <- M_re[!is.na(M_re)]

# Mean Absolute Error (MAE)

mae <- mean(abs(original_M - M_re), na.rm = TRUE)
print(paste("Mean Absolute Error (MAE):", mae))

# Calculate Root Mean Square Error (RMSE)
rmse <- sqrt(mean((original_M - M_re)^2, na.rm = TRUE))
print(paste("Root Mean Square Error (RMSE):", rmse))


# Additive model ---------------------------------------------------------------
Decom_data_adi <- decompose(Close_data,type = "additive")
plot(Decom_data_adi)

A_re <- Decom_data_adi$trend + Decom_data_adi$seasonal + Decom_data_adi$random
original_A <- Close_data[!is.na(A_re)]
A_re <- A_re[!is.na(A_re)]

# Mean Absolute Error (MAE)

mae <- mean(abs(original_A - A_re), na.rm = TRUE)
print(paste("Mean Absolute Error (MAE):", mae))

# Calculate Root Mean Square Error (RMSE)
rmse <- sqrt(mean((original_A - A_re)^2, na.rm = TRUE))
print(paste("Root Mean Square Error (RMSE):", rmse))


#-----------------------------------------------------------------------------#
# Difference ------------------------------------------------------------------
#-----------------------------------------------------------------------------#


Train_diff <- as.numeric(diff(Close_data,lag = 1,order=1))
Test_diff <- as.numeric(diff(Close_data,lag = 1,order=1))


Diff_train_df <- data_frame(Date = train_df$Date[2:length(Close_data)] ,
                            Diffrence_Prices = Train_diff,
                            Original_Prices = Close_data[2:length(Close_data)])

# Plot the data
ggplot(Diff_train_df , aes(x = Date, y = Train_diff)) +
  geom_line(color = "blue") +
  labs( x = "Date", y = "Close Price") +
  theme_minimal()




#-----------------------------------------------------------------------------#
# Check stationarity-----------------------------------------------------------
#-----------------------------------------------------------------------------#

# Perform the Augmented Dickey-Fuller Test
adf_result <- adf.test(Train_diff)
# Print ADF Test results
cat("Augmented Dickey-Fuller Test:\n")
print(adf_result)

# Time series is non stationary according to adf_test

# Perform the KPSS Test
kpss_result <- kpss.test(Train_diff)
# Print KPSS Test results
cat("\nKwiatkowski-Phillips-Schmidt-Shin Test:\n")
print(kpss_result)

# Time series is stationary according to KPSS test

# Perform the PP Test
PP_result <- pp.test(Train_diff)
# Print KPSS Test results
cat("\nPhillips-Perron (PP) test:\n")
print(PP_result)

# The time series is now stationary.

# Set up the plotting area
par(mfrow=c(2,1)) # Arrange plots in 2 rows and 1 column
# Plot the original time series
plot(data_train, main="Original Time Series", ylab="Values", col="blue", lwd=2,type='l')
# Plot the differenced time series
plot(Train_diff, main="Differenced Time Series", ylab="Differenced Values", col="red", lwd=2,type = 'l')









#-----------------------------------------------------------------------------#
# Autocorrelation functions ---------------------------------------------------
#-----------------------------------------------------------------------------#

# ACF/PACF 
#par(mfrow=c(1,2))
# Plot ACF with a specified lag range and adjusted x-axis labels
acf(Train_diff, na.action = na.pass, main = '', lag.max = 30,ci=0.95)

# Plot PACF with a specified lag range and adjusted x-axis labels
pacf(Train_diff, na.action = na.pass, main = '', lag.max = 30,ci=0.95)





#-----------------------------------------------------------------------------#
# Model the mean --------------------------------------------------------------
#-----------------------------------------------------------------------------#
# Load necessary package
library(forecast)

# Fit an ARIMA model and automatically select the best (p, d, q)
auto_arima_model <- auto.arima(Close_data,seasonal = F)

# Display the model
summary(auto_arima_model)

# Loop for find the best model--------------------------------------------------

# Define the ranges for seasonal (p, d, q)
p_range <- 0:5  
d_range <- 1    
q_range <- 0:5

# Initialize a list to store models and AIC values
results <- list()

# Loop through all combinations of (p, d, q)
for (p in p_range) {
  for (q in q_range) {
    tryCatch({
      
      model <- arima(
        Close_data,
        order = c(p, d_range, q))
      
      # Store the model order and AIC value in the results list
      results <- append(results, list(list(p = p, d = d_range, q = q, AIC = model$aic)))
      
    }, error = function(e) {
      # Handle cases where the model fails to fit
      cat("Model failed for seasonal order (", p, ",", d_range, ",", q, ")\n")
    })
  }
}

# Convert the results list into a dataframe
results_df <- do.call(rbind, lapply(results, as.data.frame))
rownames(results_df) <- NULL  # Reset row names

# Print the dataframe of models and AIC values
print(results_df)

# Find and print the best model based on AIC
best_model_row <- results_df[which.min(results_df$AIC), ]
cat("Best Model: Order (p, d, q) = (", 
    best_model_row$p, ",", best_model_row$d, ",", best_model_row$q, 
    ") with AIC =", best_model_row$AIC, "\n")


model <- arima(Close_data , order=c(5 , 1 , 4))
summary(model)

#------------------------------------------------------------------------------#
# Assumptions check -----------------------------------------------------------#
#------------------------------------------------------------------------------#

ggplot(data_frame(Error = model$residuals), aes(Error)) +
  geom_histogram()

summary(model$residuals)

#------------------------------------------------------------------------------#
# Fitted values for train data set ---------------------------------------------
#------------------------------------------------------------------------------#

fitted_values <- forecast(model, h = length(data_train))$fitted

# Create time indices 
train_index <- 1:length(data_train) 
fitted_index <- train_index - 1

# Create data frames for plotting 
df_train <- data.frame(Time = train_index, Values = as.numeric(data_train),
                       Type = "Train") 
df_forecast <- data.frame(Time = fitted_index, Values = as.numeric(fitted_values),
                          Type = "Forecast") 

# Combine the data frames 
df <- rbind(df_train, df_forecast)
df <- df[df$Time >= 300 & df$Time <= 400, ]

# Plot model fit for train data
ggplot(df, aes(x = Time, y = Values, color = Type)) +
  geom_line() +
  labs(y = "Values", x = "Time") +
  scale_color_manual(values = c("Train" = "blue", "Forecast" = "black")) +
  theme_minimal()

ggplot(df, aes(x = Time, y = Values, color = Type)) +
  geom_line(data = df[df$Type == "Train", ]) +
  geom_point(data = df[df$Type == "Forecast", ], shape = 4, size = 3) + # Use shape 4 for "X"
  labs(y = "Values", x = "Time") +
  scale_color_manual(values = c("Train" = "blue", "Forecast" = "black")) +
  theme_minimal()
# -----------------------------------------------------------------------------#
# plot model forecast for test data---------------------------------------------
# -----------------------------------------------------------------------------#


# Forecast
forecast_values <- forecast(model, h = length(data_test))$mean

# Create time indices 
test_index <- 1:length(data_test)
forecast_index <- test_index

# Create data frames for plotting 
df_test <- data.frame(Time = test_index, Values = as.numeric(data_test),
                      Type = "Test") 
df_forecast <- data.frame(Time = forecast_index, Values = as.numeric(forecast_values),
                          Type = "Forecast") 

# Combine the data frames 
df <- rbind(df_test, df_forecast)
#df <- df[df$Time >= 300 & df$Time <= 400, ]

# Plot model fit for train data
ggplot(df, aes(x = Time, y = Values, color = Type)) +
  geom_line() +
  labs(y = "Values", x = "Time") +
  scale_color_manual(values = c("Test" = "black", "Forecast" = "red")) +
  theme_minimal()

#------------------------------------------------------------------------------#

# Create a data frame for ggplot
Train_df <- data.frame(
  Time = tail(train_df$Date,15),
  Value = tail(train_df$`EURUSD=X`,15),
  Type = "Train Data"
)

Fitted_df <- data.frame(
  Time = tail(train_df$Date,16)[1:15],
  Value = tail(fitted_values,15),
  Type = "Fitted Values"
)

Test_df <- data.frame(
  Time = test_df$Date[1:10],
  Value = test_df$`EURUSD=X`[1:10],
  Type = "Test Data"
)

Forecast_df <- data.frame(
  Time = test_df$Date[1:10],
  Value = forecast_values[1:10],
  Type = "Forecast"
)

# Combine all data frames
combined_df <- bind_rows(Train_df, Fitted_df, Test_df, Forecast_df)

# Plot using ggplot2
ggplot(combined_df, aes(x = Time, y = Value, color = Type)) +
  geom_line(size = 1) +
  labs(
    x = "Time",
    y = "Value",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_color_manual(values = c(
    "Train Data" = "blue",
    "Fitted Values" = "green",
    "Test Data" = "red",
    "Forecast" = "purple"
  )) +
  theme(legend.position = "top")



#-----------------------------------------------------------------------------#
# ARCH test -------------------------------------------------------------------
#-----------------------------------------------------------------------------#
library(nortsTest)

arch_test <- arch.test(model$residuals) 
print(arch_test)


arch.test(model$residuals,lag=1)


#------------------------------------------------------------------------------#
# ARCH modeling ---------------------------------------------------------------#
#------------------------------------------------------------------------------#
library(zoo)
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(rugarch)

train_xts <- as.xts(train_df)
return <- CalculateReturns(train_xts$`GBPUSD=X`)
return <- return[-1]
return<- na.omit(return)

# We now look at our data in the form of chart series to get
# a better understanding

autoplot(return)
hist(return)

chart.Histogram(return,
                methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'green', 'red'))
chartSeries(return)

# Before going further we will now look at the annual volatility using Rolling 
# statistics.

chart.RollingPerformance(R = return["2023::2024"],
                         width = 22,
                         FUN = "sd.annualized",
                         scale = 252,
                         main = "Yearly rolling volatility")

# Before applying GARCH model, we need to find the right set of values of GARCH 
# order and ARMA order , for this, we build a set of models using standard GARCH 
# by tweaking the order values a bit, and trying different combinations to find
# the most suitable one for forecasting.This particular model will be chosen by 
# looking for the least AIC(Akaike Information Criterion) value in each model.

#------------------------------------------------------------------------------#

# Fit ARIMA model to determine optimal order
auto_arima_model <- auto.arima(train_df$`GBPUSD=X`, seasonal = FALSE)
arima_order <- arimaorder(auto_arima_model)
p <- arima_order[1]  # AR order
d <- arima_order[2]  # Differencing order
q <- arima_order[3]  # MA order

# Define a grid search function for optimal GARCH parameters
garch_orders <- expand.grid(p = 1:2, q = 1:2)  # Adjust range as needed

best_aic <- Inf
best_model <- NULL
best_order <- NULL

for (i in 1:nrow(garch_orders)) {
  s1 <- ugarchspec(variance.model = list(model = "sGARCH",
                                         garchOrder = c(garch_orders$p[i], garch_orders$q[i])),
                   mean.model = list(armaOrder = c(p, q)),
                   distribution.model = "norm")
  
  fit <- ugarchfit(data = return, spec = s1)
  
  if (infocriteria(fit)[1] < best_aic) {  # AIC criterion
    best_aic <- infocriteria(fit)[1]
    best_model <- fit
    best_order <- garch_orders[i, ]
  }
}

# Print the best ARIMA and GARCH order
cat("Optimal ARIMA Order: (", p, ",", d, ",", q, ")\n")
cat("Optimal GARCH Order: (", best_order$p, ",", best_order$q, ")\n")


# Garch fit-------------------------------------------------------------------

s1 <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,2)),
                 mean.model=list(armaOrder=c(2,2)),distribution.model="norm")
m1 <- ugarchfit(data = return, spec = s1)
m1
plot(m1, which = 'all')



# Prediction -------------------------------------------------------------------
sfinal <- s1
setfixed(sfinal) <- as.list(coef(m1))

sim <- ugarchpath(spec = sfinal,
                  m.sim = 1,
                  n.sim = 1*10,
                  rseed = 1)
plot.zoo(fitted(sim))

plot.zoo(sigma(sim))

multiplier = mean(train_df$`GBPUSD=X`)

prediction<- multiplier*apply(fitted(sim), 2, 'cumsum') + multiplier
matplot(prediction, type = "l", lwd = 3)


# Create a data frame for ggplot
Train_df <- data.frame(
  Time = tail(train_df$Date,15),
  Value = tail(train_df$`GBPUSD=X`,15),
  Type = "Train Data"
)

Test_df <- data.frame(
  Time = test_df$Date[1:10],
  Value = test_df$`GBPUSD=X`[1:10],
  Type = "Test Data"
)

Forecast_df <- data.frame(
  Time = test_df$Date[1:10],
  Value = prediction[1:10],
  Type = "Forecast"
)

# Combine all data frames
combined_df <- bind_rows(Train_df, Test_df, Forecast_df)

# Plot using ggplot2
ggplot(combined_df, aes(x = Time, y = Value, color = Type)) +
  geom_line(size = 1) +
  labs(
    x = "Time",
    y = "Value",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_color_manual(values = c(
    "Train Data" = "blue",
    "Fitted Values" = "green",
    "Test Data" = "red",
    "Forecast" = "purple"
  )) +
  theme(legend.position = "top")




















