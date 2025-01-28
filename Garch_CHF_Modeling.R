library(tidyverse)
library(quantmod)
setwd("C:/Users/pc/Desktop/Case_Study_II/ALL IN ONE")
################################################################################
# MODELING #####################################################################
################################################################################

Data_set <- read_csv("D:\\Desktop New\\NEWWWWW\\FOREX_MAJOR_PAIRS.csv")
View(Data_set)

# Dataset split
train_df <- Data_set[0:as.integer(nrow(Data_set)*0.8),]
test_df <- Data_set[as.integer(nrow(Data_set)*0.8):nrow(Data_set),]



#-----------------------------------------------------------------------------#
# Check stationarity-----------------------------------------------------------
#-----------------------------------------------------------------------------#
data_train <- train_df$`USDCHF=X`
data_test <- test_df$`USDCHF=X`


library(tseries)
Close_data <- ts(data_train , frequency = 252)


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
ggplot(train_df, aes(x = Date, y =`USDCHF=X` )) +
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

Train_diff <- as.numeric(diff(log(Close_data)))
Test_diff <- as.numeric(diff(log(Close_data)))


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
par(mfrow=c(2,1))
# Plot ACF with a specified lag range and adjusted x-axis labels
acf(Train_diff)

# Plot PACF with a specified lag range and adjusted x-axis labels
pacf(Train_diff)

#finding mean of returns
mean(Train_diff)
#almost zero

library(nortsTest)
arch.test(Train_diff)

McLeod.Li.test(y=Train_diff)
#acording to the arch test the null hypothesis is not rejected i.e the model will foloows GARCH or ARCH process.
#check the normality

shapiro.test(Train_diff)
hist(Train_diff)


#implementing arch n garch models

library(rugarch)
library(e1071)
library(tseries)
garch_model<-garch(Train_diff,grad = "numerical",trace = FALSE)
garch_model
