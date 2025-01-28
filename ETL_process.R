library(tidyverse)
library(quantmod)
setwd("C:/Users/pc/Desktop/Case_Study_II/ALL IN ONE")
# Extract the data #############################################################

# Daily data for one year-------------------------------------------------------
pairs <- c("EURUSD=X","GBPUSD=X","USDJPY=X","USDCHF=X","AUDUSD=X","USDCAD=X","NZDUSD=X")
start_date <- as.Date("2023-01-02")
end_date <- as.Date("2024-08-30")


# Create a sequence of dates 
date_seq <- seq(start_date, end_date, by = "day") 
# Exclude Saturdays and Sundays 
date_seq <- date_seq[!(weekdays(date_seq) %in% c("Saturday", "Sunday"))] 
# Create a DataFrame 
DF_Data <- data.frame(Date = date_seq)


for (pair in pairs){
  
  #Get the data
  df <- getSymbols(pair, src = "yahoo", from = start_date, to = end_date, 
                   auto.assign = TRUE)
  df_daily <- get(pair)
  
  # Data preparation #############################################################
  
  df_daily <-data.frame(Date = index(df_daily), coredata(df_daily))
  
  # Convert Date column to Date type
  df_daily$Date <- as.Date(df_daily$Date)
  
  # Create a full sequence of dates between the start and end date
  full_dates <- seq(start_date, end_date, by = "day")
  
  # Identify missing dates (Ensure dates are in the correct format)
  missing_dates <- as.Date(setdiff(full_dates, df_daily$Date), origin = "1970-01-01")
  
  # Get the names of the missing dates
  missing_date_names <- weekdays(missing_dates)
  
  # Combine missing dates and their names into a data frame
  missing_info <- data.frame(Date = missing_dates, Day = missing_date_names)
  
  #So there are some fridays are missing so 
  
  # Find missing Fridays
  missing_fridays <- missing_dates[weekdays(missing_dates) == "Friday"]
  
  # Function to find Thursday of the same week for a given date (Friday)
  find_thursday <- function(date) {
    date - 1  # Subtract 1 day to get Thursday
  }
  
  new_rows <- data.frame()
  # Fill missing Fridays with the entire row of the corresponding Thursday
  for (friday in missing_fridays) {
    friday = as.Date(friday, origin = "1970-01-01")
    corresponding_thursday <- find_thursday(friday)
    
    # Check if Thursday exists in the data
    if (corresponding_thursday %in% df_daily$Date) {
      thursday_row <- df_daily[df_daily$Date == corresponding_thursday, ]
      
      # Change the Date to Friday in the copied row
      thursday_row$Date <- friday
      # Add the new row for the missing Friday
      new_rows <- rbind(new_rows, thursday_row)
    }
  }
  
  # Combine the original data with the new rows
  df_daily <- rbind(df_daily, new_rows)
  
  # Remove rows where Date is Saturday or Sunday
  df_daily <- df_daily[!weekdays(df_daily$Date) %in% c("Saturday", "Sunday"), ]
  
  # Sort data by date
  df_daily <- df_daily[order(df_daily$Date), ]
  
  DF_Data[pair] <- df_daily[,5]
  
}

write_csv(DF_Data,"FOREX_MAJOR_PAIRS.csv")


