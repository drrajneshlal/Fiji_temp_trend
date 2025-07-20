
#This R codes computes  Descriptive statistics of (Tmin, Tmax, and Tavg) at 23 stations in Fiji.
library(modifiedmk)

# Load the data
tmax_data <- read.csv("tmax.csv")     # Tmax data
tmin_data <- read.csv("tmin.csv")    # Tmin data
tavg_data <- read.csv("avg.csv")       # Tavg data
years <- tmax_data$Year

# Check if year columns match
stopifnot(all(tmax_data$Year == tmin_data$Year))
stopifnot(all(tmax_data$Year == tavg_data$Year))

# Station names
stations <- names(tmax_data)[names(tmax_data) != "Year"]

# Create summary table
summary_table <- data.frame(
  No. = integer(),
  Station_Name = character(),
  Time_Period = character(),
  
  Tmax_Max = numeric(),
  Tmax_Min = numeric(),
  Tmax_Mean = numeric(),
  Tmax_SD = numeric(),
  
  Tmin_Max = numeric(),
  Tmin_Min = numeric(),
  Tmin_Mean = numeric(),
  Tmin_SD = numeric(),
  
  Tavg_Max = numeric(),
  Tavg_Min = numeric(),
  Tavg_Mean = numeric(),
  Tavg_SD = numeric(),
  
  stringsAsFactors = FALSE
)

counter <- 1
for (station in stations) {
  tmax_vector <- tmax_data[[station]]
  tmin_vector <- tmin_data[[station]]
  tavg_vector <- tavg_data[[station]]
  
  non_na_years <- years[!is.na(tmax_vector) | !is.na(tmin_vector) | !is.na(tavg_vector)]
  if (length(non_na_years) == 0) next
  
  first_year <- min(non_na_years)
  last_year <- max(non_na_years)
  
  valid_indices <- years >= first_year & years <= last_year
  valid_years <- years[valid_indices]
  
  tmax_valid <- tmax_vector[valid_indices]
  tmin_valid <- tmin_vector[valid_indices]
  tavg_valid <- tavg_vector[valid_indices]
  
  # Interpolate internal NAs only (no extrapolation)
  tmax_interp <- approx(x = valid_years, y = tmax_valid, xout = valid_years, method = "linear", rule = 1)$y
  tmin_interp <- approx(x = valid_years, y = tmin_valid, xout = valid_years, method = "linear", rule = 1)$y
  tavg_interp <- approx(x = valid_years, y = tavg_valid, xout = valid_years, method = "linear", rule = 1)$y
  
  # Populate row without year columns
  summary_table[counter, ] <- list(
    No. = counter,
    Station_Name = station,
    Time_Period = paste0(first_year, "–", last_year),
    
    Tmin_Max = round(max(tmin_interp, na.rm = TRUE), 2),
    Tmin_Min = round(min(tmin_interp, na.rm = TRUE), 2),
    Tmin_Mean = round(mean(tmin_interp, na.rm = TRUE), 2),
    Tmin_SD = round(sd(tmin_interp, na.rm = TRUE), 2),
    
    Tmax_Max = round(max(tmax_interp, na.rm = TRUE), 2),
    Tmax_Min = round(min(tmax_interp, na.rm = TRUE), 2),
    Tmax_Mean = round(mean(tmax_interp, na.rm = TRUE), 2),
    Tmax_SD = round(sd(tmax_interp, na.rm = TRUE), 2),
    
    
    Tavg_Max = round(max(tavg_interp, na.rm = TRUE), 2),
    Tavg_Min = round(min(tavg_interp, na.rm = TRUE), 2),
    Tavg_Mean = round(mean(tavg_interp, na.rm = TRUE), 2),
    Tavg_SD = round(sd(tavg_interp, na.rm = TRUE), 2)
  )
  
  counter <- counter + 1
}

# Print the final summary
print(summary_table)


