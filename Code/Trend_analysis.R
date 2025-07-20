#Statistical test results for temperature (Tmin, Tmax and Tavg) trends.
# using Mann-Kendal, Kendals tau and Sens slope. 
#A csv file is created

library(modifiedmk)
library(ggplot2)
library(lmtest)
library(patchwork)
library(trend)
library(zyp)
library(ggrepel)
library(gridExtra)

analyze_temp_data <- function(temp_data, label_prefix) {
  years <- temp_data$Year
  stations <- names(temp_data)[names(temp_data) != "Year"]

  results_list <- list()

  for (station in stations) {
    temp_vector <- temp_data[[station]]
    non_na_years <- years[!is.na(temp_vector)]

    if (length(non_na_years) == 0) {
      next
    }

    first_year <- min(non_na_years)
    last_year <- max(non_na_years)
    valid_years <- years[years >= first_year & years <= last_year]
    valid_temp <- temp_vector[years >= first_year & years <= last_year]

    interp_temp <- approx(
      x = valid_years,
      y = valid_temp,
      xout = valid_years,
      method = "linear",
      rule = 1
    )$y

    acf_values <- acf(interp_temp, plot = FALSE)
    lag1_acf <- acf_values$acf[2]

    ljung_test <- Box.test(interp_temp, lag = 1, type = "Ljung-Box")
    dw_test <- dwtest(lm(interp_temp ~ 1))

    mk_result <- mkttest(interp_temp)
    mmk_result <- mmkh(interp_temp)
    sen_result <- sens.slope(interp_temp)
    sen_result_zyp <- zyp.sen(interp_temp ~ valid_years)

    summary_row <- data.frame(
      Station = station,
      LB = round(ljung_test$statistic, 3),
      LBp = round(ljung_test$p.value, 4),
      Z = if (!is.null(mmk_result) && length(mmk_result) >= 4) round(mmk_result[4], 3) else NA,
      p = if (!is.null(mmk_result) && length(mmk_result) >= 5) round(mmk_result[5], 4) else NA,
      Zcorr = if (!is.null(mmk_result) && length(mmk_result) >= 1) round(mmk_result[1], 3) else NA,
      pnew = if (!is.null(mmk_result) && length(mmk_result) >= 2) round(mmk_result[2], 4) else NA,
      Tau = if (!is.null(mmk_result) && length(mmk_result) >= 6) round(mmk_result[6], 3) else NA,
      slope = round(sen_result$estimates, 3)
    )

    results_list[[station]] <- summary_row
  }

  summary_df <- do.call(rbind, results_list)
  return(summary_df)
}

# Read data
tmin_data <- read.csv("tmin.csv")
tmax_data <- read.csv("tmax.csv")
tavg_data <- read.csv("avg.csv")

# Run analysis for each dataset
tmin_result <- analyze_temp_data(tmin_data, "Tmin")
tmax_result <- analyze_temp_data(tmax_data, "Tmax")
tavg_result <- analyze_temp_data(tavg_data, "Tavg")

# Merge results by Station
merged_results <- Reduce(function(x, y) merge(x, y, by = "Station", all = TRUE),
                         list(tmin_result, tmax_result, tavg_result))

# Order stations by the original Tmax column order
desired_order <- names(tmax_data)[names(tmax_data) != "Year"]
merged_results$Station <- factor(merged_results$Station, levels = desired_order)
merged_results <- merged_results[order(merged_results$Station), ]

# Add No. column
merged_results$No. <- seq_len(nrow(merged_results))
merged_results <- merged_results[, c("No.", "Station", setdiff(names(merged_results), c("No.", "Station")))]

# Final output
print(merged_results)

#  write to CSV
write.csv(merged_results, "final_results.csv", row.names = FALSE)

