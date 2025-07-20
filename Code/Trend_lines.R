# This R code plot the trend lines and  saves them into 4 eps files
library(ggplot2)
library(trend)
library(latex2exp)
library(dplyr)
library(patchwork)
library(tidyr)

# Read data
tmin_data <- read.csv("tmin.csv")
tmax_data <- read.csv("tmax.csv")
tavg_data <- read.csv("avg.csv")

years <- tmin_data$Year
station_ids <- colnames(tmin_data)[-1]

# Replace station names 
custom_station_names <- c(
  "Koronivia", "Labasa", "Lakeba", "Laucala", "Lautoka", "Matei", "Matuku",
  "Monasavu", "Nabouwalu", "Nadi", "Nausori", "Navua", "Ono-i-Lau", "Penang",
  "Rarawai", "Rotuma", "Savusavu", "Udu Point", "Vanuabalavu", "Viwa", "Vunisea",
  "Yasawa", "Nacocolevu"
)

# Map CSV column names to custom station names
display_names <- setNames(custom_station_names, station_ids)

create_station_plot <- function(station) {
  display_name <- display_names[station]
  tmin_vec <- tmin_data[[station]]
  tmax_vec <- tmax_data[[station]]
  tavg_vec <- tavg_data[[station]]
  
  non_na_years <- years[!is.na(tmin_vec)]
  if (length(non_na_years) == 0) return(NULL)
  
  first_year <- min(non_na_years)
  last_year <- max(non_na_years)
  valid_years <- years[years >= first_year & years <= last_year]
  
  tmin_interp <- approx(years, tmin_vec, xout = valid_years, rule = 1)$y
  tmax_interp <- approx(years, tmax_vec, xout = valid_years, rule = 1)$y
  tavg_interp <- approx(years, tavg_vec, xout = valid_years, rule = 1)$y
  
  slope_tmin <- sens.slope(tmin_interp)$estimates
  slope_tmax <- sens.slope(tmax_interp)$estimates
  slope_tavg <- sens.slope(tavg_interp)$estimates
  
  median_year <- median(valid_years)
  trend_tmin <- (median(tmin_interp) - slope_tmin * median_year) + slope_tmin * valid_years
  trend_tmax <- (median(tmax_interp) - slope_tmax * median_year) + slope_tmax * valid_years
  trend_tavg <- (median(tavg_interp) - slope_tavg * median_year) + slope_tavg * valid_years
  
  df_plot <- data.frame(
    Year = valid_years,
    Tmin = tmin_interp,
    Tmax = tmax_interp,
    Tavg = tavg_interp,
    Trend_Tmin = trend_tmin,
    Trend_Tmax = trend_tmax,
    Trend_Tavg = trend_tavg
  )
  
  # Label positions: find a lower point than the plot line for each variable
  label_x <- min(df_plot$Year) + 2
  label_y_tmin <- min(df_plot$Tmin, na.rm = TRUE) - 0.3
  label_y_tmax <- min(df_plot$Tmax, na.rm = TRUE) - 0.3
  label_y_tavg <- min(df_plot$Tavg, na.rm = TRUE) - 0.3
  
  ggplot(df_plot, aes(x = Year)) +
    geom_line(aes(y = Tmin), color = "blue", linewidth = 0.6) +
    geom_line(aes(y = Tmax), color = "red", linewidth = 0.6) +
    geom_line(aes(y = Tavg), color = "black", linewidth = 0.6) +
    geom_line(aes(y = Trend_Tmin), color = "blue", linetype = "dashed", linewidth = 0.6) +
    geom_line(aes(y = Trend_Tmax), color = "red", linetype = "dashed", linewidth = 0.6) +
    geom_line(aes(y = Trend_Tavg), color = "black", linetype = "dashed", linewidth = 0.6) +
    annotate("text", x = label_x, y = label_y_tmin, 
             label = TeX(paste0("$\\beta_{min} = ", round(slope_tmin, 3), "$")), 
             color = "blue", size = 3.5, hjust = 0) +
    annotate("text", x = label_x, y = label_y_tmax, 
             label = TeX(paste0("$\\beta_{max} = ", round(slope_tmax, 3), "$")), 
             color = "red", size = 3.5, hjust = 0) +
    annotate("text", x = label_x, y = label_y_tavg, 
             label = TeX(paste0("$\\beta_{avg} = ", round(slope_tavg, 3), "$")), 
             color = "black", size = 3.5, hjust = 0) +
    labs(
      title = paste0("(", which(names(display_names) == station), ") ", display_name),
      x = "Year",
      y = "Temperature (°C)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 9, color = "black", face = "bold"),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)))
}

# Generate all plots
plots <- lapply(station_ids, create_station_plot)
plots <- Filter(Negate(is.null), plots)

# Function to save plots in batches of 6
save_plot_batch <- function(plot_list, batch_number) {
  start_idx <- (batch_number - 1) * 6 + 1
  end_idx <- min(batch_number * 6, length(plot_list))
  batch_plots <- plot_list[start_idx:end_idx]
  
  if (length(batch_plots) == 0) return()
  
  combined_plot <- wrap_plots(batch_plots, ncol = 2) +
    plot_layout(guides = "collect", heights = rep(1, ceiling(length(batch_plots)/2))) &
    theme(plot.margin = margin(10, 5, 10, 5))
  
  ggsave(
    filename = paste0("stations_batch_", batch_number, ".eps"),
    plot = combined_plot,
    device = cairo_ps,
    width = 7,
    height = 9.5
  )
}

# Save all batches
num_batches <- ceiling(length(plots) / 6)
for (i in 1:num_batches) {
  save_plot_batch(plots, i)
}
