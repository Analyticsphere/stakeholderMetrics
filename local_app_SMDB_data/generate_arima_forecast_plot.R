generate_arima_forecast_plot <- function(data, date_col, value_col, h = 52, series_name = "Time Series") {
  library(forecast)
  library(plotly)
  library(dplyr)
  library(lubridate)
  
  # Ensure necessary columns exist
  if (!(date_col %in% names(data)) || !(value_col %in% names(data))) {
    stop("Specified columns do not exist in the data frame.")
  }
  
  # Convert to appropriate types
  data[[date_col]] <- as.Date(data[[date_col]], origin = "1970-01-01")
  data[[value_col]] <- as.numeric(data[[value_col]])
  
  # Filter and order the data
  data <- data %>%
    arrange(.data[[date_col]]) %>%
    filter(!is.na(.data[[date_col]]), !is.na(.data[[value_col]]))
  
  # Check for sufficient data length
  if (nrow(data) < h) {
    stop("Not enough data points for reliable forecasting.")
  }
  
  # Create ts object
  start_date <- min(data[[date_col]])
  end_date <- max(data[[date_col]])
  ts_data <- ts(data[[value_col]], start = c(year(start_date), week(start_date)), frequency = 52)
  
  # Fit ARIMA model
  fit <- auto.arima(ts_data)
  
  # Forecast
  future <- forecast(fit, h = h)
  
  # Prepare forecast dates starting from the day after the last date in actual data
  future_dates <- seq.Date(from = end_date + 1, by = "week", length.out = h)
  
  # Prepare forecast data for plotting
  forecast_data <- data.frame(Time = future_dates, Forecast = future$mean)
  
  # Plot
  forecast_plot <- plot_ly() %>%
    add_trace(data = data, x = ~.data[[date_col]], y = ~.data[[value_col]], type = 'scatter', mode = 'lines', name = 'Original Data') %>%
    add_trace(data = forecast_data, x = ~Time, y = ~Forecast, type = 'scatter', mode = 'lines', name = 'Forecast') %>%
    add_ribbons(data = forecast_data, x = ~Time, ymin = future$lower[,2], ymax = future$upper[,2], name = '95% Confidence Interval') %>%
    layout(title = paste("ARIMA Forecast for", series_name),
           xaxis = list(title = "Time", type = "date"),
           yaxis = list(title = "Value"))
  
  return(forecast_plot)
}
