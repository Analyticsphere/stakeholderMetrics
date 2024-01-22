generate_arima_forecast_plot <- function(data, date_col= "verified_date", value_col = "cumul_verified", h = 52, series_name = "Verified") {
  library(forecast)
  library(plotly)
  library(dplyr)
  library(lubridate)
  
  #aggregation
  data$Verified_wkdate <- as.Date(data$Verified_wkdate, format = "%Y-%m-%d")  # adjust the format as per your data
  data$verified <- ifelse(is.na(data$Verified_wkdate), 0, 1)
  data$blood <- ifelse(is.na(data$Blood_wk), 0, 1)
  data$all_survey_complete <- ifelse(is.na(data$CompleteDate), 0, 1)
  data$only_blood <- ifelse((is.na(data$CompleteDate)&!is.na(data$Blood_wk)), 1, 0)
  data$blood_and_all_survey <- ifelse((data$blood ==1 & data$all_survey_complete==1), 1, 0)
  data$only_all_survey_complete <- ifelse((data$blood ==0 & data$all_survey_complete==1), 1, 0)
  
  
  #lastupdateblooddate = 
  #lastmoduledate = the latest date any module was completed
  agg <- aggregate(list(data$verified, data$blood, data$all_survey_complete, data$only_blood, data$blood_and_all_survey, data$only_all_survey_complete), list(verifiedcount = activity_data$Verified_wkdate) , FUN=sum)
  
  colnames(agg) <- c("verified_date", "verified", "blood", "all_survey_complete", "only_blood", "blood_and_all_survey", "only_all_survey_complete")
  
  #cumulative variables
  agg$cumul_verified <- cumsum(agg$verified)
  
  
  # Sorting the data by VerifiedWeekDate
  agg <- agg[order(as.Date(agg$verified_date)),]
  data = agg
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
