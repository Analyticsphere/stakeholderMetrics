by_ruca_scatter_double <- function(data, site_name) {
  
  # Total Verified Clean/Arrange
  data_sub_tv <- filter(data, population == "total_verified", site == site_name)
  relevant_columns <- grep("urbanicity_", colnames(data_sub_tv), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month", "overall_count")
  ruca_data_tv <- data_sub_tv[, relevant_columns]
  
  # Pivot to Long for TV data
  long_ruca_tv <- ruca_data_tv %>%
    pivot_longer(cols = urbanicity_ruca_code_1:urbanicity_missing,
                 names_to = "urbanicity_ruca_code", 
                 names_prefix = "urbanicity_",
                 values_to = "n")
  
  # Create date variable
  long_ruca_tv$date <- as.Date(paste(long_ruca_tv$year, long_ruca_tv$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  # Rewrite urbanicity labels
  long_ruca_tv <- long_ruca_tv %>%
    mutate(urbanicity_ruca_code = case_when(
      urbanicity_ruca_code == "ruca_code_1" ~ "Code 1",
      urbanicity_ruca_code == "ruca_code_2" ~ "Code 2",
      urbanicity_ruca_code == "ruca_code_3" ~ "Code 3",
      urbanicity_ruca_code == "ruca_code_4" ~ "Code 4",
      urbanicity_ruca_code == "ruca_code_5" ~ "Code 5",
      urbanicity_ruca_code == "ruca_code_6" ~ "Code 6",
      urbanicity_ruca_code == "ruca_code_7" ~ "Code 7",
      urbanicity_ruca_code == "ruca_code_8" ~ "Code 8",
      urbanicity_ruca_code == "ruca_code_9" ~ "Code 9",
      urbanicity_ruca_code == "ruca_code_10" ~ "Code 10",
      urbanicity_ruca_code == "missing" ~ "Code Unknown",
      TRUE ~ urbanicity_ruca_code
    ))
  
  # Order and normalize by overall count
  long_ruca_tv <- long_ruca_tv[order(long_ruca_tv$date), ]
  long_ruca_tv$n <- long_ruca_tv$n / long_ruca_tv$overall_count
  
  # Response Ratio Clean/Arrange
  data_sub_rr <- filter(data, population == "response_ratio", site == site_name)
  relevant_columns <- grep("urbanicity_", colnames(data_sub_rr), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month")
  ruca_data_rr <- data_sub_rr[, relevant_columns]
  
  # Pivot to Long for RR data
  long_ruca_rr <- ruca_data_rr %>%
    pivot_longer(cols = urbanicity_ruca_code_1:urbanicity_missing,
                 names_to = "urbanicity_ruca_code", 
                 names_prefix = "urbanicity_",
                 values_to = "n")
  
  # Create date variable for RR data
  long_ruca_rr$date <- as.Date(paste(long_ruca_rr$year, long_ruca_rr$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  # Rewrite urbanicity labels for RR data
  long_ruca_rr <- long_ruca_rr %>%
    mutate(urbanicity_ruca_code = case_when(
      urbanicity_ruca_code == "ruca_code_1" ~ "Code 1",
      urbanicity_ruca_code == "ruca_code_2" ~ "Code 2",
      urbanicity_ruca_code == "ruca_code_3" ~ "Code 3",
      urbanicity_ruca_code == "ruca_code_4" ~ "Code 4",
      urbanicity_ruca_code == "ruca_code_5" ~ "Code 5",
      urbanicity_ruca_code == "ruca_code_6" ~ "Code 6",
      urbanicity_ruca_code == "ruca_code_7" ~ "Code 7",
      urbanicity_ruca_code == "ruca_code_8" ~ "Code 8",
      urbanicity_ruca_code == "ruca_code_9" ~ "Code 9",
      urbanicity_ruca_code == "ruca_code_10" ~ "Code 10",
      urbanicity_ruca_code == "missing" ~ "Code Unknown",
      TRUE ~ urbanicity_ruca_code
    ))
  
  # Order and normalize by overall count for RR data
  long_ruca_rr <- long_ruca_rr[order(long_ruca_rr$date), ]
  
  # Identify the number of unique urbanicity codes for color mapping
  unique_items <- unique(long_ruca_tv$urbanicity_ruca_code)
  n_colors <- length(unique(unique_items))
  
  # Ensure sufficient colors for urbanicity categories
  cols <- select_colors(color_palette2, n_colors)
  
  # Map colors to urbanicity codes
  color_mapping <- setNames(cols, unique_items)
  
  # Create plot
  plot <- plot_ly()
  
  # Add trace for TV data
  plot <- plot %>% add_trace(
    data = long_ruca_tv,
    x = ~date,
    y = ~n,
    color = ~urbanicity_ruca_code,
    colors = color_mapping,
    type = 'scatter',
    mode = 'lines+markers',
    hoverinfo = 'x+y',
    name = ~urbanicity_ruca_code  # Legend labels
  )
  
  # Add trace for RR data
  plot <- plot %>% add_trace(
    data = long_ruca_rr,
    x = ~date,
    y = ~n,
    color = ~urbanicity_ruca_code,
    colors = color_mapping,
    line = list(dash = "dash"),
    yaxis = "y2",
    type = 'scatter',
    mode = 'lines+markers',
    hoverinfo = 'x+y',
    showlegend = FALSE  # Prevent duplicate legend items
  )
  
  # Layout settings, including secondary y-axis for RR data
  ay <- list(
    overlaying = "y",
    side = "right",
    title = "<b>Response Ratio</b>"
  )
  plot <- plot %>%
    layout(
      yaxis2 = ay,
      title = paste(site_name, "By Urbanicity Code"),
      xaxis = list(title = "Date"),
      yaxis = list(title = "Total Verified"),
      legend = list(title = list(text = "Urbanicity Code"))
    )
  
  return(plot)
}

