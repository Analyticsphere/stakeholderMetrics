by_ruca_scatter_stacked <- function(data, site_name) {
  
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
      urbanicity_ruca_code == "ruca_code_1" ~ "1",
      urbanicity_ruca_code == "ruca_code_2" ~ "2",
      urbanicity_ruca_code == "ruca_code_3" ~ "3",
      urbanicity_ruca_code == "ruca_code_4" ~ "4",
      urbanicity_ruca_code == "ruca_code_5" ~ "5",
      urbanicity_ruca_code == "ruca_code_6" ~ "6",
      urbanicity_ruca_code == "ruca_code_7" ~ "7",
      urbanicity_ruca_code == "ruca_code_8" ~ "8",
      urbanicity_ruca_code == "ruca_code_9" ~ "9",
      urbanicity_ruca_code == "ruca_code_10" ~ "10",
      urbanicity_ruca_code == "missing" ~ "Unk.",
      TRUE ~ urbanicity_ruca_code
    ))
  
  # Order and normalize by overall count
  long_ruca_tv <- long_ruca_tv[order(long_ruca_tv$date), ]
  long_ruca_tv$hover_date <- format(long_ruca_tv$date, "%B %Y")
  long_ruca_tv$n <- long_ruca_tv$n / long_ruca_tv$overall_count * 100
  
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
  long_ruca_rr$hover_date <- format(long_ruca_rr$date, "%B %Y")
  
  # Rewrite urbanicity labels for RR data
  long_ruca_rr <- long_ruca_rr %>%
    mutate(urbanicity_ruca_code = case_when(
      urbanicity_ruca_code == "ruca_code_1" ~ "1",
      urbanicity_ruca_code == "ruca_code_2" ~ "2",
      urbanicity_ruca_code == "ruca_code_3" ~ "3",
      urbanicity_ruca_code == "ruca_code_4" ~ "4",
      urbanicity_ruca_code == "ruca_code_5" ~ "5",
      urbanicity_ruca_code == "ruca_code_6" ~ "6",
      urbanicity_ruca_code == "ruca_code_7" ~ "7",
      urbanicity_ruca_code == "ruca_code_8" ~ "8",
      urbanicity_ruca_code == "ruca_code_9" ~ "9",
      urbanicity_ruca_code == "ruca_code_10" ~ "10",
      urbanicity_ruca_code == "missing" ~ "Unk.",
      TRUE ~ urbanicity_ruca_code
    ))
  
  # Order and normalize by overall count for RR data
  long_ruca_rr <- long_ruca_rr[order(long_ruca_rr$date), ]
  long_ruca_rr$n = long_ruca_rr$n * 100
  
  # Identify the number of unique urbanicity codes for color mapping
  unique_items <- unique(long_ruca_tv$urbanicity_ruca_code)
  n_colors <- length(unique(unique_items))
  
  # Ensure sufficient colors for urbanicity categories
  cols <- select_colors(color_palette2, n_colors)
  
  # Map colors to urbanicity codes
  color_mapping <- setNames(cols, unique_items)
  
  # Create plot for TV Data (Plot 1)
  plot1 <- plot_ly() %>%
    add_trace(
      data = long_ruca_tv,
      x = ~date,
      y = ~n,
      color = ~urbanicity_ruca_code,
      colors = color_mapping,
      type = 'scatter',
      mode = 'lines+markers',
      text = ~hover_date,
      hoverinfo = 'text+y',
      hovertemplate = '%{x|%B %Y}<br>%{y:.2f}%',
      name = ~urbanicity_ruca_code,  # Set name for the legend
      showlegend = TRUE,
      legendgroup = ~urbanicity_ruca_code  # Group legends by urbanicity
    ) %>%
    layout(
      yaxis = list(title = "% of Total Verified", range = c(0, 100)),
      xaxis = list(title = "Date"),
      showlegend = TRUE  # Ensure hovermode is x unified for plot1
    )
  
  # Create plot for RR Data (Plot 2)
  plot2 <- plot_ly() %>%
    add_trace(
      data = long_ruca_rr,
      x = ~date,
      y = ~n,
      color = ~urbanicity_ruca_code,
      colors = color_mapping,
      type = 'scatter',
      mode = 'lines+markers',
      text = ~hover_date,
      hoverinfo = 'text+y',
      hovertemplate = '%{x|%B %Y}<br>%{y:.2f}%',
      showlegend = FALSE,
      legendgroup = ~urbanicity_ruca_code  # Group legends by urbanicity
    ) %>%
    layout(
      yaxis = list(title = "Response Ratio (%)"),
      xaxis = list(title = "Date"),
      showlegend = TRUE
        # Ensure hovermode is x unified for plot2
    )
  
  # Combine both plots in a stacked layout
  plot <- subplot(plot1, plot2, nrows = 2, shareX = TRUE, titleX = TRUE, titleY = TRUE) %>%
    layout(
      title = paste(site_name, "Total % of Verified Participants & Response Ratio by RUCA Code"),
      legend = list(title = list(text = "RUCA Code")),
      hovermode = "x all unified")

  
  return(plot)
}

