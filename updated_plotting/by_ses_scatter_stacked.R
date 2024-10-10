by_ses_scatter_stacked <- function(data, site_name) {
  # Filter and prepare Total Verified (TV) data
  data_sub_tv <- filter(data, population == "total_verified", site == site_name)
  relevant_columns <- grep("socioeconomic_status_", colnames(data_sub_tv), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month", "overall_count")
  ses_data_tv <- data_sub_tv[, relevant_columns]
  
  # Pivot table for Total Verified
  long_ses_tv <- ses_data_tv %>% 
    pivot_longer(cols = socioeconomic_status_first_quartile:socioeconomic_status_missing,
                 names_to = "ses_quartile", 
                 names_prefix = "socioeconomic_status_",
                 values_to = "n")
  
  long_ses_tv$date <- as.Date(paste(long_ses_tv$year, long_ses_tv$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  # Update legend titles for TV data and set the order
  long_ses_tv <- long_ses_tv %>%
    mutate(ses_quartile = case_when(
      ses_quartile == "first_quartile" ~ "First Quartile",
      ses_quartile == "second_quartile" ~ "Second Quartile",
      ses_quartile == "third_quartile" ~ "Third Quartile",
      ses_quartile == "fourth_quartile" ~ "Fourth Quartile",
      ses_quartile == "missing" ~ "Missing",
      TRUE ~ ses_quartile
    )) %>%
    mutate(ses_quartile = factor(ses_quartile, levels = c("First Quartile", "Second Quartile", "Third Quartile", "Fourth Quartile", "Missing")))
  
  # Order TV data and normalize `n` by `overall_count`
  long_ses_tv <- long_ses_tv[order(long_ses_tv$date), ]
  long_ses_tv$n <- pmin((long_ses_tv$n / long_ses_tv$overall_count) * 100, 100)
  
  # Filter and prepare Response Ratio (RR) data
  data_sub_rr <- filter(data, population == "response_ratio", site == site_name)
  relevant_columns <- grep("socioeconomic_status_", colnames(data_sub_rr), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month")
  ses_data_rr <- data_sub_rr[, relevant_columns]
  
  # Pivot table for Response Ratio
  long_ses_rr <- ses_data_rr %>% 
    pivot_longer(cols = socioeconomic_status_first_quartile:socioeconomic_status_missing,
                 names_to = "ses_quartile", 
                 names_prefix = "socioeconomic_status_",
                 values_to = "n")
  
  long_ses_rr$date <- as.Date(paste(long_ses_rr$year, long_ses_rr$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  # Update legend titles for RR data and set the order
  long_ses_rr <- long_ses_rr %>%
    mutate(ses_quartile = case_when(
      ses_quartile == "first_quartile" ~ "First Quartile",
      ses_quartile == "second_quartile" ~ "Second Quartile",
      ses_quartile == "third_quartile" ~ "Third Quartile",
      ses_quartile == "fourth_quartile" ~ "Fourth Quartile",
      ses_quartile == "missing" ~ "Missing",
      TRUE ~ ses_quartile
    )) %>%
    mutate(ses_quartile = factor(ses_quartile, levels = c("First Quartile", "Second Quartile", "Third Quartile", "Fourth Quartile", "Missing")))
  
  # Order RR data
  long_ses_rr <- long_ses_rr[order(long_ses_rr$date), ]
  long_ses_rr$n = pmin(long_ses_rr$n * 100, 100)
  
  # Identify the number of unique SES quartiles for color mapping
  unique_items <- unique(long_ses_tv$ses_quartile)
  n_colors <- length(unique_items)
  
  # Select color palette and map colors
  cols <- select_colors(color_palette2, n_colors)
  color_mapping <- setNames(cols, unique_items)
  
  # Create plot for TV Data (Plot 1)
  plot1 <- plot_ly() %>%
    add_trace(
      data = long_ses_tv,
      x = ~date,
      y = ~n,
      color = ~ses_quartile,
      colors = color_mapping,
      type = 'scatter',
      mode = 'lines+markers',
      hoverinfo = 'x+y',
      hovertemplate = '%{x|%B %Y}<br>%{y:.2f}%',
      name = ~ses_quartile,  # Set name for the legend
      showlegend = TRUE,
      legendgroup = ~ses_quartile  # Group legends by SES
    ) %>%
    layout(
      yaxis = list(title = "% of Total Verified", range = c(0, 100)),
      xaxis = list(title = "Date"),
      showlegend = TRUE
    )
  
  # Create plot for RR Data (Plot 2)
  plot2 <- plot_ly() %>%
    add_trace(
      data = long_ses_rr,
      x = ~date,
      y = ~n,
      color = ~ses_quartile,
      colors = color_mapping,
      type = 'scatter',
      mode = 'lines+markers',
      hoverinfo = 'x+y',
      hovertemplate = '%{x|%B %Y}<br>%{y:.2f}%',
      name = ~ses_quartile,  # Set name for the legend
      showlegend = FALSE,
      legendgroup = ~ses_quartile  # Group legends by SES
    ) %>%
    layout(
      yaxis = list(title = "Response Ratio(%)", range = c(0, 10)),
      xaxis = list(title = "Date"),
      showlegend = TRUE
    )
  
  # Combine both plots in a stacked layout
  plot <- subplot(plot1, plot2, nrows = 2, shareX = TRUE, titleX = TRUE, titleY = TRUE) %>%
    layout(
      title = paste(site_name, "Total % Of Verified Participants & Response Ratio By Socioeconomic Status"),
      legend = list(title = list(text = "Socioeconomic Status"))
    )
  
  return(plot)
}
