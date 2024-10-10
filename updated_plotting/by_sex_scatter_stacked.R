by_sex_scatter_stacked <- function(data, site_name) {
  # Filter and prepare Total Verified (TV) data
  data_sub_tv <- filter(data, population == "total_verified", site == site_name)
  relevant_columns <- grep("sex_", colnames(data_sub_tv), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month", "overall_count")
  sex_data_tv <- data_sub_tv[, relevant_columns]
  
  # Pivot table for Total Verified
  long_sex_tv <- sex_data_tv %>%
    pivot_longer(cols = sex_female:sex_unknown,
                 names_to = "sex",
                 names_prefix = "sex_",
                 values_to = 'n')
  
  long_sex_tv$date <- as.Date(paste(long_sex_tv$year, long_sex_tv$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  # Update legend titles for TV data
  long_sex_tv <- long_sex_tv %>%
    mutate(sex = case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      sex == "unknown" ~ "Unknown"))
  
  # Order TV data and normalize `n` by `overall_count`
  long_sex_tv <- long_sex_tv[order(long_sex_tv$date), ]
  long_sex_tv$n <- pmin((long_sex_tv$n / long_sex_tv$overall_count)*100,100)
  # long_sex_tv$n <- trunc(long_sex_tv$n)
  
  
  # Filter and prepare Response Ratio (RR) data
  data_sub_rr <- filter(data, population == "response_ratio", site == site_name)
  relevant_columns <- grep("sex_", colnames(data_sub_rr), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month")
  sex_data_rr <- data_sub_rr[, relevant_columns]
  
  # Pivot table for Response Ratio
  long_sex_rr <- sex_data_rr %>%
    pivot_longer(cols = sex_female:sex_unknown,
                 names_to = "sex",
                 names_prefix = "sex_",
                 values_to = 'n')
  
  long_sex_rr$date <- as.Date(paste(long_sex_rr$year, long_sex_rr$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  # Update legend titles for RR data
  long_sex_rr <- long_sex_rr %>%
    mutate(sex = case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      sex == "unknown" ~ "Unknown"))
  
  # Order RR data
  long_sex_rr <- long_sex_rr[order(long_sex_rr$date), ]
  long_sex_rr$n = pmin(long_sex_rr$n * 100, 100)
  # long_sex_rr$n = trunc(long_sex_rr$n)
  
  # Identify the number of unique sex values for color mapping
  unique_items <- unique(long_sex_tv$sex)
  n_colors <- length(unique_items)
  
  # Select color palette and map colors
  cols <- select_colors(color_palette2, n_colors)
  color_mapping <- setNames(cols, unique_items)
  
  # Create plot for TV Data (Plot 1)
  plot1 <- plot_ly() %>%
    add_trace(
      data = long_sex_tv,
      x = ~date,
      y = ~n,
      color = ~sex,
      colors = color_mapping,
      type = 'scatter',
      mode = 'lines+markers',
      hoverinfo = 'x+y',
      hovertemplate = '%{x|%B %Y}<br>%{y:.2f}%',
      name = ~sex,  # Set name for the legend
      showlegend = TRUE,
      legendgroup = ~sex  # Group legends by sex
    ) %>%
    layout(
      yaxis = list(title = "% of Total Verified", range = c(0, 100)),
      xaxis = list(title = "Date"),
      showlegend = TRUE
    )
  
  # Create plot for RR Data (Plot 2)
  plot2 <- plot_ly() %>%
    add_trace(
      data = long_sex_rr,
      x = ~date,
      y = ~n,
      color = ~sex,
      colors = color_mapping,
      type = 'scatter',
      mode = 'lines+markers',
      hoverinfo = 'x+y',
      hovertemplate = '%{x|%B %Y}<br>%{y:.2f}%',
      name = ~sex,  # Set name for the legend
      showlegend = FALSE,
      legendgroup = ~sex  # Group legends by sex
    ) %>%
    layout(
      yaxis = list(title = "Response Ratio(%)", range = c(0,10)),
      xaxis = list(title = "Date"),
      showlegend = TRUE
    )

  
  # Combine both plots in a stacked layout
  plot <- subplot(plot1, plot2, nrows = 2, shareX = TRUE, titleX = TRUE, titleY = TRUE) %>%
    layout(
      title = paste(site_name, "Total % Of Verified Participants & Response Ratio By Sex"),
      legend = list(title = list(text = "Sex"))
    )
  
  return(plot)
}
