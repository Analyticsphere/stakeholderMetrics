by_race_scatter_stacked <- function(data, site_name){
  # Total Verified Clean/Arrange
  data_sub_tv <- filter(data, population == "total_verified", site == site_name)
  relevant_columns <- grep("urbanicity_", colnames(data_sub_tv), value = TRUE)
  relevant_columns <- grep("race_ethnicity_", colnames(data_sub_tv), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month", "overall_count")
  race_data_tv = data_sub_tv[,relevant_columns]
  
  
  long_race_tv <- race_data_tv %>% pivot_longer(cols = race_ethnicity_white_hispanic:race_ethnicity_unknown_unknown_ethnicity,
                                                names_to = "race_ethnicity", 
                                                names_prefix = "race_ethnicity_",
                                                values_to = "n")
  long_race_tv <- long_race_tv[,c("year", "month", "race_ethnicity", "n", "overall_count")]
  
  #create date variable
  long_race_tv$date <- as.Date(paste(long_race_tv$year, long_race_tv$month, "01", sep = "-"), format = "%Y-%m-%d")
  long_race_tv$n <- long_race_tv$n / long_race_tv$overall_count * 100

  
  race_mapping <- list(
    "White" = c("white_hispanic", "white_non_hispanic", "white_unknown_ethnicity"),
    "Black" = c("black_hispanic", "black_non_hispanic", "black_unknown_ethnicity"),
    "Asian" = c("asian_hispanic", "asian_non_hispanic", "asian_unknown_ethnicity"),
    "Native_American" = c("native_american_hispanic", "native_american_non_hispanic", "native_american_unknown_ethnicity"),
    "Native_Hawaiian" = c("native_hawaiian_hispanic", "native_hawaiian_non_hispanic", "native_hawaiian_unknown_ethnicity"),
    "Other" = c("other_hispanic", "other_non_hispanic", "other_unknown_ethnicity"),
    "Unknown" = c("unknown_hispanic", "unknown_non_hispanic", "unknown_unknown_ethnicity"),
    "Multiracial" = c("more_than_one_race_hispanic", "more_than_one_race_non_hispanic", "more_than_one_race_unknown")
  )
  
  # Filter, map, and summarize the data
  filtered_data_tv <- long_race_tv %>%
    mutate(race_ethnicity = case_when(
      race_ethnicity %in% race_mapping$White ~ "White",
      race_ethnicity %in% race_mapping$Black ~ "Black",
      race_ethnicity %in% race_mapping$Asian ~ "Asian",
      race_ethnicity %in% race_mapping$'Native_American' ~ "Native American",
      race_ethnicity %in% race_mapping$'Native_Hawaiian' ~ "Native Hawaiian",
      race_ethnicity %in% race_mapping$Other ~ "Other",
      race_ethnicity %in% race_mapping$Unknown ~ "Unknown",
      race_ethnicity %in% race_mapping$Multiracial ~ "Multiracial"
    )) %>%
    group_by(race_ethnicity, date) %>%
    summarize(n = sum(n, na.rm = TRUE)) 

  # Format Date
  filtered_data_tv$hover_date <- format(filtered_data_tv$date, "%B %Y")
  
  # Total Verified Clean/Arrange
  data_sub_rr <- filter(data, population == "total_verified", site == site_name)
  relevant_columns <- grep("urbanicity_", colnames(data_sub_rr), value = TRUE)
  relevant_columns <- grep("race_ethnicity_", colnames(data_sub_rr), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month")
  race_data_rr = data_sub_rr[,relevant_columns]
  
  
  long_race_rr <- race_data_rr %>% pivot_longer(cols = race_ethnicity_white_hispanic:race_ethnicity_unknown_unknown_ethnicity,
                                          names_to = "race_ethnicity", 
                                          names_prefix = "race_ethnicity_",
                                          values_to = "n")
  long_race_rr <- long_race_rr[,c("year", "month", "race_ethnicity", "n")]
  
  #create date variable
  long_race_rr$date <- as.Date(paste(long_race_rr$year, long_race_rr$month, "01", sep = "-"), format = "%Y-%m-%d")

  # Filter, map, and summarize the data
  filtered_data_rr <- long_race_rr %>%
    mutate(race_ethnicity = case_when(
      race_ethnicity %in% race_mapping$White ~ "White",
      race_ethnicity %in% race_mapping$Black ~ "Black",
      race_ethnicity %in% race_mapping$Asian ~ "Asian",
      race_ethnicity %in% race_mapping$'Native_American' ~ "Native American",
      race_ethnicity %in% race_mapping$'Native_Hawaiian' ~ "Native Hawaiian",
      race_ethnicity %in% race_mapping$Other ~ "Other",
      race_ethnicity %in% race_mapping$Unknown ~ "Unknown",
      race_ethnicity %in% race_mapping$Multiracial ~ "Multiracial"
    )) %>%
    group_by(race_ethnicity, date) %>%
    summarize(n = sum(n, na.rm = TRUE)) 
  
  #Fix date
  filtered_data_rr$hover_date <- format(filtered_data_rr$date, "%B %Y")
  
  #identify number of colors to use  
  unique_items <- unique(filtered_data_tv$race_ethnicity)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette2, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  
  # Create plot for TV Data (Plot 1)
  plot1 <- plot_ly() %>%
    add_trace(
      data = filtered_data_tv,
      x = ~date,
      y = ~n,
      color = ~race_ethnicity,
      colors = color_mapping,
      type = 'scatter',
      mode = 'lines+markers',
      text = ~hover_date,
      hoverinfo = 'text+y',
      hovertemplate = '%{x|%B %Y}<br>%{y:.2f}%',
      name = ~race_ethnicity,  # Set name for the legend
      showlegend = TRUE,
      legendgroup = ~race_ethnicity  # Group legends by urbanicity
    ) %>%
    layout(
      yaxis = list(title = "% of Total Verified", range = c(0, 100)),
      xaxis = list(title = "Date"),
      showlegend = TRUE  # Ensure hovermode is x unified for plot1
    )
  
  # Create plot for RR Data (Plot 2)
  plot2 <- plot_ly() %>%
    add_trace(
      data = filtered_data_rr,
      x = ~date,
      y = ~n,
      color = ~race_ethnicity,
      colors = color_mapping,
      type = 'scatter',
      mode = 'lines+markers',
      text = ~hover_date,
      hoverinfo = 'text+y',
      hovertemplate = '%{x|%B %Y}<br>%{y:.2f}%',
      showlegend = FALSE,
      legendgroup = ~race_ethnicity  # Group legends by urbanicity
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
      title = paste(site_name, "Total % of Verified Participants & Response Ratio by Race"),
      legend = list(title = list(text = "Race")),
      hovermode = "x unified")
  
  
  return(plot)
}

