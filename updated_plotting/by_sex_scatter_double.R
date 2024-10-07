by_sex_scatter_double <- function(data, site_name) {
  # Write new dataframes to pull from
  data_sub_tv <- filter(data, population == "total_verified")
  data_sub_tv <- filter(data_sub_tv, site == site_name)
  relevant_columns <- grep("sex_", colnames(data_sub_tv), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month", "overall_count")
  sex_data_tv = data_sub_tv[,relevant_columns]
  
  # Pivot table
  long_sex_tv <- sex_data_tv %>% pivot_longer(cols = sex_female:sex_unknown,
                                        names_to = "sex", 
                                        names_prefix = "sex_",
                                        values_to = 'n')
  
  long_sex_tv$date <- as.Date(paste(long_sex_tv$year, long_sex_tv$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  # Rewrite legend titles
  long_sex_tv <- long_sex_tv %>%
    mutate(sex = case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      sex == "unknown" ~ "Unknown"))
  
  # Order table so lines populate correctly
  long_sex_tv = long_sex_tv[order(long_sex_tv$date), ]
  long_sex_tv$n = long_sex_tv$n/long_sex_tv$overall_count
  
  # Write new dataframes to pull from
  data_sub_rr <- filter(data, population == "response_ratio")
  data_sub_rr <- filter(data_sub_rr, site == site_name)
  relevant_columns <- grep("sex_", colnames(data_sub_rr), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month")
  sex_data_rr = data_sub_rr[,relevant_columns]
  
  # Pivot table
  long_sex_rr <- sex_data_rr %>% pivot_longer(cols = sex_female:sex_unknown,
                                              names_to = "sex", 
                                              names_prefix = "sex_",
                                              values_to = 'n')
  
  long_sex_rr$date <- as.Date(paste(long_sex_rr$year, long_sex_rr$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  # Rewrite legend titles
  long_sex_rr <- long_sex_rr %>%
    mutate(sex = case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      sex == "unknown" ~ "Unknown"))
  
  # Order table so lines populate correctly
  long_sex_rr = long_sex_rr[order(long_sex_rr$date), ]
  
  
  #identify number of colors to use  
  unique_items <- unique(long_sex_tv$sex)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette2, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  
  # Create plot
  plot <- plot_ly()
  
  # Add trace for TV Data
  plot = plot %>% add_trace(
    data = long_sex_tv,
    x = ~date,
    y = ~n,
    color = ~sex,
    colors = color_mapping,
    type = 'scatter',
    mode = 'lines + markers',
    hoverinfo = 'x+y') # Specifies what info to display on hover
  
  # Add trace for RR Data
  plot = plot %>% add_trace(
    data = long_sex_rr,
    x = ~date,
    y = ~n,
    color = ~sex,
    colors = color_mapping,
    line = list (dash = "dash"),
    yaxis = "y2",
    type = 'scatter',
    mode = 'lines + markers',
    hoverinfo = 'x+y',
    showlegend = FALSE) # Specifies what info to display on hover

    ay <- list(
    overlaying = "y",
    side = "right",
    title = "<b>Response Ratio</b>")
  plot <- plot %>%
    layout(
      yaxis2 = ay,
      title = paste (site_name, "By Sex"),
      xaxis = list(title = "Date"),
      yaxis = list(title = paste ("Total Verified")), 
      legend = list(title = list(text = "Sex")))

  plot
}