hp_aggregate_ses_scatter <- function(data){
  
  hp_data <- filter(data, site == "HealthPartners",
                    year < 2024,
                    population == "response_ratio")
  relevant_columns <- grep("socioeconomic_status_", colnames(hp_data), value = TRUE)
  relevant_columns <- c(relevant_columns,"year", "month")
  hp_data = hp_data[,relevant_columns]
  
  hp_data <- hp_data %>% pivot_longer(cols = socioeconomic_status_first_quartile:socioeconomic_status_missing,
                                      names_to = "ses_quartile", 
                                      names_prefix = "socioeconomic_status_",
                                      values_to = "rr")
  
  hp_data <- hp_data %>%
    mutate(ses_quartile = case_when(
      ses_quartile == "first_quartile" ~ "First Quartile",
      ses_quartile == "second_quartile" ~ "Second Quartile",
      ses_quartile == "third_quartile" ~ "Third Quartile",
      ses_quartile == "fourth_quartile" ~ "Fourth Quartile",
      ses_quartile == "missing" ~ "Missing",
      TRUE ~ ses_quartile  # Default case to handle any other values that do not match
    ))
  
  hp_data$date <- as.Date(paste(hp_data$year, hp_data$month, "01", sep = "-"), "%Y-%m-%d")
  
  #manually remove outliers
  hp_data <- hp_data %>%
    filter(!(rr > 1))
  
  #identify number of colors to use  
  unique_items <- unique(hp_data$ses_quartile)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  
  plot <- plot_ly(
    data = hp_data,
    x = ~date,
    y = ~rr,
    color = ~ses_quartile,
    colors = color_mapping,
    type = 'scatter',
    mode = 'markers',
    text = ~paste(ses_quartile),  # Custom text for hover
    hoverinfo = 'text+x+y'  # Specifies what info to display on hover
  ) %>%
    layout(
      title = "Response Ratio by Socioeconomic Status",
      xaxis = list(title = "Site"),
      yaxis = list(title = paste0("Response Ratio")),
      legend = list(title = list(text = "SES Quartile"))
    )
  
  plot
  
}
