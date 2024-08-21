response_ratio_by_ses <- function(data){
  
  rr_data <- filter(data, population == "response_ratio")
  rr_data <- filter(rr_data, site == "HealthPartners")
  relevant_columns <- grep("socioeconomic_status_", colnames(rr_data), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month")
  ses_data = rr_data[,relevant_columns]
  
  long_ses <- ses_data %>% pivot_longer(cols = socioeconomic_status_first_quartile:socioeconomic_status_missing,
                                        names_to = "ses_quartile", 
                                        names_prefix = "socioeconomic_status_",
                                        values_to = "rr")
  
  long_ses <- filter(long_ses, month != 6)
  long_ses$date <- as.Date(paste(long_ses$year, long_ses$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  
  
  long_ses <- long_ses %>%
    mutate(ses_quartile = case_when(
      ses_quartile == "first_quartile" ~ "First Quartile",
      ses_quartile == "second_quartile" ~ "Second Quartile",
      ses_quartile == "third_quartile" ~ "Third Quartile",
      ses_quartile == "fourth_quartile" ~ "Fourth Quartile",
      ses_quartile == "missing" ~ "Missing",
      TRUE ~ ses_quartile  # Default case to handle any other values that do not match
    ))
  
  long_ses$rr <- round(long_ses$rr,2) 
  long_ses <- long_ses %>% filter(rr <= 1)
  
  #identify number of colors to use  
  unique_items <- unique(long_ses$ses_quartile)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  
  plot <- plot_ly(
    data = long_ses,
    x = ~date,
    y = ~rr,
    color = ~ses_quartile,
    colors = color_mapping,
    type = 'scatter',
    mode = 'markers',
    hoverinfo = 'x+y'  # Specifies what info to display on hover
  ) %>%
    layout(
      title = "Response Ratio by Socioeconomic Status",
      xaxis = list(title = "Date"),
      yaxis = list(title = paste0("Response Ratio")),
      legend = list(title = list(text = "SES Quartile"))
    )
  
  plot
  
}
