response_ratio_by_ses <- function(data){
  
  rr_data <- filter(data, population == "response_ratio")
  rr_data <- filter(rr_data, site == "Marshfield Clinic Health System")
  relevant_columns <- grep("socioeconomic_status_", colnames(rr_data), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month")
  rr_data = rr_data[,relevant_columns]
  
  rr_data <- rr_data %>% pivot_longer(cols = socioeconomic_status_first_quartile:socioeconomic_status_missing,
                                        names_to = "ses_quartile", 
                                        names_prefix = "socioeconomic_status_",
                                        values_to = "rr")
  rr_data$month <- rr_data$month/10
  rr_data$year <- rr_data$year/10
  
  rr_data$month <- ifelse(rr_data$month ==0, 1,rr_data$month)
  
  rr_data$date <- as.Date(paste(rr_data$year, rr_data$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  rr_data <- rr_data %>%
    mutate(rr = case_when(
      year %in% c(2022, 2023) ~ rr / 100,  # Divide by 100 for years 2022 and 2023
      year == 2024 & month == 1 ~ rr / 100,  # Divide by 100 for February 2024
      TRUE ~ rr  # Keep rr unchanged for all other cases
    ))
  
  
  rr_data <- rr_data %>%
    mutate(ses_quartile = case_when(
      ses_quartile == "first_quartile" ~ "First Quartile",
      ses_quartile == "second_quartile" ~ "Second Quartile",
      ses_quartile == "third_quartile" ~ "Third Quartile",
      ses_quartile == "fourth_quartile" ~ "Fourth Quartile",
      ses_quartile == "missing" ~ "Missing",
      TRUE ~ ses_quartile  # Default case to handle any other values that do not match
    ))
  
  rr_data$rr <- round(rr_data$rr,2) 
  rr_data <- rr_data %>% filter(rr <= 1)
  
  #identify number of colors to use  
  unique_items <- unique(rr_data$ses_quartile)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  
  plot <- plot_ly(
    data = rr_data,
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
