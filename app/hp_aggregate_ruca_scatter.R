hp_aggregate_ruca_scatter <- function(data){
  
  hp_data <- filter(data, site == "HealthPartners",
                    year < 2024,
                    population == "response_ratio")
  relevant_columns <- grep("urbanicity_", colnames(hp_data), value = TRUE)
  relevant_columns <- c(relevant_columns,"year", "month")
  hp_data = hp_data[,relevant_columns]
  
  hp_data <- hp_data %>% pivot_longer(cols = urbanicity_ruca_code_1:urbanicity_missing,
                                          names_to = "urbanicity_ruca_code", 
                                          names_prefix = "socioeconomic_status_",
                                          values_to = "rr")
  
  hp_data <- hp_data %>%
    mutate(urbanicity_ruca_code = case_when(
      urbanicity_ruca_code == "urbanicity_ruca_code_1" ~ "Code 1",
      urbanicity_ruca_code == "urbanicity_ruca_code_2" ~ "Code 2",
      urbanicity_ruca_code == "urbanicity_ruca_code_3" ~ "Code 3",
      urbanicity_ruca_code == "urbanicity_ruca_code_4" ~ "Code 4",
      urbanicity_ruca_code == "urbanicity_ruca_code_5" ~ "Code 5",
      urbanicity_ruca_code == "urbanicity_ruca_code_6" ~ "Code 6",
      urbanicity_ruca_code == "urbanicity_ruca_code_7" ~ "Code 7",
      urbanicity_ruca_code == "urbanicity_ruca_code_8" ~ "Code 8",
      urbanicity_ruca_code == "urbanicity_ruca_code_9" ~ "Code 9",
      urbanicity_ruca_code == "urbanicity_ruca_code_10" ~ "Code 10",
      urbanicity_ruca_code == "urbanicity_missing" ~ "Code Unknown",
      TRUE ~ urbanicity_ruca_code  # Default case to handle any other values that do not match
    ))
  
  hp_data$date <- as.Date(paste(hp_data$year, hp_data$month, "01", sep = "-"), "%Y-%m-%d")
  #manually remove the private commercial etc. this is clearly an error
  hp_data <- hp_data %>%
    filter(rr<1)
  
  hp_data$rr <- round(hp_data$rr,2)
  
  #identify number of colors to use  
  unique_items <- unique(hp_data$urbanicity_ruca_code)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  
  
  plot <- plot_ly(
    data = hp_data,
    x = ~date,
    y = ~rr,
    color = ~urbanicity_ruca_code,
    colors = color_mapping,
    type = 'scatter',
    mode = 'markers',
    hoverinfo = 'x+y'  # Specifies what info to display on hover
  ) %>%
    layout(
      title = "Response Ratio by Urbanicity",
      xaxis = list(title = "Site"),
      yaxis = list(title = paste0("Response Ratio")),
      legend = list(title = list(text = "RUCA code"))
    )
  
  plot
  
}