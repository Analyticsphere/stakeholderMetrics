response_ratio_by_ruca_mf <- function(data){
  
  rr_data <- filter(data, population == "response_ratio")
  rr_data <- filter(rr_data, site == "Marshfield Clinic Health System")
  relevant_columns <- grep("urbanicity_", colnames(rr_data), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month")
  rr_data = rr_data[,relevant_columns]
  
  
  rr_data <- rr_data %>% pivot_longer(cols = urbanicity_ruca_code_1:urbanicity_missing,
                                          names_to = "urbanicity_ruca_code", 
                                          names_prefix = "socioeconomic_status_",
                                          values_to = "rr")
  rr_data$month <- rr_data$month/10
  rr_data$year <- rr_data$year/10
  
  rr_data <- rr_data %>%
    mutate(rr = case_when(
      year %in% c(2022,2023) ~ rr / 100,  # Divide by 100 for year 2024
      year == 2024 & month ==1 ~ rr / 100,  # Divide by 100 for year 2024
      TRUE ~ rr  # Keep rr unchanged for all other cases
    ))
  
  
  #fix month, rr and year
  rr_data$rr <- round(rr_data$rr,2)
  rr_data <- rr_data %>% filter(rr <=1)

  rr_data$month <- ifelse(rr_data$month ==0, 1, rr_data$month)
  
  rr_data$date <- as.Date(paste(rr_data$year, rr_data$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  
  
  rr_data <- rr_data %>%
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
  
  

  
  #identify number of colors to use  
  unique_items <- unique(rr_data$urbanicity_ruca_code)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  
  
  plot <- plot_ly(
    data = rr_data,
    x = ~date,
    y = ~rr,
    color = ~urbanicity_ruca_code,
    colors = color_mapping,
    type = 'scatter',
    mode = 'markers',
    text = ~paste(urbanicity_ruca_code),  # Custom text for hover
    hoverinfo = 'text+x+y'  # Specifies what info to display on hover
  ) %>%
    layout(
      title = "Response Ratio by Urbanicity",
      xaxis = list(title = "Date"),
      yaxis = list(title = paste0("Response Ratio")),
      legend = list(title = list(text = "RUCA code"))
    )
  
  plot
  
}