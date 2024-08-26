verified_by_ruca_mf <- function(data) {
  
  tv_data <- filter(data, population == "total_verified")
  tv_data <- filter(tv_data, site == "Marshfield Clinic Health System")
  relevant_columns <- grep("urbanicity_", colnames(tv_data), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month")
  tv_data = tv_data[,relevant_columns]
  
  
  tv_data <- tv_data %>% pivot_longer(cols = urbanicity_ruca_code_1:urbanicity_missing,
                                          names_to = "urbanicity_ruca_code", 
                                          names_prefix = "socioeconomic_status_",
                                          values_to = "total_verified")

  #create date variable
  tv_data$date <- as.Date(paste(tv_data$year, tv_data$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  
  tv_data <- tv_data %>%
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
  unique_items <- unique(tv_data$urbanicity_ruca_code)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  
  plot <- plot_ly(
    data = tv_data,
    x = ~date,
    y = ~total_verified,
    color = ~urbanicity_ruca_code,
    colors = color_mapping,
    type = 'bar',
    barmode = 'group',
    hoverinfo = 'text+x+y'  # Specifies what info to display on hover
  )
  plot <- plot %>%
    layout(
      title = "Verified Participants by RUCA Code",
      xaxis = list(title = "Date"),
      yaxis = list(title = paste0("Total Verified Participants")),
      legend = list(title = list(text = "RUCA Code")))
  plot
}