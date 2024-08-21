response_ratio_by_sex <- function(data){
  
  rr_data <- filter(data, population == "response_ratio")
  rr_data <- filter(rr_data, site == "Sanford Health")
  relevant_columns <- grep("sex_", colnames(rr_data), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month")
  rr_data = rr_data[,relevant_columns]
  
  rr_data <- rr_data %>% pivot_longer(cols = sex_female:sex_unknown,
                                        names_to = "sex", 
                                        names_prefix = "sex_",
                                        values_to = "rr")
  rr_data$year <- rr_data$year/10
  rr_data$month <- rr_data$month/10
  rr_data$rr <- rr_data$rr/100
  
  
  rr_data$date <- as.Date(paste(rr_data$year, rr_data$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  
  rr_data <- rr_data %>%
    mutate(sex = case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      sex == "unknown" ~ "Unknown"))
  
  rr_data$rr <- round(rr_data$rr,2)
  rr_data <- rr_data %>% filter(rr <=1)
  
  #identify number of colors to use  
  unique_items <- unique(rr_data$sex)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  
  
  plot <- plot_ly(
    data = rr_data,
    x = ~date,
    y = ~rr,
    color = ~sex,
    colors = color_mapping,
    type = 'scatter',
    mode = 'markers',
    text = ~paste(sex),  # Custom text for hover
    hoverinfo = 'text+x+y'  # Specifies what info to display on hover
  ) %>%
    layout(
      title = "Response Ratio by Sex",
      xaxis = list(title = "Date"),
      yaxis = list(title = paste0("Response Ratio")),
      legend = list(title = list(text = "Sex"))
    )
  
  plot
  
}