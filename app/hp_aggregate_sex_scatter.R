hp_aggregate_sex_scatter <- function(data){
  
  hp_data <- filter(data, site == "HealthPartners",
                    year < 2024,
                    population == "response_ratio")
  relevant_columns <- grep("sex_", colnames(hp_data), value = TRUE)
  relevant_columns <- c(relevant_columns,"year", "month")
  hp_data = hp_data[,relevant_columns]
  
  hp_data <- hp_data %>% pivot_longer(cols = sex_female:sex_unknown,
                                        names_to = "sex", 
                                        names_prefix = "sex_",
                                        values_to = "rr")
  
  hp_data$date <- as.Date(paste(hp_data$year, hp_data$month, "01", sep = "-"),
                          "%Y-%m-%d")
  
  
  hp_data <- hp_data %>%
    mutate(sex = case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      sex == "unknown" ~ "Unknown"))
  
  #manually remove the private commercial etc. this is clearly an error
  hp_data <- hp_data %>%
    filter(rr < 1)
  
  hp_data$rr <- round(hp_data$rr,2)
  
  #identify number of colors to use  
  unique_items <- unique(hp_data$sex)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  
  
  plot <- plot_ly(
    data = hp_data,
    x = ~date,
    y = ~rr,
    color = ~sex,
    colors = color_mapping,
    type = 'scatter',
    mode = 'markers',
    hoverinfo = 'x+y'  # Specifies what info to display on hover
  ) %>%
    layout(
      title = "Response Ratio by Sex",
      xaxis = list(title = "Site"),
      yaxis = list(title = paste0("Response Ratio")),
      legend = list(title = list(text = "Sex"))
    )
  
  plot
  
}