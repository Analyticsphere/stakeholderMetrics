aggregate_sex_scatter <- function(data){
  
  rr_data <- filter(data, population == "response_ratio")
  relevant_columns <- grep("sex_", colnames(rr_data), value = TRUE)
  relevant_columns <- c(relevant_columns, "site")
  sex_data = rr_data[,relevant_columns]
  
  long_sex <- sex_data %>% pivot_longer(cols = sex_female:sex_unknown,
                                          names_to = "sex", 
                                          names_prefix = "sex_",
                                          values_to = "rr")
  
  long_sex <- long_sex %>%
    mutate(sex = case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      sex == "unknown" ~ "Unknown"))
  
  long_sex$rr <- round(long_sex$rr,2)
  
  #identify number of colors to use  
  unique_items <- unique(long_sex$sex)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  
  
  plot <- plot_ly(
    data = long_sex,
    x = ~site,
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
      xaxis = list(title = "Site"),
      yaxis = list(title = paste0("Response Ratio")),
      legend = list(title = list(text = "Sex"))
    )
  
  plot
  
}