by_sex_scatter <- function(data, population_type, site_name) {
  
  tv_data <- filter(data, population == population_type)
  tv_data <- filter(tv_data, site == site_name)
  relevant_columns <- grep("sex_", colnames(tv_data), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month")
  sex_data = tv_data[,relevant_columns]
  
  value2 = ifelse(population_type =="total_verified", 'total_verified', 'rr')
  
  long_sex <- sex_data %>% pivot_longer(cols = sex_female:sex_unknown,
                                        names_to = "sex", 
                                        names_prefix = "sex_",
                                        values_to = 'value2')
  
  long_sex <- filter(long_sex, month != 6)
  
  long_sex$date <- as.Date(paste(long_sex$year, long_sex$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  
  long_sex <- long_sex %>%
    mutate(sex = case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      sex == "unknown" ~ "Unknown"))
  
  long_sex = long_sex[order(long_sex$date), ]
  
  #identify number of colors to use  
  unique_items <- unique(long_sex$sex)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette2, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  
  plot <- plot_ly(
    data = long_sex,
    x = ~date,
    y = ~value2,
    color = ~sex,
    colors = color_mapping,
    type = 'scatter',
    mode = 'lines + markers',
    hoverinfo = 'x+y') # Specifies what info to display on hover
  
  population_stylized = gsub("_", " ", population_type)
  population_stylized = str_to_title(population_stylized)
  population_title <- ifelse(population_stylized == "Total Verified",
                             paste(population_stylized, "Participants"),
                             population_stylized)
  plot <- plot %>%
    layout(
      title = paste (population_title, "By Sex"),
      xaxis = list(title = "Date"),
      yaxis = list(title = paste (site_name, population_title)),
      legend = list(title = list(text = "Sex")))
  plot
  
}