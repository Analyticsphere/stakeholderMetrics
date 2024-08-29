verified_by_sex_uc <- function(data) {
  
  tv_data <- filter(data, population == "total_verified")
  tv_data <- filter(tv_data, site == "University of Chicago")
  relevant_columns <- grep("sex_", colnames(tv_data), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month")
  tv_data = tv_data[,relevant_columns]
  
  
  tv_data <- tv_data %>% pivot_longer(cols = sex_female:sex_unknown,
                                        names_to = "sex", 
                                        names_prefix = "sex_",
                                        values_to = "total_verified")

  tv_data$date <- as.Date(paste(tv_data$year, tv_data$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  
  tv_data <- tv_data %>%
    mutate(sex = case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      sex == "unknown" ~ "Unknown"))
  
  #identify number of colors to use  
  unique_items <- unique(tv_data$sex)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  
  plot <- plot_ly(
    data = tv_data,
    x = ~date,
    y = ~total_verified,
    color = ~sex,
    colors = color_mapping,
    type = 'bar',
    barmode = 'group',
    hoverinfo = 'x+y'  # Specifies what info to display on hover
  )
  plot <- plot %>%
    layout(
      title = "Verified Participants by Sex",
      xaxis = list(title = "Date"),
      yaxis = list(title = paste0("Total Verified Participants")),
      legend = list(title = list(text = "Sex")))
  plot
}