aggregate_sex_grouped_bar_chart <- function(data) {
  
  tv_data <- filter(data, population == "total_verified")
  relevant_columns <- grep("sex_", colnames(tv_data), value = TRUE)
  relevant_columns <- c(relevant_columns, "site")
  sex_data = tv_data[,relevant_columns]
  
  long_sex <- sex_data %>% pivot_longer(cols = sex_female:sex_unknown,
                                                    names_to = "sex", 
                                                    names_prefix = "sex_",
                                                    values_to = "total_verified")
  long_sex <- long_sex %>%
    mutate(sex = case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      sex == "unknown" ~ "Unknown"))
  
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
    y = ~total_verified,
    color = ~sex,
    colors = color_mapping,
    type = 'bar',
    barmode = 'group',
    text = ~paste(sex),  # Custom text for hover
    hoverinfo = 'text+x+y'  # Specifies what info to display on hover
  )
  plot <- plot %>%
    layout(
      title = "Verified Participants by sex",
      xaxis = list(title = "Site"),
      yaxis = list(title = paste0("Total Verified Participants")),
      legend = list(title = list(text = "Sex")))
  plot
}