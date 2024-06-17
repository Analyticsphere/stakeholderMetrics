aggregate_ses_scatter <- function(data){
  
  tv_data <- filter(data, population == "total_verified")
  relevant_columns <- grep("socioeconomic_status_", colnames(tv_data), value = TRUE)
  relevant_columns <- c(relevant_columns, "site")
  ses_data = tv_data[,relevant_columns]
  
  long_ses <- ses_data %>% pivot_longer(cols = socioeconomic_status_first_quartile:socioeconomic_status_missing,
                                        names_to = "ses_quartile", 
                                        names_prefix = "socioeconomic_status_",
                                        values_to = "total_verified")
  
  long_ses <- long_ses %>%
    mutate(ses_quartile = case_when(
      ses_quartile == "first_quartile" ~ "First Quartile",
      ses_quartile == "second_quartile" ~ "Second Quartile",
      ses_quartile == "third_quartile" ~ "Third Quartile",
      ses_quartile == "fourth_quartile" ~ "Fourth Quartile",
      ses_quartile == "missing" ~ "Missing",
      TRUE ~ ses_quartile  # Default case to handle any other values that do not match
    ))
  
  
  #identify number of colors to use  
  unique_items <- unique(long_ses$ses_quartile)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)

  plot <- plot_ly(
    data = long_ses,
    x = ~site,
    y = ~total_verified,
    color = ~ses_quartile,
    colors = color_mapping,
    type = 'scatter',
    mode = 'markers',
    text = ~paste(ses_quartile),  # Custom text for hover
    hoverinfo = 'text+x+y'  # Specifies what info to display on hover
  ) %>%
    layout(
      title = "Response Ratio by Socioeconomic Status",
      xaxis = list(title = "Site"),
      yaxis = list(title = paste0("Response Ratio")),
      legend = list(title = list(text = "SES Quartile"))
    )
  
  plot
  
}
