verified_by_ses_mf <- function(data) {
  
  tv_data <- filter(data, population == "total_verified")
  tv_data <- filter(tv_data, site == "Marshfield Clinic Health System")
  relevant_columns <- grep("socioeconomic_status_", colnames(tv_data), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month")
  tv_data = tv_data[,relevant_columns]
  
  
  tv_data <- tv_data %>% pivot_longer(cols = socioeconomic_status_first_quartile:socioeconomic_status_missing,
                                        names_to = "ses_quartile", 
                                        names_prefix = "socioeconomic_status_",
                                        values_to = "total_verified")
 

  #create date variable
  tv_data$date <- as.Date(paste(tv_data$year, tv_data$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  
  
  tv_data <- tv_data %>%
    mutate(ses_quartile = case_when(
      ses_quartile == "first_quartile" ~ "First Quartile",
      ses_quartile == "second_quartile" ~ "Second Quartile",
      ses_quartile == "third_quartile" ~ "Third Quartile",
      ses_quartile == "fourth_quartile" ~ "Fourth Quartile",
      ses_quartile == "missing" ~ "Missing",
      TRUE ~ ses_quartile  # Default case to handle any other values that do not match
    ))
  
  
  #identify number of colors to use  
  unique_items <- unique(tv_data$ses_quartile)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  
  
  plot <- plot_ly(
    data = tv_data,
    x = ~date,
    y = ~total_verified,
    color = ~ses_quartile,
    colors = color_mapping,
    type = 'bar',
    barmode = 'group',
    hoverinfo = 'text+x+y'  # Specifies what info to display on hover
  )
  plot <- plot %>%
    layout(
      title = "Verified Participants by Socioeconomic Status",
      xaxis = list(title = "Date"),
      yaxis = list(title = paste0("Total Verified Participants")),
      legend = list(title = list(text = "SES Quartile")))
  plot
}