hp_aggregate_race_grouped_bar_chart <- function(data){
  
  hp_data <- filter(data, site == "HealthPartners",
                    year < 2024,
                    population == "total_verified")
  relevant_columns <- grep("race_ethnicity_", colnames(hp_data), value = TRUE)
  relevant_columns <- c(relevant_columns,"year", "month")
  hp_data = hp_data[,relevant_columns]
  
  hp_data <- hp_data %>% pivot_longer(cols = race_ethnicity_white_hispanic:race_ethnicity_unknown_ethnicity_unknown,
                                          names_to = "race_ethnicity", 
                                          names_prefix = "race_ethnicity_",
                                          values_to = "tv")
  race_mapping <- list(
    "White" = c("white_hispanic", "white_non_hispanic", "white_unknown_ethnicity"),
    "Black" = c("black_hispanic", "black_non_hispanic", "black_unknown_ethnicity"),
    "Asian" = c("asian_hispanic", "asian_non_hispanic", "asian_unknown_ethnicity"),
    "Native_American" = c("native_american_hispanic", "native_american_non_hispanic", "native_american_unknown_ethnicity"),
    "Native_Hawaiian" = c("native_hawaiian_hispanic", "native_hawaiian_non_hispanic", "native_hawaiian_unknown_ethnicity"),
    "Other" = c("other_hispanic", "other_non_hispanic", "other_unknown_ethnicity"),
    "Unknown" = c("unknown_hispanic", "unknown_non_hispanic", "unknown_ethnicity_unknown")
  )
  
  hp_data$date <- as.Date(paste(hp_data$year, hp_data$month, "01", sep = "-"), "%Y-%m-%d")
  

  # Filter, map, and summarize the data
  filtered_data <- hp_data %>%
    mutate(race_ethnicity = case_when(
      race_ethnicity %in% race_mapping$White ~ "White",
      race_ethnicity %in% race_mapping$Black ~ "Black",
      race_ethnicity %in% race_mapping$Asian ~ "Asian",
      race_ethnicity %in% race_mapping$'Native American' ~ "Native American",
      race_ethnicity %in% race_mapping$'Native Hawaiian' ~ "Native Hawaiian",
      race_ethnicity %in% race_mapping$Other ~ "Other",
      race_ethnicity %in% race_mapping$Unknown ~ "Unknown"
    )) %>%
    group_by(race_ethnicity, date) %>%
    summarize(tv = sum(tv, na.rm = TRUE))
  
  #manually remove june data, its reported in the 100ks
  filtered_data <- filtered_data %>%
    filter(!(date == "2023-06-01"))
  
  
  
  #identify number of colors to use  
  unique_items <- unique(filtered_data$race_ethnicity)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  
  plot <- plot_ly(
    data = filtered_data,
    x = ~date,
    y = ~tv,
    color = ~race_ethnicity,
    colors = color_mapping,
    type = 'bar',
    mode = 'group',
    hoverinfo = 'x+y'  # Specifies what info to display on hover
  ) %>%
    layout(
      title = "Verified Participants by Race",
      xaxis = list(title = "Site"),
      yaxis = list(title = paste0("Participants")),
      legend = list(title = list(text = "Race"))
    )
  
  plot
  
}