by_race_scatter <- function(data, population_type, site_name){
  # Write new dataframes to pull from
  
  data_sub <- filter(data, population == population_type)
  data_sub <- filter(data_sub, site == site_name)
  relevant_columns <- grep("race_ethnicity_", colnames(data_sub), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month")
  race_data = data_sub[,relevant_columns]
  
  
  long_race <- race_data %>% pivot_longer(cols = race_ethnicity_white_hispanic:race_ethnicity_unknown_unknown_ethnicity,
                                          names_to = "race_ethnicity", 
                                          names_prefix = "race_ethnicity_",
                                          values_to = "value2")
  
  long_race <- long_race[,c("year", "month", "race_ethnicity", "value2")]
  
  #create date variable
  long_race$date <- as.Date(paste(long_race$year, long_race$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  
  race_mapping <- list(
    "White" = c("white_hispanic", "white_non_hispanic", "white_unknown_ethnicity"),
    "Black" = c("black_hispanic", "black_non_hispanic", "black_unknown_ethnicity"),
    "Asian" = c("asian_hispanic", "asian_non_hispanic", "asian_unknown_ethnicity"),
    "Native_American" = c("native_american_hispanic", "native_american_non_hispanic", "native_american_unknown_ethnicity"),
    "Native_Hawaiian" = c("native_hawaiian_hispanic", "native_hawaiian_non_hispanic", "native_hawaiian_unknown_ethnicity"),
    "Other" = c("other_hispanic", "other_non_hispanic", "other_ethnicity_unknown"),
    "Unknown" = c("unknown_hispanic", "unknown_non_hispanic", "unknown_unknown_ethnicity")
  )
  
  # Filter, map, and summarize the data
  filtered_data <- long_race %>%
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
    summarize(value2 = sum(value2, na.rm = TRUE)) 
  
  #identify number of colors to use  
  unique_items <- unique(filtered_data$race_ethnicity)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette2, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  
  plot <- plot_ly(
    data = filtered_data,
    x = ~date,
    y = ~value2,
    color = ~race_ethnicity,
    colors = color_mapping,
    type = 'scatter',
    mode = 'markers + lines',
    text = ~paste(race_ethnicity),  # Custom text for hover
    hoverinfo = 'text+x+y'  # Specifies what info to display on hover
  )
  
  # Rewrite titles
  population_stylized = gsub("_", " ", population_type)
  population_stylized = str_to_title(population_stylized)
  population_title <- ifelse(population_stylized == "Total Verified",
                             paste(population_stylized, "Participants"),
                             population_stylized)
  # Apply titles
  plot <- plot %>%
    layout(
      title = paste (population_title, "By Race"),
      xaxis = list(title = "Date"),
      yaxis = list(title = paste (site_name, population_title)),
      legend = list(title = list(text = "Race")))
  
  
  plot
  
}

