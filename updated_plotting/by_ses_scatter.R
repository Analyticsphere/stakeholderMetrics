by_ses_scatter <- function(data, population_type, site_name) {
  # Write new dataframes to pull from
  data_sub <- filter(data, population == population_type)
  data_sub <- filter(data_sub, site == site_name)
  
  # Get the relevant columns for SES
  relevant_columns <- grep("socioeconomic_status_", colnames(data_sub), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month")
  ses_data <- data_sub[, relevant_columns]
  
  # Pivot the table and store the numeric values in the "value" column
  long_ses <- ses_data %>% 
    pivot_longer(cols = socioeconomic_status_first_quartile:socioeconomic_status_missing,
                 names_to = "ses_quartile", 
                 names_prefix = "socioeconomic_status_",
                 values_to = "value")  # This column now contains the numeric values
  
  long_ses <- filter(long_ses, month != 6)
  
  # Create date variable
  long_ses$date <- as.Date(paste(long_ses$year, long_ses$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  # Rewrite legend titles
  long_ses <- long_ses %>%
    mutate(ses_quartile = case_when(
      ses_quartile == "first_quartile" ~ "First Quartile",
      ses_quartile == "second_quartile" ~ "Second Quartile",
      ses_quartile == "third_quartile" ~ "Third Quartile",
      ses_quartile == "fourth_quartile" ~ "Fourth Quartile",
      ses_quartile == "missing" ~ "Missing",
      TRUE ~ ses_quartile  # Default case to handle any other values that do not match
    ))
  
  # Identify number of colors to use  
  unique_items <- unique(long_ses$ses_quartile)
  n_colors <- length(unique_items)
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette2, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  
  # Create the plot with actual numeric values for the y-axis
  plot <- plot_ly(
    data = long_ses,
    x = ~date,
    y = ~value,  # Now using the actual numeric values for y
    color = ~ses_quartile,
    colors = color_mapping,
    type = 'scatter',
    mode = 'markers+lines',
    hoverinfo = 'x+y'  # Specifies what info to display on hover
  )
  
  # Rewrite titles
  population_stylized <- gsub("_", " ", population_type)
  population_stylized <- str_to_title(population_stylized)
  population_title <- ifelse(population_stylized == "Total Verified",
                             paste(population_stylized, "Participants"),
                             population_stylized)
  
  # Apply titles
  plot <- plot %>%
    layout(
      title = paste(site_name, population_title, "By SES Quartile"),
      xaxis = list(title = "Date"),
      yaxis = list(title = paste(site_name, population_title)),
      legend = list(title = list(text = "SES Quartile"))
    )
  
  return(plot)
}
