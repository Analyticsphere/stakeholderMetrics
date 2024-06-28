hp_aggregate_insurance_scatter <- function(data){
  
  hp_data <- filter(data, site == "HealthPartners",
                    year < 2024,
                    population == "response_ratio")
  relevant_columns <- grep("insurance_", colnames(hp_data), value = TRUE)
  relevant_columns <- c(relevant_columns,"year", "month")
  insurance_data = hp_data[,relevant_columns]
  
  long_insurance <- insurance_data %>% pivot_longer(cols = insurance_private_commercial_employer_or_direct_pay:insurance_unknown,
                                                    names_to = "insurance_type", 
                                                    names_prefix = "insurance",
                                                    values_to = "rr")
  
  
  long_insurance$insurance_type <- substr(long_insurance$insurance_type, 2, nchar(long_insurance$insurance_type))
  
  
  long_insurance <- long_insurance %>%
    mutate(insurance_type = case_when(
      insurance_type == "private_commercial_employer_or_direct_pay" ~ "Private, Commercial, Employer or Direct Pay",
      insurance_type == "medicare" ~ "Medicare",
      insurance_type == "medicaid" ~ "Medicaid",
      insurance_type == "va_champ" ~ "VA Champ",
      insurance_type == "tricare_military" ~ "Tricare Military",
      insurance_type == "uninsured" ~ "Uninsured",
      insurance_type == "unknown" ~ "Unknown",
      insurance_type == "workers_comp" ~ "Workers Comp",
      TRUE ~ insurance_type  # Default case to handle any other values that do not match
    ))
  long_insurance$date <- as.Date(paste(long_insurance$year, long_insurance$month, "01", sep = "-"), "%Y-%m-%d")
  
  
  #identify number of colors to use  
  unique_items <- unique(long_insurance$insurance_type)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  
    #manually remove the private commercial etc. this is clearly an error
  long_insurance <- long_insurance %>%
    filter(!(insurance_type == "Private, Commercial, Employer or Direct Pay"
             & date == "2023-05-01"))  %>%
    filter(!(insurance_type == "Medicaid"
             & date == "2023-05-01"))   %>%
    filter(!(insurance_type == "Medicare"
             & date == "2023-05-01"))  
  
  
  plot <- plot_ly(
    data = long_insurance,
    x = ~date,
    y = ~rr,
    color = ~insurance_type,
    colors = color_mapping,
    type = 'scatter',
    mode = 'markers',
    text = ~paste(insurance_type),  # Custom text for hover
    hoverinfo = 'text+x+y'  # Specifies what info to display on hover
  ) %>%
    layout(
      title = "Response Ratio by Insurance",
      xaxis = list(title = "Site"),
      yaxis = list(title = paste0("Response Ratio")),
      legend = list(title = list(text = "Insurance"))
    )
  
  plot
  
}
