verified_by_insurance<- function(data) {
  
  tv_data <- filter(data, population == "total_verified")
  tv_data <- filter(tv_data, site == "HealthPartners")
  relevant_columns <- grep("insurance_", colnames(tv_data), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month")
  insurance_data = tv_data[,relevant_columns]
  
  
  long_insurance <- insurance_data %>% pivot_longer(cols = insurance_private_commercial_employer_or_direct_pay:insurance_unknown,
                                                    names_to = "insurance_type", 
                                                    names_prefix = "insurance",
                                                    values_to = "total_verified")
  
  long_insurance$insurance_type <- substr(long_insurance$insurance_type, 2, nchar(long_insurance$insurance_type))
  
  long_insurance <- filter(long_insurance, month != 6)
  
  #create date variable
  long_insurance$date <- as.Date(paste(long_insurance$year, long_insurance$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  
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
  
  
  #identify number of colors to use  
  unique_items <- unique(long_insurance$insurance_type)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  plot <- plot_ly(
    data = long_insurance,
    x = ~date,
    y = ~total_verified,
    color = ~insurance_type,
    colors = color_mapping,
    type = 'bar',
    barmode = 'group',
    hoverinfo = 'x+y'  # Specifies what info to display on hover
  )
  plot <- plot %>%
    layout(
      title = "Verified Participants by Insurance",
      xaxis = list(title = "Date"),
      yaxis = list(title = paste0("Total Verified Participants")),
      legend = list(title = list(text = "Insurance")))
  plot
}
