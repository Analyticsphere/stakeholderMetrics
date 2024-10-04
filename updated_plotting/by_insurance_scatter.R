by_insurance_scatter <- function(data, population_type, site_name){
  # Write new dataframes to pull from
  
  data_sub <- filter(data, population == population_type)
  data_sub <- filter(data_sub, site == site_name)
  relevant_columns <- grep("insurance", colnames(data_sub), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month")
  insurance_data = data_sub[,relevant_columns]
  
  # Creating variable to use as Y in graphs
  value2 = ifelse(population_type =="total_verified", 'total_verified', 'rr')
  
  # Pivot table
  long_insurance <- insurance_data %>% pivot_longer(cols = insurance_private_commercial_employer_or_direct_pay:insurance_unknown,
                                                    names_to = "insurance_type", 
                                                    names_prefix = "insurance",
                                                    values_to = "value2")
  long_insurance <- long_insurance[,c("year", "month", "value2", "insurance_type")]
  
  long_insurance <- filter(long_insurance, month != 6)
  long_insurance$date <- as.Date(paste(long_insurance$year, long_insurance$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  
  long_insurance$insurance_type <- substr(long_insurance$insurance_type, 2, nchar(long_insurance$insurance_type))
  
  # Rewrite legend titles
  long_insurance <- long_insurance %>%
    mutate(insurance_type = case_when(
      insurance_type == "private_commercial_employer_or_direct_pay" ~ "Private, Commercial, Employer or Direct Pay",
      insurance_type == "medicare" ~ "Medicare",
      insurance_type == "medicaid" ~ "Medicaid",
      insurance_type == "va_champ" ~ "VA Champ",
      insurance_type == "va_and_champ_va" ~ "VA Champ",
      insurance_type == "tricare_military" ~ "Tricare Military",
      insurance_type == "tricare" ~ "Tricare Military",
      insurance_type == "uninsured" ~ "Uninsured",
      insurance_type == "unknown" ~ "Unknown",
      insurance_type == "workers_comp" ~ "Workers Comp",
      insurance_type == "other_public" ~ "Other Public Insurance",
      TRUE ~ insurance_type  # Default case to handle any other values that do not match
    ))
  
  
  #identify number of colors to use  
  unique_items <- unique(long_insurance$insurance_type)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette2, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  
  plot <- plot_ly(
    data = long_insurance,
    x = ~date,
    y = ~value2,
    color = ~insurance_type,
    colors = color_mapping,
    type = 'scatter',
    mode = 'lines + markers',
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
      title = paste (population_title, "By Insurance"),
      xaxis = list(title = "Date"),
      yaxis = list(title = paste (site_name, population_title)),
      legend = list(title = list(text = "Insurance")))
  
  
  plot
  
}
