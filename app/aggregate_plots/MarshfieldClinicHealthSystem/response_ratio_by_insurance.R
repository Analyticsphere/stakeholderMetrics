response_ratio_by_insurance <- function(data){
  
  rr_data <- filter(data, population == "response_ratio")
  rr_data <- filter(rr_data, site == "Marshfield Clinic Health System")
  relevant_columns <- grep("insurance", colnames(rr_data), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month")
  rr_data = rr_data[,relevant_columns]
  
  rr_data <- rr_data %>% pivot_longer(cols = insurance_private_commercial_employer_or_direct_pay:insurance_unknown,
                                                    names_to = "insurance_type", 
                                                    names_prefix = "insurance",
                                                    values_to = "rr")
  #include only what i want
  rr_data <- rr_data[,c("year", "month", "rr", "insurance_type")]

  #convert dates
  rr_data$month <- rr_data$month/10
  rr_data$year <- rr_data$year/10
  
  #weirdness with a date
  rr_data$month <- ifelse(rr_data$month == 0, 1, rr_data$month)
  
  #2022, 2023 and 2024 rr are too large, divide by 100
  #except for 2024 month 1
  rr_data <- rr_data %>%
    mutate(rr = case_when(
      year %in% c(2022, 2023) ~ rr / 100,  # Divide by 100 for years 2022 and 2023
      year == 2024 & month == 1 ~ rr / 100,  # Divide by 100 for February 2024
      TRUE ~ rr  # Keep rr unchanged for all other cases
    ))
  
  
  #round and filter rr
  rr_data$rr <- round(rr_data$rr,2)
  rr_data <- rr_data%>% filter(rr <= 1) 
  
  #create date variable
  rr_data$date <- as.Date(paste(rr_data$year, rr_data$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  #edit insurance labels
  rr_data$insurance_type <- substr(rr_data$insurance_type, 2, nchar(rr_data$insurance_type))
  
  rr_data <- rr_data %>%
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
  unique_items <- unique(rr_data$insurance_type)
  n_colors <- length(unique(unique_items))
  
  # Ensure you have a sufficient number of colors for your activities
  cols <- select_colors(color_palette, n_colors)
  
  # Map colors to activities to ensure consistency
  color_mapping <- setNames(cols, unique_items)
  
  plot <- plot_ly(
    data = rr_data,
    x = ~date,
    y = ~rr,
    color = ~insurance_type,
    colors = color_mapping,
    type = 'scatter',
    mode = 'markers',
    hoverinfo = 'text+x+y'  # Specifies what info to display on hover
  ) %>%
    layout(
      title = "Response Ratio by Insurance",
      xaxis = list(title = "Date"),
      yaxis = list(title = paste0("Response Ratio")),
      legend = list(title = list(text = "Insurance"))
    )
  
  plot
  
}
