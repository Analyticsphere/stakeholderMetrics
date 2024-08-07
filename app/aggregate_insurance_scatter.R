aggregate_insurance_scatter <- function(data){
  
rr_data <- filter(data, population == "response_ratio")
relevant_columns <- grep("insurance", colnames(rr_data), value = TRUE)
relevant_columns <- c(relevant_columns, "site")
insurance_data = rr_data[,relevant_columns]

long_insurance <- insurance_data %>% pivot_longer(cols = insurance_private_commercial_employer_or_direct_pay:insurance_unknown,
                                                  names_to = "insurance_type", 
                                                  names_prefix = "insurance",
                                                  values_to = "rr")

long_insurance$rr <- long_insurance$rr*10
long_insurance$rr <- round(long_insurance$rr,2)
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


#identify number of colors to use  
unique_items <- unique(long_insurance$insurance_type)
n_colors <- length(unique(unique_items))

# Ensure you have a sufficient number of colors for your activities
cols <- select_colors(color_palette, n_colors)

# Map colors to activities to ensure consistency
color_mapping <- setNames(cols, unique_items)

plot <- plot_ly(
  data = long_insurance,
  x = ~site,
  y = ~rr,
  color = ~insurance_type,
  colors = color_mapping,
  type = 'scatter',
  mode = 'markers',
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
