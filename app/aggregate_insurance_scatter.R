aggregate_insurance_scatter <- function(data){
  
rr_data <- filter(data, population == "response_ratio")
relevant_columns <- grep("insurance", colnames(rr_data), value = TRUE)
relevant_columns <- c(relevant_columns, "site")
insurance_data = rr_data[,relevant_columns]

long_insurance <- insurance_data %>% pivot_longer(cols = insurance_private_commercial_employer_or_direct_pay:insurance_unknown,
                                                  names_to = "insurance_type", 
                                                  names_prefix = "insurance",
                                                  values_to = "rr")


long_insurance$insurance_type <- substr(long_insurance$insurance_type, 2, nchar(long_insurance$insurance_type))

plot <- plot_ly(
  data = long_insurance,
  x = ~site,
  y = ~rr,
  color = ~insurance_type,
  type = 'scatter',
  mode = 'markers'
) %>%
  layout(
    title = "Site-reported Response Ratio by Insurance",
    xaxis = list(title = "Site"),
    yaxis = list(title = paste0("response ratio")),
    legend = list(title = list(text = "insurance"))
  )

plot

}
