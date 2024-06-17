aggregate_insurance_grouped_bar_chart <- function(data) {
    
    tv_data <- filter(data, population == "total_verified")
    relevant_columns <- grep("insurance_", colnames(tv_data), value = TRUE)
    relevant_columns <- c(relevant_columns, "site")
    insurance_data = tv_data[,relevant_columns]
    
    long_insurance <- insurance_data %>% pivot_longer(cols = insurance_private_commercial_employer_or_direct_pay:insurance_unknown,
                                                      names_to = "insurance_type", 
                                                      names_prefix = "insurance",
                                                      values_to = "total_verified")
    
    long_insurance$insurance_type <- substr(long_insurance$insurance_type, 2, nchar(long_insurance$insurance_type))
    
  plot <- plot_ly(
    data = long_insurance,
    x = ~site,
    y = ~total_verified,
    color = ~insurance_type,
    type = 'bar',
    barmode = 'group'
  )
  plot <- plot %>%
    layout(
      title = "Site reported total verified by insurnace",
      xaxis = list(title = "Site"),
      yaxis = list(title = paste0("Total Verified Participants")),
      legend = list(title = list(text = "Insurance")))
  plot
}
