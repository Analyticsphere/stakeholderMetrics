aggregate_sex_scatter <- function(data){
  
  rr_data <- filter(data, population == "response_ratio")
  relevant_columns <- grep("sex_", colnames(rr_data), value = TRUE)
  relevant_columns <- c(relevant_columns, "site")
  sex_data = rr_data[,relevant_columns]
  
  long_sex <- sex_data %>% pivot_longer(cols = sex_female:sex_unknown,
                                          names_to = "sex", 
                                          names_prefix = "sex_",
                                          values_to = "rr")
  
  
  
  plot <- plot_ly(
    data = long_sex,
    x = ~site,
    y = ~rr,
    color = ~sex,
    type = 'scatter',
    mode = 'markers'
  ) %>%
    layout(
      title = "Site-reported Response Ratio by Sex",
      xaxis = list(title = "Site"),
      yaxis = list(title = paste0("response ratio")),
      legend = list(title = list(text = "Sex"))
    )
  
  plot
  
}