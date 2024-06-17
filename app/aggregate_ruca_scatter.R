aggregate_ruca_scatter <- function(data){
  
  rr_data <- filter(data, population == "response_ratio")
  relevant_columns <- grep("urbanicity_", colnames(rr_data), value = TRUE)
  relevant_columns <- c(relevant_columns, "site")
  ruca_data = rr_data[,relevant_columns]
  
  long_ruca <- ruca_data %>% pivot_longer(cols = urbanicity_ruca_code_1:urbanicity_missing,
                                          names_to = "urbanicity_ruca_code", 
                                          names_prefix = "socioeconomic_status_",
                                          values_to = "rr")
  
  

  plot <- plot_ly(
    data = long_ruca,
    x = ~site,
    y = ~rr,
    color = ~urbanicity_ruca_code,
    type = 'scatter',
    mode = 'markers'
  ) %>%
    layout(
      title = "Site-reported Response Ratio by Ruca",
      xaxis = list(title = "Site"),
      yaxis = list(title = paste0("response ratio")),
      legend = list(title = list(text = "RUCA"))
    )
  
  plot
  
}