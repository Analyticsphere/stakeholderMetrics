aggregate_ruca_grouped_bar_chart <- function(data) {
  
  tv_data <- filter(data, population == "total_verified")
  relevant_columns <- grep("urbanicity_", colnames(tv_data), value = TRUE)
  relevant_columns <- c(relevant_columns, "site")
  ruca_data = tv_data[,relevant_columns]
  
  long_ruca <- ruca_data %>% pivot_longer(cols = urbanicity_ruca_code_1:urbanicity_missing,
                                        names_to = "urbanicity_ruca_code", 
                                        names_prefix = "socioeconomic_status_",
                                        values_to = "total_verified")
  
  
  plot <- plot_ly(
    data = long_ruca,
    x = ~site,
    y = ~total_verified,
    color = ~urbanicity_ruca_code,
    type = 'bar',
    barmode = 'group'
  )
  plot <- plot %>%
    layout(
      title = "Site-reported Total Verified Participants by RUCA Code",
      xaxis = list(title = "Site"),
      yaxis = list(title = paste0("Total Verified Participants")),
      legend = list(title = list(text = "RUCA Code")))
  plot
}