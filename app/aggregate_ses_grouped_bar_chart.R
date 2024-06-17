aggregate_ses_grouped_bar_chart <- function(data) {
  
  tv_data <- filter(data, population == "total_verified")
  relevant_columns <- grep("socioeconomic_status_", colnames(tv_data), value = TRUE)
  relevant_columns <- c(relevant_columns, "site")
  ses_data = tv_data[,relevant_columns]
  
  long_ses <- ses_data %>% pivot_longer(cols = socioeconomic_status_first_quartile:socioeconomic_status_missing,
                                                    names_to = "ses_quartile", 
                                                    names_prefix = "socioeconomic_status_",
                                                    values_to = "total_verified")
  

  plot <- plot_ly(
    data = long_ses,
    x = ~site,
    y = ~total_verified,
    color = ~ses_quartile,
    type = 'bar',
    barmode = 'group'
  )
  plot <- plot %>%
    layout(
      title = "Site-reported Total Verified Participants by Socioeconomic Status",
      xaxis = list(title = "Site"),
      yaxis = list(title = paste0("Total Verified Participants")),
      legend = list(title = list(text = "SES Quartile")))
  plot
}