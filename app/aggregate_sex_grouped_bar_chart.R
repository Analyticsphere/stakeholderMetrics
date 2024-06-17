aggregate_sex_grouped_bar_chart <- function(data) {
  
  tv_data <- filter(data, population == "total_verified")
  relevant_columns <- grep("sex_", colnames(tv_data), value = TRUE)
  relevant_columns <- c(relevant_columns, "site")
  sex_data = tv_data[,relevant_columns]
  
  long_sex <- sex_data %>% pivot_longer(cols = sex_female:sex_unknown,
                                                    names_to = "sex", 
                                                    names_prefix = "sex_",
                                                    values_to = "total_verified")
  

  plot <- plot_ly(
    data = long_sex,
    x = ~site,
    y = ~total_verified,
    color = ~sex,
    type = 'bar',
    barmode = 'group'
  )
  plot <- plot %>%
    layout(
      title = "Site reported total verified by sex",
      xaxis = list(title = "Site"),
      yaxis = list(title = paste0("Total Verified Participants")),
      legend = list(title = list(text = "Sex")))
  plot
}