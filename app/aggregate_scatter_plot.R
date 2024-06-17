aggregate_scatter_plot <- function(data, category, title) {
  if(category == "Race"){
    race_mapping <- list(
      "White" = c("Race/Ethnicity - White, Hispanic", "Race/Ethnicity - White, Non-Hispanic", "Race/Ethnicity - White, Unknown ethnicity"),
      "Black" = c("Race/Ethnicity - Black, Hispanic", "Race/Ethnicity - Black, Non-Hispanic", "Race/Ethnicity - Black, Unknown ethnicity"),
      "Asian" = c("Race/Ethnicity - Asian, Hispanic", "Race/Ethnicity - Asian, Non-Hispanic", "Race/Ethnicity - Asian, Unknown ethnicity"),
      "Native American" = c("Race/Ethnicity - Native American, Hispanic", "Race/Ethnicity - Native American, Non-Hispanic", "Race/Ethnicity - Native American, Unknown ethnicity"),
      "Native Hawaiian" = c("Race/Ethnicity - Native Hawaiian, Hispanic", "Race/Ethnicity - Native Hawaiian, Non-Hispanic", "Race/Ethnicity - Native Hawaiian, Unknown ethnicity"),
      "Other" = c("Race/Ethnicity - Other, Hispanic", "Race/Ethnicity - Other, Non-Hispanic", "Race/Ethnicity - Other, Unknown"),
      "Unknown" = c("Race/Ethnicity - Unknown, Hispanic", "Race/Ethnicity - Unknown, Non-Hispanic", "Race/Ethnicity - Unknown, ethnicity unknown")
    )
    
    # Filter, map, and summarize the data
    filtered_data <- data %>%
      filter(row_description %in% unlist(race_mapping)) %>%
      mutate(row_description = case_when(
        row_description %in% race_mapping$White ~ "White",
        row_description %in% race_mapping$Black ~ "Black",
        row_description %in% race_mapping$Asian ~ "Asian",
        row_description %in% race_mapping$'Native American' ~ "Native American",
        row_description %in% race_mapping$'Native Hawaiian' ~ "Native Hawaiian",
        row_description %in% race_mapping$Other ~ "Other",
        row_description %in% race_mapping$Unknown ~ "Unknown"
      )) %>%
      group_by(site, row_description) %>%
      summarize(response_ratio = sum(response_ratio, na.rm = TRUE))
  }else if(category == "Sex"){
    filtered_data <- data %>%
      filter(row_description %in% c("Sex - Female", "Sex - Male", "Sex - Unknown")) %>%
      mutate(row_description = case_when(
        row_description == "Sex - Female" ~ "Female",
        row_description == "Sex - Male" ~ "Male",
        row_description == "Sex - Unknown" ~ "Unknown"
      )) %>%
      group_by(site, row_description) %>%
      summarize(response_ratio = sum(response_ratio, na.rm = TRUE))
  }else{
    filtered_data <- data %>%
      filter(str_detect(row_description, category)) %>%
      group_by(site, row_description) %>%
      summarize(response_ratio = sum(response_ratio, na.rm = TRUE))}
  
  plot <- plot_ly(
    data = filtered_data,
    x = ~site,
    y = ~response_ratio,
    color = ~row_description,
    type = 'scatter',
    mode = 'markers'
  ) %>%
    layout(
      title = "Insurance of Recruited Participants",
      xaxis = list(title = "Site"),
      yaxis = list(title = paste0("Site-reported Response Ratio \n By Insurance")),
      legend = list(title = list(text = "Insurace"))
    )
  
  plot
}

