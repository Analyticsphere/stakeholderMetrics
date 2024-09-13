get_active_filters <- function(input) {
  filters <- list(
    site = if (input$siteFilter != ".") input$siteFilter else NULL,
    sex = if (input$sexFilter != ".") input$sexFilter else NULL,
    age = if (input$ageFilter != ".") input$ageFilter else NULL,
    race = if (input$raceFilter != ".") input$raceFilter else NULL,
    campaign = if (input$campaignFilter != ".") input$campaignFilter else NULL,
    biospecimen = if (input$biospecFilter != ".") input$biospecFilter else NULL,
    survey_completion = if (input$surveycompleteFilter != ".") input$surveycompleteFilter else NULL
  )
  
  # Filter out NULL values (filters not applied)
  filters <- filters[!sapply(filters, is.null)]
  
  return(filters)
}
