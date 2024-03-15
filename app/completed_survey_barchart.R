completed_survey_barchart <- function(survey_data = data) {
  library(plotly)
  
  if (nrow(survey_data) <= 9) {
    return(plotly::plot_ly() %>% 
             layout(title = "Not Enough Data to Display This Chart"))
  } else {
    #creating the dummy variables for each type of collection
    survey_data <- survey_data %>%
      mutate(
        BOH = as.integer(d_949302066 == "231311385"),
        MRE = as.integer(d_536735468 == "231311385"),
        SAS = as.integer(d_976570371 == "231311385"),
        LAW = as.integer(d_663265240 == "231311385"),
        all = as.integer(d_100767870 == "353358909"),
        none = as.integer(BOH == 0 & MRE == 0 & SAS == 0 & LAW ==0 )
      )
    
    # Summarize data-- sum each collection type
    all_data <- summarise(survey_data, 
                          BOH = sum(BOH),
                          MRE = sum(MRE),
                          SAS = sum(SAS),
                          LAW = sum(LAW),
                          All = sum(all),
                          None = sum(none)) %>%
      pivot_longer(cols = everything(), names_to = "SurveyType", values_to = "Count") %>%
      arrange(desc(Count))
    
    #descending order of bars in the chart
    all_data$SurveyType <- factor(all_data$SurveyType, levels = all_data$SurveyType)
    
    
    #identify number of colors to use  
    unique_items <- unique(all_data$SurveyType)
    n_colors <- length(unique_items)+10
    
    # Ensure you have a sufficient number of colors for your activities
    cols <- select_colors(color_palette, n_colors)
    
    # Map colors to activities to ensure consistency
    color_mapping <- setNames(cols, unique_items)
    
    fig <- plot_ly(all_data, x = ~SurveyType, y = ~Count, type = 'bar', 
                   color = ~SurveyType, colors = color_mapping,
                   showlegend = TRUE) %>%
      layout(
        title = "Survey Completion Status By Survey",
        xaxis = list(title = "", showticklabels = FALSE),  # Hide x-axis labels
        yaxis = list(title = "Count"),
        legend = list(title = list(text = 'Survey')),
        margin = list(t = 50),
        annotations = list(
          list(x = 0,
               y = -0.05,
               text = paste0("<b>BOH</b>",
                            ": Background and Overall Health; ",
                            "<b>MRE</b>",
                            ": Medications, Reproductive Health, Exercise and Sleep;",
                            "<b> SAS</b>",
                            ": Smoking, Alcohol, and Sun Exposure;",
                            "<b> LAW</b>",": Where You Live and Work"),
               showarrow = F,
               xref = "paper", # Reference the entire paper area
               yref = "paper", # Reference the entire paper area
               font = list(size = 7),
               xanchor = 'left', align = 'left'))
      )
    
    fig
  }
}
