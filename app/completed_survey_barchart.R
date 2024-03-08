completed_survey_barchart <- function(survey_data = data) {
  library(plotly)
  
  if (nrow(survey_data) <= 9) {
    return(plotly::plot_ly() %>% 
             layout(title = "Not Enough Data to Display This Chart"))
  } else {
    msrv_counts <- table(survey_data$Msrv_complt)
    msrv_df <- as.data.frame(msrv_counts)
    names(msrv_df) <- c("Msrv_complt", "Count")
    
    fig <- plot_ly(msrv_df, x = ~Msrv_complt, y = ~Count, type = 'bar', 
                   color = ~Msrv_complt, colors = "Paired", showlegend = TRUE) %>%
      layout(
        title = "Survey Completion Status",
        xaxis = list(title = "", showticklabels = FALSE),  # Hide x-axis labels
        yaxis = list(title = "Count"),
        legend = list(title = list(text = 'Survey Completion')),
        margin = list(t = 50),
        annotations = list(
          list(x = 0, y = -0.15, text = paste0("<b>BOH</b>",": Background and Overall Health; ","<b>MRE</b>",": Medications, Reproductive Health, Exercise and Sleep;","<b> SAS</b>",": Smoking, Alcohol, and Sun Exposure;","<b> LAW</b>",": Where You Live and Work"),
               showarrow = F, font = list(size = 7),
               xanchor = 'left', align = 'left'))
      )
    
    fig
  }
}
