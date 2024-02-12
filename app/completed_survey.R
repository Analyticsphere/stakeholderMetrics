

completed_survey <- function(survey_data, selected_hospital = ".", selected_sex = ".",
                             selected_age = ".", selected_race = ".", selected_campaign = ".",
                             selected_biospec = ".", selected_surveycomplete = ".") {
# Load libraries
library(tidyverse) 
library(dplyr) 
library(plotly)
  
  # Filter data based on the provided criteria
  if(selected_hospital != "."){
    survey_data <- survey_data[survey_data$d_827220437 == selected_hospital,]
  }
  if(selected_sex != "."){
    survey_data <- survey_data[survey_data$sex == selected_sex,]
  }
  if(selected_age != "."){
    survey_data <- survey_data[survey_data$age == selected_age,]
  }
  if(selected_race != "."){
    survey_data <- survey_data[survey_data$race == selected_race,]
  }
  if(selected_campaign != "."){
    survey_data <- survey_data[survey_data$active_camptype == selected_campaign,]
  }
  if(selected_biospec != "."){
    survey_data <- survey_data[survey_data$biocol_type == selected_biospec,]
  }
  if(selected_surveycomplete != "."){
    survey_data <- survey_data[survey_data$Msrv_complt == selected_surveycomplete,]
  }
  
  # Check if the filtered dataset is empty
  if (nrow(survey_data) <= 9) {
    # Return a message indicating not enough data
    return(plotly::plot_ly() %>% 
             layout(title = "Not Enough Data to Display This Chart"))
  } else {
# Count the occurrences of each Msrv_complt
msrv_counts <- table(survey_data$Msrv_complt)
    
# Convert to a dataframe for Plotly
msrv_df <- as.data.frame(msrv_counts)
names(msrv_df) <- c("Msrv_complt", "Count")
    
# Create a Plotly pie chart
fig <- plot_ly(msrv_df, labels = ~Msrv_complt, values = ~Count, type = 'pie',
                   textinfo = 'label+percent',
                   insidetextorientation = 'radial')
    
# Customize the layout
curr.date <- Sys.Date()
fig <- fig %>% layout(title = paste0("Survey Completion Status as of ", curr.date),
                          annotations = list(
                            list(x = -0.1, y = -0.1, text = paste0("<b>BOH</b>",": Background and Overall Health; ","<b>MRE</b>",": Medications, Reproductive Health, Exercise and Sleep;","<b> SAS</b>",": Smoking, Alcohol, and Sun Exposure;","<b> LAW</b>",": Where You Live and Work"),
                            showarrow = F, font = list(size = 7),
                            xanchor = 'left', align = 'left')),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    # Print the plot
    fig
  }
}
