

completed_survey <- function(survey_data = data) {
# Load libraries
library(tidyverse) 
library(dplyr) 
library(plotly)
  
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

#identify number of colors to use  
unique_items <- unique(msrv_df$Msrv_complt)
n_colors <- length(unique(msrv_df$Msrv_complt))+15

# Ensure you have a sufficient number of colors for your activities
cols <- select_colors(color_palette, n_colors)

# Map colors to activities to ensure consistency
color_mapping <- setNames(cols, unique_items)
    
# Create a Plotly pie chart
fig <- plot_ly(msrv_df, labels = ~Msrv_complt, values = ~Count, type = 'pie',
               marker = list(colors = color_mapping),
               hoverinfo = 'label+percent',
               textinfo ='label',
               insidetextorientation = 'radial',
               domain = list(x = c(0.1, 0.9), y = c(0.1, 0.9)),
               showlegend = FALSE)
    
# Customize the layout
curr.date <- Sys.Date()
fig <- fig %>%
  layout(autosize = TRUE
         #title = c("Survey Completion Status"),
         # annotations = list(
         # list(x = 0,
         #      y = -0.05,
         #      text = paste0("<b>BOH</b>",
         #                    ": Background and Overall Health; ",
         #                      "<b>MRE</b>",
         #                      ": Medications, Reproductive Health, Exercise and Sleep;",
         #                      "<b> SAS</b>",
         #                      ": Smoking, Alcohol, and Sun Exposure;",
         #                      "<b> LAW</b>",": Where You Live and Work"),
         #        showarrow = F,
         #        xref = "paper", # Reference the entire paper area
         #        yref = "paper", # Reference the entire paper area
         #        font = list(size = 7),
         #        xanchor = 'left', align = 'left'))
         )
    
    # Print the plot
    fig
  }
}
