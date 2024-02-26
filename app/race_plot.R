race_plot <- function(race_data = data){
  #load libraries
library(tidyverse) 
library(dplyr) 
library(plotly)
  
  # Check if the filtered dataset is empty
  if (nrow(race_data) <= 9) {
    # Return a message indicating not enough data
    return(plotly::plot_ly() %>% 
             layout(title = "Not Enough Data to Display This Chart"))
  } else {
# Assuming the data is already read and cleaned in your R environment
# Replace 'data_cleaned' with the name of your cleaned dataframe
# Count the occurrences of each race/ethnicity
race_counts <- table(race_data$race)
  
# Convert to a dataframe for Plotly
race_df <- as.data.frame(race_counts)
names(race_df) <- c("race", "Count")
  
# Create a Plotly pie chart
fig <- plot_ly(race_df, labels = ~race, values = ~Count, type = 'pie',
               textinfo='label',
               hoverinfo = 'label+percent',
               insidetextorientation = 'radial',
               domain = list(x = c(0.1, 0.9), y = c(0.1, 0.9)))
  
# Customize the layout
curr.date <- Sys.Date()
fig <- fig %>% layout(title = c("Self-Reported Race of Participants Who Completed BOH Section"),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      font = list(family = "Noto Sans"),
                      margin = list(t = 50))
  
  # Print the plot
  fig
  }
}

