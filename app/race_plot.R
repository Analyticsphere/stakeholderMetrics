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
                 textinfo = 'label+percent',
                 insidetextorientation = 'radial')
  
# Customize the layout
curr.date <- Sys.Date()
fig <- fig %>% layout(title = paste0("Race of Participants Who Completed BOH \n Section of First Survey as of ",curr.date),
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  # Print the plot
  fig
  }
}

