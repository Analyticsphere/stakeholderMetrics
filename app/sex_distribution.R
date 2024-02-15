#male/female pie chart
sex_distribution <- function(sex_data = data){
library(tidyverse) 
library(dplyr) 
library(plotly)
  
  # Check if the filtered dataset is empty
  if (nrow(sex_data) <= 9) {
    # Return a message indicating not enough data
    return(plotly::plot_ly() %>% 
             layout(title = "Not Enough Data to Display This Chart"))
  } else {
  #keep only the observations with responses
sex_data$sex_factor <- factor(sex_data$sex, levels = c("Female", "Nonbinary", "Male"),
                                labels = c("Female", "Nonbinary", "Male"))

sex_counts <- table(sex_data$sex_factor)

  
# Convert to a dataframe for Plotly
sex_df <- as.data.frame(sex_counts)
names(sex_df) <- c("sex", "Count")
  
  
# Create a Plotly pie chart
fig <- plot_ly(sex_df, labels = ~c("Female", "Nonbinary", "Male"), values = ~Count, type = 'pie',
                 textinfo = 'label+percent',
                 insidetextorientation = 'radial',
                marker = list(colors = c('rgb(42, 114, 165)', 'rgb(28, 94, 134)', 'rgb(49, 159, 190)'))) # Use the specified colors
  
# Customize the layout
curr.date <- Sys.Date()
fig <- fig %>% layout(title = paste0("Site Reported Sex of Participants as of ",curr.date),
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      font = list(family = "Montserrat"))
  
  # Print the plot
  fig
  }
}
