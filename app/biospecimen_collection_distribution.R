biospecimen_collection_distribution <- function(biocol_data = data){
#load libraries
library(tidyverse) 
library(dplyr) 
library(plotly)
  # Check if the filtered dataset is empty
  if (nrow(biocol_data) <= 9) {
    # Return a message indicating not enough data
    return(plotly::plot_ly() %>% 
             layout(title = "Not Enough Data to Display This Chart"))
  } else {

# Count the occurrences of each biospecimen collection type
biocol_counts <- table(biocol_data$biocol_type)
  
# Convert to a dataframe for Plotly
biocol_df <- as.data.frame(biocol_counts)
names(biocol_df) <- c("CollectionType", "Count")
  
# Create a Plotly pie chart
fig <- plot_ly(biocol_df, labels = ~CollectionType, values = ~Count, type = 'pie',
                 textinfo = 'label+percent',
                 insidetextorientation = 'radial')
  
# Customize the layout
curr.date <- Sys.Date()
  fig <- fig %>% layout(title = paste0("Distribution of Biospecimen Collections as of ", curr.date),
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  # Print the plot
  fig
  }
}
