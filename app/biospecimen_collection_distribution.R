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
biocol_counts <- as.data.frame(table(biocol_data$biocol_type))
  
# Convert to a dataframe for Plotly
names(biocol_counts) <- c("CollectionType", "Count")
  
# Create a Plotly pie chart
fig <- plot_ly(biocol_counts, labels = ~CollectionType, values = ~Count, type = 'pie',
                 textinfo = 'label+percent',
                 insidetextorientation = 'radial')
  
# Customize the layout
curr.date <- Sys.Date()

fig <- fig %>% layout(
  title = list(
    text = paste0("Distribution of Biospecimen Collections as of ", curr.date),
    font = list(family = "Noto Sans")
  ),
  xaxis = list(showgrid = FALSE,
    zeroline = FALSE,
    showticklabels = FALSE
  ),
  yaxis = list(
    showgrid = FALSE,
    zeroline = FALSE,
    showticklabels = FALSE
  ),
  showlegend = TRUE,
  font = list(family = "Noto Sans")
)
  
  # Print the plot
  fig
  }
}
