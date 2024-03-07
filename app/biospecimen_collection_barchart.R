biospecimen_collection_barchart <- function(biocol_data = data) {
  # Load libraries
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
    
    fig <- plot_ly(biocol_counts, x = ~CollectionType, y = ~Count, type = 'bar', 
                   color = ~CollectionType, colors = "Paired", showlegend = TRUE) %>%
      layout(
        title = "Distribution of Biospecimen Collections",
        xaxis = list(title = "", showticklabels = FALSE),  # Hide x-axis labels
        yaxis = list(title = "Count"),
        legend = list(title = list(text = 'Collection Type')),
        margin = list(t = 50)
      )
    
    fig
  }
}
