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

#identify number of colors to use  
unique_items <- unique(biocol_counts$CollectionType)
n_colors <- length(unique(biocol_counts$CollectionType))

# Ensure you have a sufficient number of colors for your activities
cols <- select_colors(color_palette, n_colors)

# Map colors to activities to ensure consistency
color_mapping <- setNames(cols, unique_items)
  
# Create a Plotly pie chart
fig <- plot_ly(biocol_counts, labels = ~CollectionType, values = ~Count,
               type = 'pie',
               marker = list(colors = color_mapping),
               textinfo='label',
               hoverinfo = 'label+percent',
               insidetextorientation = 'radial',
               domain = list(x = c(0.1, 0.9), y = c(0.1, 0.9)))
  
# Customize the layout
curr.date <- Sys.Date()

fig <- fig %>% layout(
  # title = list(
  #   text = c("Distribution of Biospecimen Collections"),
  #   font = list(family = "Noto Sans")
  # ),
  xaxis = list(showgrid = FALSE,
    zeroline = FALSE,
    showticklabels = FALSE
  ),
  yaxis = list(
    showgrid = FALSE,
    zeroline = FALSE,
    showticklabels = FALSE
  ),
  showlegend = FALSE,
  font = list(family = "Noto Sans"),
  margin = list(b = 50, t = 50, l = -100, r = 50),
  autosize = TRUE
)
  
  # Print the plot
  fig
  }
}
