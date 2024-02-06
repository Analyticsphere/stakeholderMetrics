biospecimen_collection_distribution <- function(biocol_data = data, selected_hospital = ".", selected_sex = ".",
                                                 selected_age = ".", selected_race = ".", selected_campaign = ".",
                                                 selected_biospec = ".", selected_surveycomplete = "."){
#load libraries
library(tidyverse) 
library(dplyr) 
library(plotly)
  
  # Filter data based on provided criteria
  if(selected_hospital != "."){
    biocol_data <- biocol_data[biocol_data$d_827220437 == selected_hospital,]
  }
  if(selected_sex != "."){
    biocol_data <- biocol_data[biocol_data$sex == selected_sex,]
  }
  if(selected_age != "."){
    biocol_data <- biocol_data[biocol_data$AgeUP_cat == selected_age,]
  }
  if(selected_race != "."){
    biocol_data <- biocol_data[biocol_data$Race_Ethnic == selected_race,]
  }
  if(selected_campaign != "."){
    biocol_data <- biocol_data[biocol_data$active_camptype == selected_campaign,]
  }
  if(selected_biospec != "."){
    biocol_data <- biocol_data[biocol_data$biocol_type == selected_biospec,]
  }
  if(selected_surveycomplete != "."){
    biocol_data <- biocol_data[biocol_data$Msrv_complt == selected_surveycomplete,]
  }
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
