race_plot <- function(race_data = data, selected_hospital = ".", selected_sex = ".",
                      selected_age = ".", selected_race = ".", selected_campaign = ".",
                      selected_biospec = ".", selected_surveycomplete = "."){
  #load libraries
library(tidyverse) 
library(dplyr) 
library(plotly)

    #filter data by hospital if necessary and make label for graph
  if(selected_hospital != "."){
    race_data <- race_data[race_data$d_827220437 == selected_hospital,]
  }
  
  if(selected_sex != "."){
    race_data <- race_data[race_data$sex == selected_sex,]
  }
  
  if(selected_age != "."){
    race_data <- race_data[race_data$age == selected_age,]
  }
  
  if(selected_race != "."){
    race_data <- race_data[race_data$race == selected_race,]
  }
  if(selected_campaign != "."){
    race_data <- race_data[race_data$active_camptype == selected_campaign,]
  }
  if(selected_biospec != "."){
    race_data <- race_data[race_data$biocol_type == selected_biospec,]
  }
  if(selected_surveycomplete != "."){
    race_data <- race_data[race_data$Msrv_complt == selected_surveycomplete,]
  }
  
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

