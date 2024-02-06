#male/female pie chart
sex_distribution <- function(sex_data = data, selected_hospital = ".", selected_sex = ".",
                             selected_age = ".", selected_race = ".", selected_campaign = ".",
                             selected_biospec = ".", selected_surveycomplete = "."){
library(tidyverse) 
library(dplyr) 
library(plotly)
  
  #filter data by hospital if necessary and make label for graph
  if(selected_hospital != "."){
    sex_data <- sex_data[sex_data$d_827220437 == selected_hospital, ]
  }
  if(selected_sex != "."){
    sex_data <- sex_data[sex_data$sex == selected_sex,]
  }
  if(selected_age != "."){
    sex_data <- sex_data[sex_data$AgeUP_cat == selected_age,]
  }
  if(selected_race != "."){
    sex_data <- sex_data[sex_data$Race_Ethnic == selected_race,]
  }
  if(selected_campaign != "."){
    sex_data <- sex_data[sex_data$active_camptype == selected_campaign,]
  }
  if(selected_biospec != "."){
    sex_data <- sex_data[sex_data$biocol_type == selected_biospec,]
  }
  if(selected_surveycomplete != "."){
    sex_data <- sex_data[sex_data$Msrv_complt == selected_surveycomplete,]
  }
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
                 insidetextorientation = 'radial')
  
# Customize the layout
curr.date <- Sys.Date()
fig <- fig %>% layout(title = paste0("Sex of Participants as of ",curr.date),
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  # Print the plot
  fig
  }
}
