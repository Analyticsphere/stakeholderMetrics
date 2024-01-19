#male/female pie chart
sex_distribution <- function(sex_data = data, selected_hospital = ".", selected_sex = ".",
                             selected_age = ".", selected_race = ".", selected_campaign = ".",
                             selected_biospec = "."){
  #load libraries
  library(bigrquery)
  library(foreach)
  library(stringr)
  library(magrittr)
  library(arsenal)
  library(gtsummary)
  library(rio)
  library(ggplot2)
  library(gridExtra)
  library(scales)
  library(gt)
  library(tinytex)
  library(data.table) 
  library(tidyverse) 
  library(dplyr) 
  library(reshape) 
  library(listr) 
  library(sqldf) 
  library(lubridate)
  library(stringr)
  library(RColorBrewer)
  library(ggrepel)
  library(DBI)
  library(RSQLite)
  library(glue)
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
  # Check if the filtered dataset is empty
  if (nrow(sex_data) == 0) {
    # Return a message indicating not enough data
    return(plotly::plot_ly() %>% 
             layout(title = "Not Enough Data to Display This Chart"))
  } else {
  #keep only the observations with responses
  #sex_data <- sex_data[!is.na(sex_data$sex),]
  sex_data$sex_factor <- factor(sex_data$sex, levels = c("Female", "Other", "Male"),
                                labels = c("Female", "Other", "Male"))
  
  
  # Assuming the data is already read and cleaned in your R environment
  # Replace 'data_cleaned' with the name of your cleaned dataframe
  # Count the occurrences of each race/ethnicity
  sex_counts <- table(sex_data$sex_factor)

  
  # Convert to a dataframe for Plotly
  sex_df <- as.data.frame(sex_counts)
  names(sex_df) <- c("sex", "Count")
  
  
  # Create a Plotly pie chart
  fig <- plot_ly(sex_df, labels = ~c("Female", "Other", "Male"), values = ~Count, type = 'pie',
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
