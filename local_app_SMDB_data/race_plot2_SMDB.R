race_plot2<- function(race_data = data, selected_hospital = ".", selected_sex = ".", selected_age = ".", selected_race = ".", selected_campaign = "."){
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
  race_data <- expss::apply_labels(race_data,d_827220437 = "Site",#RcrtES_Site_v1r0
                                  d_827220437 = c("HealthPartners"= 531629870,
                                                  "Henry Ford Health System"=548392715,
                                                  "Kaiser Permanente Colorado" = 125001209,
                                                  "Kaiser Permanente Georgia" = 327912200,
                                                  "Kaiser Permanente Hawaii" = 300267574,
                                                  "Kaiser Permanente Northwest" = 452412599,
                                                  "Marshfield Clinic Health System" = 303349821,
                                                  "Sanford Health" = 657167265, 
                                                  "University of Chicago Medicine" = 809703864,
                                                  "National Cancer Institute" = 517700004,
                                                  "National Cancer Institute" = 13,"Other" = 181769837),
                                  sex = "sex", sex = c("Male" = 654207589, "Female" = 536341288, "Other" = 576796184))

    #filter data by hospital if necessary and make label for graph
  if(selected_hospital != "."){
    race_data <- race_data[race_data$d_827220437 == selected_hospital,]
  }
  
  if(selected_sex != "."){
    race_data <- race_data[race_data$sex == selected_sex,]
  }
  
  if(selected_age != "."){
    race_data <- race_data[race_data$AgeUP_cat == selected_age,]
  }
  
  if(selected_race != "."){
    race_data <- race_data[race_data$Race_Ethnic == selected_race,]
  }
  if(selected_campaign != "."){
    race_data <- race_data[race_data$active_camptype == selected_campaign,]
  }
  
  
  # Assuming the data is already read and cleaned in your R environment
  # Replace 'data_cleaned' with the name of your cleaned dataframe
  # Count the occurrences of each race/ethnicity
  race_counts <- table(race_data$Race_Ethnic)
  
  # Convert to a dataframe for Plotly
  race_df <- as.data.frame(race_counts)
  names(race_df) <- c("Race_Ethnic", "Count")
  
  # Create a Plotly pie chart
  fig <- plot_ly(race_df, labels = ~Race_Ethnic, values = ~Count, type = 'pie',
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

