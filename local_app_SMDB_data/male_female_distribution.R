#male/female pie chart
sex_distribution <- function(sex_data = data, selected_hospital = ".", selected_sex = "."){
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
  
  sex_data <- expss::apply_labels(sex_data,
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
                                   sex = c("Male" = 654207589, "Female" = 536341288, "Other" = 576796184))
  
  #filter data by hospital if necessary and make label for graph
  if(selected_hospital != "."){
    sex_data <- sex_data[sex_data$d_827220437 == selected_hospital, ]
  }
  
  if(selected_sex != "."){
    sex_data <- sex_data[sex_data$sex == selected_sex,]
  }
  
  #keep only the observations with responses
  sex_data <- sex_data[!is.na(sex_data$sex),]
  sex_data$sex_factor <- factor(sex_data$sex, levels = c("536341288", "576796184", "654207589"),
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
