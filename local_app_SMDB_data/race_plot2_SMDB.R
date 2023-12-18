race_plot<- function(){
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

  project <- "nih-nci-dceg-connect-prod-6d04"
  query <- glue("SELECT * FROM `nih-nci-dceg-connect-prod-6d04.StakeHolderMetrics_tmp.figure3_race`")
  data <- bq_table_download(bq_project_query(project, query = query), bigint = "integer64")
  data <- data[complete.cases(data),]

  
  # Assuming the data is already read and cleaned in your R environment
  # Replace 'data_cleaned' with the name of your cleaned dataframe
  # Count the occurrences of each race/ethnicity
  race_counts <- table(data$Race_Ethnic)
  
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
