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





race_plot2<- function(race_data = data, selected_hospital = ".", sex = "."){
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
  #filter by hospital if specified
  hospital_list <- data.frame(hospital_name = c("HealthPartners", "Henry Ford Health System", "Kaiser Permanente Colorado",
                                                "Kaiser Permanente Georgia", "Kaiser Permanente Hawaii", "Kaiser Permanente Northwest",
                                                "Marshfield Clinic Health System","Sanford Health","University of Chicago Medicine",
                                                "National Cancer Institute", "National Cancer Institute", "other"), hospital_cid = c("531629870","548392715","125001209",
                                                                                                                                     "327912200","300267574", "452412599",
                                                                                                                                     "303349821", "657167265", "809703864",
                                                                                                                                     "517700004", "13", "181769837"))
  sex_list <- data.frame(sex_name = c("Male", "Female", "Other"), sex_cid = c("654207589", "536341288", "576796184"))
  
  #filter data by hospital if necessary and make label for graph
  if(selected_hospital != "."){
    race_data <- race_data[race_data$d_827220437 == selected_hospital,]
    hospital_label <- hospital_list[hospital_list$hospital_cid==selected_hospital, 1]
  }
  
  if(sex != "."){
    race_data <- race_data[race_data$sex == sex,]
    sex_label <- sex_list[sex_list$sex_cid==sex, 1]
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

