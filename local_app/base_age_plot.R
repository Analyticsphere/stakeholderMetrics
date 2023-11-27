#plot2-- activities by participant
age_plot<- function(data, selected_hospital = "All Hospitals"){
  library(bigrquery)
  library(plotly)
  library(dplyr)
  library(glue)
  library(plyr)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(tidyverse)
  library(tidyr)
  library(magrittr)
  library(lubridate)
  library(purrr)
  library(reshape2)
  library(forcats)
  library(ggrepel)
  library(stringr)
  library(expss)
  
#  project <- 'nih-nci-dceg-connect-prod-6d04'
  dataset <- 'FlatConnect'
  table   <- 'participants_JP'
  data <- expss::apply_labels(data,d_827220437 = "Site",#RcrtES_Site_v1r0
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
                                              "National Cancer Institute" = 13,"Other" = 181769837))
  print(paste0("activity plot data dimensions, beginning of file", dim(data)))
  #filter by hospital if specified
  hospital_list <- data.frame(hospital_name = c("HealthPartners", "Henry Ford Health System", "Kaiser Permanente Colorado",
                                                "Kaiser Permanente Georgia", "Kaiser Permanente Hawaii", "Kaiser Permanente Northwest",
                                                "Marshfield Clinic Health System","Sanford Health","University of Chicago Medicine",
                                                "National Cancer Institute", "National Cancer Institute", "other"), hospital_cid = c("531629870","548392715","125001209",
                                                                                                                                     "327912200","300267574", "452412599",
                                                                                                                                     "303349821", "657167265", "809703864",
                                                                                                                                     "517700004", "13", "181769837"))
#filter data by hospital if necessary and make label for graph
    if(selected_hospital != "All Hospitals"){
    data <- data[data$d_827220437 == selected_hospital,]
    hospital_label <- hospital_list[hospital_list$hospital_cid==selected_hospital, 1]
  }
  
  
  
#  sql     <- glue::glue('SELECT state_d_934298480, d_914594314 ',
#                  'FROM `{project}.{dataset}.{table}` ','', paste0("WHERE d_821247024 = '197316935' "))
  
  # Authenticate to BigQuery
#  bigrquery::bq_auth()
  
  # Download data
#  tb      <- bigrquery::bq_project_query(project, query=sql)
#  data    <- bigrquery::bq_table_download(tb, bigint="integer64")

  d2 <- data.frame(d_914594314=as.Date(data$d_914594314),
                   age = case_when(data$state_d_934298480 == 124276120 ~ "40-45",
                                   data$state_d_934298480 == 450985724 ~ "46-50",
                                   data$state_d_934298480 == 363147933 ~ "51-55",
                                   data$state_d_934298480 == 636706443 ~ "56-60",
                                   data$state_d_934298480 == 771230670 ~ "61-65"))
  
  #using the verification date as the censor date
  d2$age_date <- as.Date(cut(d2$d_914594314,"week"))
  d2$a <- 1
  
  agg <- aggregate(d2$a, list(d2$age_date, d2$age), FUN = sum)
  agg$x <- as.character(agg$x)


# Customize the layout
  histogram <- plot_ly(y=agg$x, x=agg$Group.2, histfunc='sum', type = "histogram")
  histogram <- histogram %>% layout(title= list(text=paste0("Ages of Verified Participants as of ", Sys.Date(), "\n hospital filter: ",hospital_label), font = list(size = 10)), yaxis=list(type='linear'))
histogram
}

