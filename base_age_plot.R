#plot2-- activities by participant
age_plot<- function(data){
  library(bigrquery)
  library(plotly)
  library(dplyr)
  library(shinydashboard)
  library(bigrquery)
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
  
#  project <- 'nih-nci-dceg-connect-prod-6d04'
#  dataset <- 'FlatConnect'
#  table   <- 'participants_JP'
  
  
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
  histogram <- histogram %>% layout(title="Ages of Verified Participants", yaxis=list(type='linear'))
histogram
}

