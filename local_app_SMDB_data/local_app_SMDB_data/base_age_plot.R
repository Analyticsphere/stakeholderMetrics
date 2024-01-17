#plot2-- activities by participant
age_plot<- function(age_data=data, selected_hospital = ".", sex="."){
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
  age_data <- expss::apply_labels(age_data,d_827220437 = "Site",#RcrtES_Site_v1r0
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
    age_data <- age_data[age_data$d_827220437 == selected_hospital,]
    hospital_label <- hospital_list[hospital_list$hospital_cid==selected_hospital, 1]
  }
  
  if(sex != "."){
    age_data <- age_data[age_data$sex == sex,]
    hospital_label <- sex_list[sex_list$sex_cid==sex, 1]
  }


  age_data <- data.frame(d_914594314=as.Date(age_data$d_914594314),
                   age = age_data$AgeUP_cat) 
    
  #using the verification date as the censor date
  age_data$age_date <- as.Date(cut(age_data$d_914594314,"week"))
  age_data$a <- 1
  
  agg <- aggregate(age_data$a, list(age_data$age_date, age_data$age), FUN = sum)
  agg$x <- as.character(agg$x)


# Customize the layout
  histogram <- plot_ly(y=agg$x, x=agg$Group.2, histfunc='sum', type = "histogram")
  histogram <- histogram %>% layout(title= list(text=paste0("Ages of Verified Participants as of ", Sys.Date()), font = list(size = 10)), yaxis=list(type='linear'))
histogram
}

