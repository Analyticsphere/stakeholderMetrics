#plot2-- activities by participant
age_plot<- function(age_data=data, selected_hospital = ".", selected_sex=".",
                    selected_age = ".", selected_race = ".", selected_campaign = ".",
                    selected_biospec = "."){
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
  age_data <- age_data %>%
    mutate(biocol_type = case_when(
      d_878865966 == 353358909 & d_167958071 == 353358909 & d_684635302 == 353358909 ~ "All 3 Sample Donations",
      d_878865966 == 353358909 & d_167958071 == 353358909 & d_684635302 == 104430631 ~ "Blood & Urine",
      d_878865966 == 353358909 & d_167958071 == 104430631 & d_684635302 == 353358909 ~ "Blood & Mouthwash",
      d_878865966 == 104430631 & d_167958071 == 353358909 & d_684635302 == 353358909 ~ "Mouthwash & Urine",
      d_878865966 == 353358909 & d_167958071 == 104430631 & d_684635302 == 104430631 ~ "Blood Only",
      d_878865966 == 104430631 & d_167958071 == 353358909 & d_684635302 == 104430631 ~ "Urine Only",
      d_878865966 == 104430631 & d_167958071 == 104430631 & d_684635302 == 353358909 ~ "Mouthwash Only",
      d_878865966 == 104430631 & d_167958071 == 104430631 & d_684635302 == 104430631 ~ "No Samples"
    ))  
  #filter data by hospital if necessary and make label for graph
  if(selected_hospital != "."){
    age_data <- age_data[age_data$d_827220437 == selected_hospital,]
  }
  
  if(selected_sex != "."){
    age_data <- age_data[age_data$sex == selected_sex,]
  }
  if(selected_age != "."){
    age_data <- age_data[age_data$AgeUP_cat == selected_age,]
  }
  if(selected_race != "."){
    age_data <- age_data[age_data$Race_Ethnic == selected_race,]
  }
  if(selected_campaign != "."){
    age_data <- age_data[age_data$active_camptype == selected_campaign,]
  }
  if(selected_biospec != "."){
    age_data <- age_data[age_data$biocol_type == selected_biospec,]
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

