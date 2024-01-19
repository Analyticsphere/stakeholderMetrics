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
  # Check if the filtered dataset is empty
  if (nrow(age_data) == 0) {
    # Return a message indicating not enough data
    return(plotly::plot_ly() %>% 
             layout(title = "Not Enough Data to Display This Chart"))
  } else {

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
}

