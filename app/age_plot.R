#plot2-- activities by participant
age_plot<- function(age_data=data, selected_hospital = ".", selected_sex=".",
                    selected_age = ".", selected_race = ".", selected_campaign = ".",
                    selected_biospec = ".", selected_surveycomplete = "."){
library(tidyverse) 
library(dplyr) 
library(plotly)

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
  if(selected_surveycomplete != "."){
    age_data <- age_data[age_data$Msrv_complt == selected_surveycomplete,]
  }
  # Check if the filtered dataset is empty
  if (nrow(age_data) <= 9) {
    # Return a message indicating not enough data
    return(plotly::plot_ly() %>% 
             layout(title = "Not Enough Data to Display This Chart"))
  } else {

# Create the histogram plot with plotly
  plot <- plot_ly(data = age_data, x = ~AgeUP_cat, type = 'histogram',
                  hoverinfo = 'x+y', hoverlabel = list(bgcolor = 'white'),
                  marker = list(line = list(color = 'black', width = 1)))
  #This variable is from the user profile, therefore its self-reported
  # Update layout
  plot <- plot %>% layout(title = paste0("Self Reported Ages of Verified Participants as of ", Sys.Date()),
                          xaxis = list(title = "Age"),
                          yaxis = list(title = "Count"))
  plot

}
}

