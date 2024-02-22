age_plot<- function(age_data=data){
library(tidyverse) 
library(dplyr) 
library(plotly)

  # Check if the filtered dataset is empty
  if (nrow(age_data) <= 9) {
    # Return a message indicating not enough data
    return(plotly::plot_ly() %>% 
             layout(title = "Not Enough Data to Display This Chart"))
  } else {

    if("age" %in% colnames(age_data)){
    
# Create the histogram plot with plotly
  plot <- plot_ly(data = age_data, x = ~age, type = 'histogram',
                  hoverinfo = 'x+y', hoverlabel = list(bgcolor = 'white'),
                  marker = list(color = 'rgb(42, 114, 165)', line = list(color = 'black', width = 1)))
                  
  #This variable is from the user profile, therefore its self-reported
  # Update layout
  plot <- plot %>% layout(title = list(text = paste0("Self Reported Ages of Verified Participants as of ", Sys.Date())),
                          xaxis = list(title = list(text = "Age")),
                          yaxis = list(title = list(text = "Count")),
                          font = list(family = "Noto Sans"))
  plot} else{
    # Create the histogram plot with plotly
    plot <- plot_ly(data = age_data, x = ~Age, type = 'histogram',
                    hoverinfo = 'x+y', hoverlabel = list(bgcolor = 'white'),
                    marker = list(color = 'rgb(42, 114, 165)', line = list(color = 'black', width = 1)))
    
    #This variable is from the user profile, therefore its self-reported
    # Update layout
    plot <- plot %>% layout(title = list(text = paste0("Self Reported Ages of Participants as of ", Sys.Date())),
                            xaxis = list(title = list(text = "Age")),
                            yaxis = list(title = list(text = "Count")),
                            font = list(family = "Noto Sans"))
    plot
    
  }

}
}

