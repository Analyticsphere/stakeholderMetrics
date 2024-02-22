income_distribution<- function(income_data=data){
library(tidyverse) 
library(dplyr) 
library(plotly)

  # Check if the filtered dataset is empty
  if (nrow(income_data) <= 9) {
    # Return a message indicating not enough data
    return(plotly::plot_ly() %>% 
             layout(title = "Not Enough Data to Display This Chart"))
  } else {

income_distribution <- income_data %>%
  count(income)

# Create the histogram plot with plotly
  plot <- plot_ly(data = income_distribution, x = ~income,
                  y = ~n, type = 'bar', hoverinfo = 'x+y',
                  hoverlabel = list(bgcolor = 'white'),
                  marker = list(color = 'rgb(28, 94, 134)',
                  line = list(color = 'black', width = 1)))
                  
  #This variable is from the user profile, therefore its self-reported
  # Update layout
  plot <- plot %>% layout(title = list(text = c("Self Reported Income of Verified Participants")),
                          xaxis = list(title = list(text = "Income")),
                          yaxis = list(title = list(text = "Count")),
                          font = list(family = "Noto Sans"))
  plot

}
}

