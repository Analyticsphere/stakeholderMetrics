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

    # Specify the order of income categories
    ordered_income_levels <- c(
      "Less than $10,000/year",
      "$10,000-$24,999/year",
      "$25,000-$34,999/year",
      "$35,000-$49,999/year",
      "$50,000-$74,999/year",
      "$75,000-$99,999/year",
      "$100,000-$149,999/year",
      "$150,000-$199,999/year",
      "$200,000 or more/year",
      "Declined",
      "Unavailable/Unknown",
      "Unknown"
    )
    
    # Now, when setting up your income_data within the function or prior to plotting,
    # convert the income column to an ordered factor using these levels:
    income_data$income <- factor(income_data$income,
                                 levels = ordered_income_levels, ordered = TRUE)
    
    
    # Continue with your data processing and plotting
    income_distribution <- income_data %>%
      count(income) %>%
      mutate(income = factor(income, levels = ordered_income_levels))
    
    plot <- plot_ly(data = income_distribution, x = ~income,
                    y = ~n, type = 'bar', hoverinfo = 'x+y',
                    hoverlabel = list(bgcolor = 'white'),
                    marker = list(color = 'rgb(28, 94, 134)',
                                  line = list(color = 'black', width = 1)))
    
    plot <- plot %>%
      layout(title = "Self-reported Income of Participants",
             xaxis = list(title = "Income"),
             yaxis = list(title = "Count"),
             font = list(family = "Noto Sans"),
             margin = list(t = 50))
    plot

}
}

