income_distribution <- function(income_data = data) {
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
      "< 10K", "10-24K", "25-34K", "35-49K", "50-74K", "75-99K",
      "100-149K", "150-199K", "> 200K", "Declined", "Unknown"
    )
    
    # Convert income column to ordered factor
    income_data$income <- factor(income_data$income, levels = ordered_income_levels, ordered = TRUE)
    
    # Prepare income distribution data
    income_distribution <- income_data %>%
      count(income) %>%
      mutate(income = factor(income, levels = ordered_income_levels))
    
    # Create Plotly bar chart with fixed aspect ratio
    plot <- plot_ly(
      data = income_distribution, 
      x = ~income, y = ~n, 
      type = 'bar', 
      hoverinfo = 'x+y',
      hoverlabel = list(bgcolor = 'white'),
      marker = list(
        color = 'rgb(28, 94, 134)',
        line = list(color = 'black', width = 1)
      )
    )
    
    # Set layout with a fixed 16x9 aspect ratio (e.g., 800px width and 450px height)
    plot <- plot %>%
      layout(
        xaxis = list(title = "Annual Income (dollars/year)"),
        yaxis = list(title = "Count"),
        font = list(family = "Noto Sans"),
        margin = list(t = 10, b = 10, l = 10, r = 10),  # Adjust margins
        autosize = TRUE  # Ensure the plot is autosized
      )
    
    plot
  }
}
