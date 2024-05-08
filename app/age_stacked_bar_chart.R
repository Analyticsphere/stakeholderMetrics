age_stacked_bar_chart <- function(ip_age_data = data, v_age_data = data) {
  library(tidyverse)
  library(plotly)
  
  if (nrow(ip_age_data) <= 9) {
    return(plotly::plot_ly() %>% layout(title = "Not Enough Data to Display This Chart"))
  } else {
    # Combining the age data and setting a unified order for age categories
    combined_ages <- c(ip_age_data$age, v_age_data$age)
    ordered_ages <- factor(combined_ages, levels = unique(combined_ages[order(combined_ages)]))
    
    ip_age_factor <- data.frame(age_factor = ordered_ages[1:nrow(ip_age_data)])
    v_age_factor <- data.frame(age_factor = ordered_ages[(nrow(ip_age_data) + 1):length(ordered_ages)])
    
    ip_age_factor$population <- "Invited"
    v_age_factor$population <- "Verified"
    
    all_age_data <- rbind(ip_age_factor, v_age_factor)
    
    count_matrix <- all_age_data %>%
      group_by(age_factor, population) %>%
      summarize(count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = population, values_from = count, values_fill = list(count = 0))
    
    verified_color <- color_palette$yellow[1]
    invited_color <- 'rgb(45, 159, 190)'
    
    fig <- plot_ly(data = count_matrix) %>%
      add_bars(x = ~age_factor, y = ~Invited, name = 'Invited', marker = list(color = invited_color)) %>%
      add_bars(x = ~age_factor, y = ~Verified, name = 'Verified', marker = list(color = verified_color)) %>%
      layout(yaxis = list(title = 'Counts'), 
             xaxis = list(title = 'Age'),
             title = 'Age Distribution, Invited vs. Verified Participants',
             barmode = 'stack',
             margin = list(t = 50, b = 50))
    fig
  }
}
