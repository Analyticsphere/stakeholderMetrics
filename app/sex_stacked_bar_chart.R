sex_stacked_bar_chart <- function(ip_sex_data = data, v_sex_data = data) {
  library(tidyverse)
  library(plotly)
  
  if (nrow(ip_sex_data) <= 9) {
    return(plotly::plot_ly() %>% layout(title = "Not Enough Data to Display This Chart"))
  } else {
    # Convert sex variable to a factor with specific levels
    ip_sex_data$sex[ip_sex_data$sex == "Nonbinary"] <- "Other"
    v_sex_data$sex[v_sex_data$sex == "Nonbinary"] <- "Other"
    
    ip_sex_factor <- data.frame(sex_factor = factor(ip_sex_data$sex,
                                                    levels = c("Female", "Other", "Male", "Unknown")))
    v_sex_factor <- data.frame(sex_factor = factor(v_sex_data$sex,
                                                   levels = c("Female", "Other", "Male", "Unknown")))
    
    # Label the data
    ip_sex_factor$population <- "Invited"
    v_sex_factor$population <- "Verified"
    
    # Combine the datasets
    all_sex_data <- rbind(ip_sex_factor, v_sex_factor)
    
    # Aggregate the number of individuals per sex bucket
    count_matrix <- all_sex_data %>%
      group_by(sex_factor, population) %>%
      summarize(count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = population, values_from = count,
                  values_fill = list(count = 0))
    
    # Calculate percentages
    count_matrix <- count_matrix %>%
      mutate(percentage = round(100 * (`Verified` / `Invited`), 2))
    
    
    # Define colors
    verified_color <- color_palette$yellow[1]
    invited_color <- 'rgb(45, 159, 190)'    
    
    # Create the stacked bar chart
    plot_ly(data = count_matrix) %>%
      add_bars(x = ~sex_factor, y = ~Invited, name = 'Invited', marker = list(color = invited_color)) %>%
      add_bars(x = ~sex_factor, y = ~Verified, name = 'Verified', marker = list(color = verified_color)) %>%
      layout(
        yaxis = list(title = 'Counts'), 
        xaxis = list(title = 'Sex'),
        title = 'Sex Distribution, Invited vs. Verified Participants',
        barmode = 'stack',
        margin = list(b = 50, t = 50)
      )
  }
}
