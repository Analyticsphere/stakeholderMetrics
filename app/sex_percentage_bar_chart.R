sex_percentage_bar_chart <- function(ip_sex_data = data, v_sex_data = data) {
  library(tidyverse)
  library(plotly)
  
  if (nrow(ip_sex_data) <= 9) {
    return(plotly::plot_ly() %>% layout(title = "Not Enough Data to Display This Chart"))
  } else {
    # Convert sex variable to a factor with specific levels
    ip_sex_factor <- data.frame(sex_factor = factor(ip_sex_data$sex,
                                                    levels = c("Female", "Nonbinary", "Male", "Unknown")))
    v_sex_factor <- data.frame(sex_factor = factor(v_sex_data$sex,
                                                   levels = c("Female", "Nonbinary", "Male", "Unknown")))
    
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
      mutate(percentage = 100 * (`Verified` / `Invited`))
    
    
    # Define colors
    verified_color <- color_palette$yellow[1]
    invited_color <- 'rgb(45, 159, 190)'    
    
    # Create the percentage bar chart
    plot_ly(data = count_matrix, x = ~sex_factor, y = ~percentage, type = 'scatter',
            marker = list(color = 'rgb(42, 114, 165)'), mode = 'markers') %>%
      layout(
        yaxis = list(title = 'Percentage'), 
        xaxis = list(title = 'Gender'),
        title = 'Response Ratio by Gender',
        margin = list(b = 50, t = 50)
      )
  }
}
