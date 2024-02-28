age_double_bar_chart<- function(ip_age_data = data, v_age_data = data){
  library(tidyverse) 
  library(dplyr) 
  library(plotly)
  
  # Check if the filtered dataset is empty
  if (nrow(ip_age_data) <= 9) {
    # Return a message indicating not enough data
    return(plotly::plot_ly() %>% 
             layout(title = "Not Enough Data to Display This Chart"))
  } else {

    #variable is Age in the invited participant data
    #variable is age in the verified participant data

      
      # Convert sex variable to a factor with specific levels
      ip_age_factor <- data.frame(age_factor = factor(ip_age_data$Age))
                                                      
      v_age_factor <- data.frame(age_factor = factor(v_sex_data$age))
      
      # Label the data
      ip_age_factor$population <- "Invited"
      v_age_factor$population <- "Verified"
      
      # Combine the datasets
      all_age_data <- rbind(ip_age_factor, v_age_factor)
      
      # Aggregate the number of individuals per sex bucket
      count_matrix <- all_age_data %>%
        group_by(age_factor, population) %>%
        summarize(count = n(), .groups = 'drop') %>%
        pivot_wider(names_from = population, values_from = count,
                    values_fill = list(count = 0))
      
      # Calculate percentages
      count_matrix <- count_matrix %>%
        mutate(percentage = 100 * (`Verified` / `Invited`))

      # Replace "Inf" values with NA in the 'percentage' column
      count_matrix <- count_matrix %>%
        mutate(percentage = ifelse(is.infinite(percentage), NA, percentage))
      
      # View the updated count_matrix
      print(count_matrix)
      
      
      # Define colors
      verified_color <- 'rgb(42, 114, 165)'
      invited_color <- 'rgb(45, 159, 190)'
      
      # Prepare the annotation text for percentages
      percentage_text <- paste0(count_matrix$age_factor, ": ",
                                round(count_matrix$percentage, 2), "%", collapse = "\n")
      indented_percentage_text <- gsub("\n", "\n    ", percentage_text) 
      annotations_text <- paste("Conversion Percentages:",
                                indented_percentage_text, sep = "\n    ")
      
      # Create the plot with corrected color specification and annotations
      p <- plot_ly(data = count_matrix) %>%
        add_bars(x = ~age_factor, y = ~Invited, name = 'Invited',
                 marker = list(color = invited_color)) %>%
        add_bars(x = ~age_factor, y = ~Verified, name = 'Verified',
                 marker = list(color = verified_color)) %>%
        layout(yaxis = list(title = 'Counts'), 
               xaxis = list(title = 'Age'),
               title = 'Site-Reported Age, Invited vs. Verified Participant Distribution',
               barmode = 'group',
               margin = list(t = 50, b = 50, r = 200), 
               annotations = list(
                 list(
                   text = annotations_text,
                   align = 'left',
                   showarrow = FALSE,
                   x = 1.15, xanchor = 'right',
                   y = 0.5, yanchor = 'middle',
                   xref = 'paper', yref = 'paper',
                   font = list(size = 10),
                   bordercolor = 'black', borderwidth = 1, borderpad = 4,
                   bgcolor = 'white', opacity = 0.8
                 )
               )
        )
      
      p
  }
}

