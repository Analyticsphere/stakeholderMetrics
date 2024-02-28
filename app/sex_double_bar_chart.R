#double bar chart
#sex
# Function to create a side-by-side bar chart comparing two categories
sex_double_bar_chart <- function(ip_sex_data = data, v_sex_data = data) {
  library(tidyverse)
  library(dplyr)
  library(plotly)
  
  # Check if the dataset is large enough
  if (nrow(ip_sex_data) <= 9) {
    return(plotly::plot_ly() %>%
             layout(title = "Not Enough Data to Display This Chart"))
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
      pivot_wider(names_from = population, values_from = count, values_fill = list(count = 0))
    
    # Calculate percentages
    count_matrix <- count_matrix %>%
      mutate(percentage = 100 * (`Verified` / `Invited`))
    
        

    
    # Define colors
    verified_color <- 'rgb(42, 114, 165)'
    invited_color <- 'rgb(45, 159, 190)'
    
    # Prepare the annotation text for percentages
    percentage_text <- paste0(count_matrix$sex_factor, "- ", round(count_matrix$percentage, 2), "%", collapse = "\n")
    indented_percentage_text <- gsub("\n", "\n    ", percentage_text) # Adds indentation
    annotations_text <- paste("Conversion Percentages:", indented_percentage_text, sep = "\n    ")
    
    # Create the plot with corrected color specification and annotations
    p <- plot_ly(data = count_matrix) %>%
      add_bars(x = ~sex_factor, y = ~Invited, name = 'Invited',
               marker = list(color = invited_color)) %>%
      add_bars(x = ~sex_factor, y = ~Verified, name = 'Verified',
               marker = list(color = verified_color)) %>%
      layout(yaxis = list(title = 'Counts'), 
             xaxis = list(title = 'Sex'),
             title = 'Site-Reported Sex, Invited vs. Verified Participant Distribution',
             barmode = 'group',
             margin = list(t = 50, b = 50, r = 200), # Adjusted for annotation space
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