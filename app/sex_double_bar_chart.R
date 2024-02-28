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
    
    # Ensure 'sex' is a factor with specific levels
    ip_sex_data$sex_factor <- factor(ip_sex_data$sex, levels =
                                       c("Female", "Nonbinary", "Male", "Unknown"))
    v_sex_data$sex_factor <- factor(v_sex_data$sex, levels =
                                      c("Female", "Nonbinary", "Male", "Unknown"))
    
    # Aggregate data by sex and group
    ip_sex_group_counts <- ip_sex_data %>%
      count(sex_factor)
    ip_sex_group_counts$population <- "Invited"
    # Aggregate data by sex and group
    v_sex_group_counts <- v_sex_data %>%
      count(sex_factor)
    v_sex_group_counts$population = "Verified"
    
    #combine
    sex_group_counts <- as.data.frame(rbind(ip_sex_group_counts, v_sex_group_counts))
        
    # Calculate percentages directly
    percentages <- sex_group_counts %>%
      group_by(sex_factor) %>%
      summarise(percentage = 100 * first(n[population == "Verified"]) / first(n[population == "Invited"])) %>%
      ungroup()
    
    # Define colors
    verified_color <- 'rgb(42, 114, 165)'
    invited_color <- 'rgb(45, 159, 190)'
    
    #conversion rates
    percentage_text <- paste(percentages$sex_factor, "- ",
                             round(percentages$percentage, 2), "%", collapse = "\n")
    # Adding spaces for indentation
    indented_percentage_text <- gsub("\n", "\n    ", percentage_text)
    annotations_text <- paste("Conversion Percentages:", indented_percentage_text, sep = "\n    ")
    
    # Plot with annotations added on the right-hand side
    p <- plot_ly() %>%
      add_bars(data = sex_group_counts[sex_group_counts$population == "Invited",], 
               x = ~sex_factor, y = ~n, 
               name = 'Invited',
               marker = list(color = invited_color)) %>%
      add_bars(data = sex_group_counts[sex_group_counts$population == "Verified",], 
               x = ~sex_factor, y = ~n, 
               name = 'Verified',
               marker = list(color = verified_color)) %>%
      layout(yaxis = list(title = 'Counts'), 
             xaxis = list(title = 'Sex'),
             title = 'Site-Reported Sex, Invited vs. Verified Participant Distribution',
             barmode = 'group',
             annotations = list(
               list(
                 text = annotations_text,
                 align = 'left',
                 showarrow = FALSE,
                 x = 1.05, 
                 xanchor = 'left',
                 y = 0.5,  
                 yanchor = 'middle',
                 xref = 'paper',
                 yref = 'paper',
                 font = list(size = 10),
                 bordercolor = 'black',
                 borderwidth = 1,
                 borderpad = 4,
                 bgcolor = 'white',
                 opacity = 0.8
               )
             ),
             margin = list(t = 50, b = 50, r = 200) 
      )
    
    p
    
  }
}