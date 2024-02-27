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
    
    ip_sex_data <- ip_sex_data %>%
      mutate(sex = case_when(
        sex == "Intersex or Other" ~ "Nonbinary",
        TRUE ~ as.character(sex) # Keep all other values as they are
      ))
    v_sex_data <- v_sex_data %>%
      mutate(sex = case_when(
        sex == "Intersex or Other" ~ "Nonbinary",
        TRUE ~ as.character(sex) # Keep all other values as they are
      ))
    # Ensure 'sex' is a factor with specific levels
    ip_sex_data$sex_factor <- factor(ip_sex_data$sex, levels = c("Female", "Nonbinary", "Male", "Unknown"))
    v_sex_data$sex_factor <- factor(v_sex_data$sex, levels = c("Female", "Nonbinary", "Male", "Unknown"))
    
    
    # Aggregate data by sex and group
    ip_sex_group_counts <- ip_sex_data %>%
      count(sex_factor)
    ip_sex_group_counts$population <- "Invited"
    # Aggregate data by sex and group
    v_sex_group_counts <- v_sex_data %>%
      count(sex_factor)
    v_sex_group_counts$population = "Verified"

    sex_group_counts <- as.data.frame(rbind(ip_sex_group_counts, v_sex_group_counts))
        
    # Calculate ratios of invited to verified for each sex
    #this seems like overkill but actually makes the annotation plotting easier
    ratios <- merge(ip_sex_group_counts, v_sex_group_counts, by = "sex_factor")
    ratios$ratio <- with(ratios, n.x / n.y)
    
    color_mapping <- setNames(c('rgb(42, 114, 165)', 'rgb(45, 159, 190)'), c("Verified", "Invited"))
    sex_group_counts$color <- color_mapping[sex_group_counts$population]
    #plot
    fig <- plot_ly(data = sex_group_counts, x = ~sex_factor, y = ~n, color = ~population,
                   type = 'bar',
                   marker = list(color = ~color)) %>%
      layout(title = "Self-Reported Sex Distribution by Population",
             yaxis = list(title = "Count"),
             xaxis = list(title = "Sex"),
             barmode = 'group', # This will place bars side by side
             legend = list(title = list(text = 'Population')),
             margin = list(b = 100,t = 50))
    # Add ratio annotations
    for (i in 1:nrow(ratios)) {
      fig <- fig %>% add_annotations(
        x = (as.numeric(as.factor(ratios$sex_factor[i])) -1.2) ,
        y = max(sex_group_counts$n[sex_group_counts$sex_factor ==
                                  ratios$sex_factor[i] & sex_group_counts$population == "Invited"])+20000,
        text = sprintf("%.2f", ratios$ratio[i]),
        xref = "x",
        yref = "y",
        showarrow = FALSE
      )
    }
    
    # Add a note at the bottom of the plot
    fig <- fig %>% add_annotations(
      x = 0.5, # Centered
      y = -0.3, # Position below the x-axis
      text = "Value displayed is the ratio of invited participants to verified participants",
      showarrow = FALSE,
      xref = 'paper', # Use 'paper' for relative positioning
      yref = 'paper',
      font = list(size = 10),
      align = 'center'
    )
    
    # Return the figure
    fig

    
  
  }
}