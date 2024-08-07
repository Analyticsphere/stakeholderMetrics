participant_status<- function(status_data=data){
  library(tidyverse) 
  library(dplyr) 
  library(plotly)
    
    #create a mapping between conceptIDs and site names
    site_map <- data.frame(site_name = c("KP Colorado", 
                                         "KP Hawaii", 
                                         "Marshfield", 
                                         "KP Georgia",
                                         "KP Northwest",
                                         "Health Partners",
                                         "Henry Ford Health System", 
                                         "Sanford",
                                         "University of Chicago"), 
                           site_id = c("125001209",
                                       "300267574",
                                       "303349821",
                                       "327912200",
                                       "452412599",
                                       "531629870",
                                       "548392715",
                                       "657167265",
                                       "809703864"))
    
    #identify number of colors to use  
    unique_items <- unique(site_map$site_name)
    n_colors <- length(unique_items)
    
    # Ensure you have a sufficient number of colors for your activities
    cols <- select_colors(color_palette, n_colors)


    # Merge to get the site names
    status_data <- left_join(status_data, site_map, by = c("site" = "site_id"))
    
    # Initialize the plot
    fig <- plot_ly(type = "funnel")
    
    # Add each site's data
    for (i in 1:nrow(status_data)) {
      fig <- fig %>%
        add_trace(
          name = status_data$site_name[i],
          y = c("Signed-In:", "Consented:", "Profile Done:", "Verification:", "Verified:"),
          x = c(status_data$signed_in[i], status_data$consented[i], status_data$profile_done[i],
                status_data$verification[i], status_data$verified[i]),
          text = c(status_data$signed_in[i], status_data$consented[i], status_data$profile_done[i],
                   status_data$verification[i], status_data$verified[i]),
          textposition = "inside",
          textinfo = "none",
          hoverinfo= "text",
          marker = list(color = cols[i])
        )
    }
    
    # Layout adjustments
    fig <- fig %>%
      layout(
        yaxis = list(
          type = 'category',  # Ensure y-axis is treated as categorical
          categoryorder = 'array',  # Specify the order of categories
          categoryarray = c("Signed-In:", "Consented:", "Profile Done:", "Verification:", "Verified:")  # Correct order
        ),
        annotations = list(
          # Adjust annotations to display to the left of the chart
          list(x = 0.05, y = "Signed-In:", xref = "paper", yref = "y",
               text = paste0("Signed-In:\n", sum(status_data$signed_in)),
               showarrow = FALSE, xanchor = "right", yanchor = "middle"),
          list(x = 0.1, y = "Consented:", xref = "paper", yref = "y",
               text = paste0("Consented:\n", sum(status_data$consented)),
               showarrow = FALSE, xanchor = "right", yanchor = "middle"),
          list(x = 0.1, y = "Profile Done:", xref = "paper", yref = "y",
               text = paste0("Profile Done:\n", sum(status_data$profile_done)),
               showarrow = FALSE, xanchor = "right", yanchor = "middle"),
          list(x = 0.1, y = "Verification:", xref = "paper", yref = "y",
               text = paste0("Verification:\n", sum(status_data$verification)),
               showarrow = FALSE, xanchor = "right", yanchor = "middle"),
          list(x = 0.1, y = "Verified:", xref = "paper", yref = "y", 
               text = paste0("Verified:\n", sum(status_data$verified)),
               showarrow = FALSE, xanchor = "right", yanchor = "middle")
        ),
        margin(l = 100)
      )
    
    fig
  }