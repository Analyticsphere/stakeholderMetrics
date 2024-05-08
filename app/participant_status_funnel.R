#participant status funnel chart
#this chart should use invited participant data
participant_status<- function(status_data=data){
  library(tidyverse) 
  library(dplyr) 
  library(plotly)
  
  # Check if the filtered dataset is empty
  if (nrow(status_data) <= 9) {
    # Return a message indicating not enough data
    return(plotly::plot_ly() %>% 
             layout(title = "Not Enough Data to Display This Chart"))
  } else {
    
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

    
    #initialize graphic with first site data
    kp_data <- status_data[status_data$site == "125001209",]
    
    signed_in <- nrow(kp_data[kp_data$d_230663853 == 353358909,])
    signed_in_population <- kp_data[kp_data$d_230663853 == 353358909,]
    consented <- nrow(signed_in_population[signed_in_population$d_919254129 == 353358909,])
    consented_population <- signed_in_population[signed_in_population$d_919254129 == 353358909,]
    profile_done <- nrow(consented_population[consented_population$d_699625233 == 353358909,])
    profile_population <-  consented_population[consented_population$d_699625233 == 353358909,]
    verification <- nrow(profile_population[profile_population$verified == 875007964 |
                                              profile_population$verified ==197316935|
                                              profile_population$verified == 219863910|
                                              profile_population$verified == 922622075,])
    verified <- nrow(profile_population[profile_population$verified == 197316935,])
    
    #initialize the plot
    fig <- plot_ly(
        type = "funnel",
        name = "KP Colorado",
        y = c("Signed-In:", "Consented:", "Profile Done:", "Verification:", "Verified:"),
        x = c(signed_in, consented, profile_done, verification, verified),
        textposition="inside",
        textinfo="none",
        marker = list(color = cols[1]))
    
    #for loop to calculate stats for each site
    for(i in 2:nrow(site_map)){
      site <- unique(site_map$site_id)[i]
      site_data <- status_data[status_data$site == site,]
      
      signed_in <- nrow(site_data[site_data$d_230663853 == 353358909,])
      signed_in_population <- site_data[site_data$d_230663853 == 353358909,]
      consented <- nrow(signed_in_population[signed_in_population$d_919254129 == 353358909,])
      consented_population <- signed_in_population[signed_in_population$d_919254129 == 353358909,]
      profile_done <- nrow(consented_population[consented_population$d_699625233 == 353358909,])
      profile_population <-  consented_population[consented_population$d_699625233 == 353358909,]
      verification <- nrow(profile_population[profile_population$verified == 875007964 |
                                                profile_population$verified ==197316935|
                                                profile_population$verified == 219863910|
                                                profile_population$verified == 922622075,])
      verified <- nrow(profile_population[profile_population$verified == 197316935,])
      
      fig <- fig %>%
        add_trace(
          type = "funnel",
          name = site_map[site_map$site_id == site,][1],
          y = c("Signed-In:", "Consented:", "Profile Done:", "Verification:", "Verified:"),
          x = c(signed_in, consented, profile_done, verification, verified),
          textposition="inside",
          textinfo="none",
          marker = list(color = cols[i]))  # Assign color from vector)
    }
    
    #calculate totals for each category
    total_signed_in <- nrow(status_data[status_data$d_230663853 == 353358909,])
    signed_in_population <- status_data[status_data$d_230663853 == 353358909,]
    total_consented <- nrow(signed_in_population[signed_in_population$d_919254129 == 353358909,])
    consented_population <- signed_in_population[signed_in_population$d_919254129 == 353358909,]
    total_profile_done <- nrow(consented_population[consented_population$d_699625233 == 353358909,])
    profile_population <-  consented_population[consented_population$d_699625233 == 353358909,]
    total_verification <- nrow(profile_population[profile_population$verified == 875007964 |
                                                    profile_population$verified ==197316935|
                                                    profile_population$verified == 219863910|
                                                    profile_population$verified == 922622075,])
    total_verified <- nrow(profile_population[profile_population$verified == 197316935,])
    
    #finalize plot here
    fig <- fig %>%
      layout(title = "Participant Workflow by Site",
             yaxis = list(categoryarray = c("Signed-In:", "Consented:", "Profile Done:", "Verification:", "Verified:")),
             annotations = list(
               list(x = 0, y = "Signed-In:", xref = "paper", yref = "y",
                    text = sprintf("%d", total_signed_in),
                    showarrow = FALSE, xanchor = "right", yanchor = "top"),
               list(x = 0, y = "Consented:", xref = "paper", yref = "y",
                    text = sprintf("%d", total_consented),
                    showarrow = FALSE, xanchor = "right", yanchor = "top"),
               list(x = 0, y = "Profile Done:", xref = "paper", yref = "y",
                    text = sprintf("%d", total_profile_done),
                    showarrow = FALSE, xanchor = "right", yanchor = "top"),
               list(x = 0, y = "Verification:", xref = "paper", yref = "y",
                    text = sprintf("%d", total_verification),
                    showarrow = FALSE, xanchor = "right", yanchor = "top"),
               list(x = 0, y = "Verified:", xref = "paper", yref = "y", 
                    text = sprintf("%d", total_verified),
                    showarrow = FALSE, xanchor = "right", yanchor = "top")
             ))
    
    fig
    
    
    }
    
}

