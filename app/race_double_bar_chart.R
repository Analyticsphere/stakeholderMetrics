#double bar chart
#sex
# Function to create a side-by-side bar chart comparing two categories
race_double_bar_chart <- function(ip_race_data = data, v_race_data = data) {
  library(tidyverse)
  library(dplyr)
  library(plotly)
  
  # Check if the dataset is large enough
  if (nrow(ip_race_data) <= 9) {
    return(plotly::plot_ly() %>%
             layout(title = "Not Enough Data to Display This Chart"))
  } else {
    
  ip_race_data <- ip_race_data %>%
    mutate(race = case_when(
      race == "WHITE, NON-HISPANIC" ~ "White, Non-Hispanic",
      race == "OTHER" ~ "Other",
      race == "UNKNOWN" ~"Unknown",
      TRUE ~ race # Default case to handle any unexpected values
    ))
  
    v_race_data <- v_race_data %>%
      mutate(race = if_else(is.na(race), "NA", race)) %>% # Convert existing NA values to "NA" string
      mutate(race = case_when(
        race == "White" ~ "White, Non-Hispanic",
        race == "Black, African American, or African" ~ "Other",
        race == "Multi-race" ~ "Other",
        race == "Asian" ~ "Other",
        race == "Hispanic, Latino, or Spanish" ~ "Other",
        race == "Hawaiian or other Pacific Islander" ~ "Other",
        race == "Middle Eastern or North African" ~ "Other",
        race == "Other" ~ "Other",
        race == "American Indian or Alaska Native" ~ "Other",
        race == "UNKNOWN" ~ "Unknown", # Retain "UNKNOWN" as is
        race == "Skipped this question" ~ "NA", # Convert to NA
        TRUE ~ race # Default case to handle any unexpected values
      ))
    
    # Ensure 'sex' is a factor with specific levels
    ip_race_data$race_factor <- factor(ip_race_data$race, levels = c("White, Non-Hispanic", "Other", "Unknown", "NA"))
    v_race_data$race_factor <- factor(v_race_data$race, levels = c("White, Non-Hispanic", "Other", "Unknown", "NA"))
    
    
    # Aggregate data by sex and group
    ip_race_group_counts <- ip_race_data %>%
      count(race_factor)
    ip_race_group_counts$population <- "Invited"
    # Aggregate data by sex and group
    v_race_group_counts <- v_race_data %>%
      count(race_factor)
    v_race_group_counts$population = "Verified"
    
    race_group_counts <- as.data.frame(rbind(ip_race_group_counts, v_race_group_counts))
    
    # Calculate ratios of invited to verified for each sex
    #this seems like overkill but actually makes the annotation plotting easier
    ratios <- merge(ip_race_group_counts, v_race_group_counts, by = "race_factor")
    ratios$percentages <- with(ratios, 100*(n.y / n.x))
    
    color_mapping <- setNames(c('rgb(42, 114, 165)', 'rgb(45, 159, 190)'), c("Verified", "Invited"))
    race_group_counts$color <- color_mapping[race_group_counts$population]
    
    # Spread the data to wide format
    wide_data <- race_group_counts %>%
      group_by(race_factor) %>%
      mutate(color = first(color)) %>% # Ensure color consistency
      ungroup() %>%
      spread(key = population, value = n)
    
    wide_data <- merge(wide_data, ratios, by = "race_factor")

    
    # Create the initial grouped bar chart without the ratio and second y-axis
    p <- plot_ly(data = wide_data) %>%
      add_trace(x = ~race_factor, y = ~Invited, type = 'bar', name = 'Invited',
                marker = list(color = 'rgb(45, 159, 190)')) %>%
      add_trace(x = ~race_factor, y = ~Verified, type = 'bar', name = 'Verified',
                marker = list(color = 'rgb(42, 114, 165)')) %>%
      layout(
        yaxis = list(title = 'Counts'), 
        xaxis = list(title = 'Race'),
        barmode = 'group', 
        margin = list(b = 50, t = 50, r = 150)  # Adjust right margin to make space for the list
      )
    
    # Create a text annotation for the ratio values
    ratio_list_text <- paste("Conversion Percentages:", paste0("• ", wide_data$race_factor, "- ",
                                                               round(wide_data$percentages,3), "%",
                                                               collapse = "\n"), sep = "\n")
    
    # Adjust the y position to place the box under the legend
    # The y position value is lower to drop the box under the legend. Adjust the value as needed based on your plot's layout
    p <- p %>%
      layout(
        title = 'Site-Reported Race, Invited vs. Verified Participant Distribution',
        annotations = list(
          list(
            text = ratio_list_text,
            align = 'left',
            showarrow = FALSE,
            x = 0.95,  # Position the box on the RHS, adjust as needed
            xanchor = 'left',
            y = 0.8,  # Lower the box under the legend. Adjust this value as necessary
            yanchor = 'top',
            xref = 'paper',
            yref = 'paper',
            font = list(size = 10),
            bordercolor = 'black',
            borderwidth = 1,
            borderpad = 4,
            bgcolor = 'white',
            opacity = 0.8
          )
        )
      )
    
    # Display the plot
    p
    
  }
}




