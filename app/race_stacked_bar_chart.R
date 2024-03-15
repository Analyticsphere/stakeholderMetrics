#double bar chart
#sex
# Function to create a side-by-side bar chart comparing two categories
race_stacked_bar_chart <- function(ip_race_data = data, v_race_data = data) {
  library(tidyverse)
  library(dplyr)
  library(plotly)
  
  # Check if the dataset is large enough
  if (nrow(ip_race_data) <= 9) {
    return(plotly::plot_ly() %>%
             layout(title = "Not Enough Data to Display This Chart"))
  } else {
    
    #this editing must remain here 
    #mapping verified data to invited data categories
    v_race_data <- v_race_data %>%
      mutate(race = if_else(is.na(race), "NA", race)) %>% 
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
        TRUE ~ race 
      ))
    
    #convert race variable to a factor
    ip_race_factor <- data.frame(race_factor = factor(ip_race_data$race, levels =
                                                        c("White, Non-Hispanic", "Other", "Unknown", "NA")))
    v_race_factor <- data.frame(race_factor = factor(v_race_data$race, levels =
                                                       c("White, Non-Hispanic", "Other", "Unknown", "NA")))
    #label the data
    ip_race_factor$population <- "Invited"
    v_race_factor$population <- "Verified"
    all_race_data <- rbind(ip_race_factor, v_race_factor)
    
    #aggregate the number of individuals per race bucket
    count_matrix <- all_race_data %>%
      group_by(race_factor, population) %>%
      summarize(count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = population, values_from = count, values_fill = list(count = 0))
    
    #make percentages
    count_matrix <- count_matrix %>%
      mutate(percentage = 100 * (`Verified` / `Invited`))
    
    
    # Prepare the annotation text for percentages using the updated count_matrix
    percentage_text <- paste0(count_matrix$race_factor, "- ",
                              round(count_matrix$percentage, 2), "%", collapse = "\n")
    indented_percentage_text <- gsub("\n", "\n    ", percentage_text)
    annotations_text <- paste("Conversion Percentages:",
                              indented_percentage_text, sep = "\n    ")
    
    # Define colors
    verified_color <- color_palette$yellow[1]
    invited_color <- 'rgb(45, 159, 190)'
    
    # Create the stacked bar chart
    plot_ly(data = count_matrix) %>%
      add_bars(x = ~race_factor, y = ~Invited, name = 'Invited', marker = list(color = invited_color)) %>%
      add_bars(x = ~race_factor, y = ~Verified, name = 'Verified', marker = list(color = verified_color)) %>%
      layout(
        yaxis = list(title = 'Counts'), 
        xaxis = list(title = 'Race'),
        title = 'Race Distribution, Invited vs. Verified Participants',
        barmode = 'stack',
        margin = list(b = 50, t = 50)
      )
  }
}