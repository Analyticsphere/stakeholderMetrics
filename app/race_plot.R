race_plot <- function(race_data = data){
  #load libraries
library(tidyverse) 
library(dplyr) 
library(plotly)
  
  # Check if the filtered dataset is empty
  if (nrow(race_data) <= 9) {
    # Return a message indicating not enough data
    return(plotly::plot_ly() %>% 
             layout(title = "Not Enough Data to Display This Chart"))
  } else {

# Count the occurrences of each race/ethnicity
race_counts <- table(race_data$race)
  
# Convert to a dataframe for Plotly
race_df <- as.data.frame(race_counts)
names(race_df) <- c("race", "Count")

#identify number of colors to use  
unique_items <- unique(race_df$race)
n_colors <- length(unique(race_df$race))

# Ensure you have a sufficient number of colors for your activities
cols <- select_colors(color_palette, n_colors)

# Map colors to activities to ensure consistency
color_mapping <- setNames(cols, unique_items)
  
# Create a Plotly pie chart
fig <- plot_ly(race_df, labels = ~race, values = ~Count, type = 'pie',
               textinfo='label',
               marker = list(colors = color_mapping),
               hoverinfo = 'label+percent',
               insidetextorientation = 'radial',
               domain = list(x = c(0.1, 0.9), y = c(0.1, 0.9)))
  
# Customize the layout
curr.date <- Sys.Date()
fig <- fig %>% layout(title = c("Self-Reported Race of Participants Who Completed BOH Section"),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      font = list(family = "Noto Sans"),
                      margin = list(t = 50))
  
  # Print the plot
  fig
  }
}







# Define a function to map full names to acronyms
map_race_to_acronym <- function(race) {
  switch(race,
         "American Indian or Alaska Native" = "AIAN",
         "Black, African American or African" = "BAAA",
         "Hawaiian or other Pacific Islander" = "HPI",
         "Middle Eastern or North African" = "MENA",
         "Multi-race" = "Muti",
         "Other" = "Other",
         "Skipped this question" = "Skip",
         "UNKNOWN" = "Unknown",
         "White" = "W",
         NULL
  )
}


# Apply the function to create a new column with acronyms
race_df$Acronym <- ifelse(race_df$race == "American Indian or Alaska Native", "AIAN",
                   ifelse(race_df$race == "Asian", "A",
                   ifelse(race_df$race == "Black, African American, or African", "BAAA",
                   ifelse(race_df$race == "Hawaiian or other Pacific Islander", "HPI",
                   ifelse(race_df$race == "Hispanic, Latino, or Spanish", "HLS",
                   ifelse(race_df$race == "Middle Eastern or North African", "MENA",
                   ifelse(race_df$race == "Multi-race", "Multi",
                   ifelse(race_df$race == "Other", "Other",
                   ifelse(race_df$race == "Skipped this question", "Skip",
                   ifelse(race_df$race == "UNKNOWN", "Unknown",
                    ifelse(race_df$race == "White", "W", NA)))))))))))
                        

# Create a Plotly pie chart
fig <- plot_ly(race_df, labels = ~race, values = ~Count, type = 'pie',
               text = race_df$Acronym,
               textinfo = 'text',
               marker = list(colors = 'blue'),
               hoverinfo = 'label+percent',
               insidetextorientation = 'radial',
               domain = list(x = c(0.1, 0.9), y = c(0.1, 0.9)))

# Customize the layout
curr.date <- Sys.Date()
fig <- fig %>% layout(title = c("Self-Reported Race of Participants Who Completed BOH Section"),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      font = list(family = "Noto Sans"),
                      margin = list(t = 50))

# Print the plot
fig
