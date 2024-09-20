participant_status <- function(status_data = data) {
  library(tidyverse)
  library(dplyr)
  library(plotly)
  
  # Create a mapping between conceptIDs and site names using tribble
  site_map <- tibble::tribble(
    ~site_name,                  ~site_id,     ~color,
    "HealthPartners",            "531629870",  "#F6CC6C",
    "Henry Ford",                "548392715",  "#164C71",
    "KP Georgia",                "327912200",  "#309EBD",
    "KP Hawaii",                 "300267574",  "#3C989E",
    "KP Northwest",              "452412599",  "#565C65",
    "Marshfield",                "303349821",  "#CC7D15",
    "Sanford",                   "657167265",  "#648EB4",
    "KP Colorado",               "125001209",  "#FDBE19",
    "University of Chicago",     "809703864",  "#51708A",
    "Baylor Scott & White",      "472940358",  "#2973A5"
  )

  # Merge to get the site names
  status_data <- left_join(status_data, site_map, by = c("site" = "site_id"))
  
  # Initialize the plot
  fig <- plot_ly(type = "funnel")
  
  # Add each site's data
  for (i in 1:nrow(status_data)) {
    fig <- fig %>%
      add_trace(
        name = status_data$site_name[i],
        y = c("Signed-In:", "Consented:", "Profile Done:", 
              #"Verification:", 
              "Verified:"),
        x = c(status_data$signed_in[i], status_data$consented[i], status_data$profile_done[i],
              #status_data$verification[i], 
              status_data$verified[i]),
        text = c(status_data$signed_in[i], status_data$consented[i], status_data$profile_done[i],
                 #status_data$verification[i], 
                 status_data$verified[i]),
        textposition = "inside",
        textinfo = "none",
        hoverinfo = "text",
        marker = list(color = site_map$color[i])
      )
  }
  
  # Layout adjustments
  fig <- fig %>%
    layout(
      yaxis = list(
        type = 'category',  # Ensure y-axis is treated as categorical
        categoryorder = 'array',  # Specify the order of categories
        categoryarray = c("Signed-In:", "Consented:", "Profile Done:", 
                          #"Verification:", 
                          "Verified:")
      ),
      annotations = list(
        # Adjust annotations to display to the left of the chart
        list(x = 0.05, y = "Signed-In:", xref = "paper", yref = "y",
             text = paste0("Signed-In:\n", sum(status_data$signed_in)),
             showarrow = FALSE, xanchor = "right", yanchor = "middle"),
        list(x = 0.05, y = "Consented:", xref = "paper", yref = "y",
             text = paste0("Consented:\n", sum(status_data$consented)),
             showarrow = FALSE, xanchor = "right", yanchor = "middle"),
        list(x = 0.05, y = "Profile Done:", xref = "paper", yref = "y",
             text = paste0("Profile Done:\n", sum(status_data$profile_done)),
             showarrow = FALSE, xanchor = "right", yanchor = "middle"),
        # list(x = 0.05, y = "Verification:", xref = "paper", yref = "y",
        #      text = paste0("Verification:\n", sum(status_data$verification)),
        #      showarrow = FALSE, xanchor = "right", yanchor = "middle"),
        list(x = 0.05, y = "Verified:", xref = "paper", yref = "y", 
             text = paste0("Verified:\n", sum(status_data$verified)),
             showarrow = FALSE, xanchor = "right", yanchor = "middle")
      ),
      margin = list(l = 100, t = 20, b = 20, r = 20),  # Adjust margins
      autosize = TRUE
    )
  
  fig
}
