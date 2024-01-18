biospecimen_collections_distribution <- function(biocol_data = data, selected_hospital = ".", selected_sex = ".",
                                                 selected_age = ".", selected_race = ".", selected_campaign = ".",
                                                 selected_biospec = "."){
  # Load libraries
  #load libraries
  library(bigrquery)
  library(foreach)
  library(stringr)
  library(magrittr)
  library(arsenal)
  library(gtsummary)
  library(rio)
  library(ggplot2)
  library(gridExtra)
  library(scales)
  library(gt)
  library(tinytex)
  library(data.table) 
  library(tidyverse) 
  library(dplyr) 
  library(reshape) 
  library(listr) 
  library(sqldf) 
  library(lubridate)
  library(stringr)
  library(RColorBrewer)
  library(ggrepel)
  library(DBI)
  library(RSQLite)
  library(glue)
  library(plotly)
  
  biocol_data <- expss::apply_labels(biocol_data,
                                  d_827220437 = c("HealthPartners"= 531629870,
                                                  "Henry Ford Health System"=548392715,
                                                  "Kaiser Permanente Colorado" = 125001209,
                                                  "Kaiser Permanente Georgia" = 327912200,
                                                  "Kaiser Permanente Hawaii" = 300267574,
                                                  "Kaiser Permanente Northwest" = 452412599,
                                                  "Marshfield Clinic Health System" = 303349821,
                                                  "Sanford Health" = 657167265, 
                                                  "University of Chicago Medicine" = 809703864,
                                                  "National Cancer Institute" = 517700004,
                                                  "National Cancer Institute" = 13,"Other" = 181769837),
                                  sex = c("Male" = 654207589, "Female" = 536341288, "Other" = 576796184))
 
  # Define biocol_type based on your criteria
  biocol_data <- biocol_data %>%
    mutate(biocol_type = case_when(
      d_878865966 == 353358909 & d_167958071 == 353358909 & d_684635302 == 353358909 ~ "All 3 Sample Donations",
      d_878865966 == 353358909 & d_167958071 == 353358909 & d_684635302 == 104430631 ~ "Blood & Urine",
      d_878865966 == 353358909 & d_167958071 == 104430631 & d_684635302 == 353358909 ~ "Blood & Mouthwash",
      d_878865966 == 104430631 & d_167958071 == 353358909 & d_684635302 == 353358909 ~ "Mouthwash & Urine",
      d_878865966 == 353358909 & d_167958071 == 104430631 & d_684635302 == 104430631 ~ "Blood Only",
      d_878865966 == 104430631 & d_167958071 == 353358909 & d_684635302 == 104430631 ~ "Urine Only",
      d_878865966 == 104430631 & d_167958071 == 104430631 & d_684635302 == 353358909 ~ "Mouthwash Only",
      d_878865966 == 104430631 & d_167958071 == 104430631 & d_684635302 == 104430631 ~ "No Samples"
    ))
  
  # Filter data based on provided criteria
  if(selected_hospital != "."){
    biocol_data <- biocol_data[biocol_data$d_827220437 == selected_hospital,]
  }
  if(selected_sex != "."){
    biocol_data <- biocol_data[biocol_data$sex == selected_sex,]
  }
  if(selected_age != "."){
    biocol_data <- biocol_data[biocol_data$AgeUP_cat == selected_age,]
  }
  if(selected_race != "."){
    biocol_data <- biocol_data[biocol_data$Race_Ethnic == selected_race,]
  }
  if(selected_campaign != "."){
    biocol_data <- biocol_data[biocol_data$active_camptype == selected_campaign,]
  }
  if(selected_biospec != "."){
    biocol_data <- biocol_data[biocol_data$biocol_type == selected_biospec,]
  }
  
  # Count the occurrences of each biospecimen collection type
  biocol_counts <- table(biocol_data$biocol_type)
  
  # Convert to a dataframe for Plotly
  biocol_df <- as.data.frame(biocol_counts)
  names(biocol_df) <- c("CollectionType", "Count")
  
  # Create a Plotly pie chart
  fig <- plot_ly(biocol_df, labels = ~CollectionType, values = ~Count, type = 'pie',
                 textinfo = 'label+percent',
                 insidetextorientation = 'radial')
  
  # Customize the layout
  curr.date <- Sys.Date()
  fig <- fig %>% layout(title = paste0("Distribution of Biospecimen Collections as of ", curr.date),
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  # Print the plot
  fig
}
