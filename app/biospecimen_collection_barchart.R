biospecimen_collection_barchart <- function(biocol_data = data) {
  # Load libraries
  library(tidyverse) 
  library(dplyr) 
  library(plotly)
  
  # Check if the filtered dataset is empty
  if (nrow(biocol_data) <= 9) {
    # Return a message indicating not enough data
    return(plotly::plot_ly() %>% 
             layout(title = "Not Enough Data to Display This Chart"))
  } else {

    #creating the dummy variables for each type of collection
    biocol_data <- biocol_data %>%
      mutate(
        blood = as.integer(d_878865966 == "353358909"),
        urine = as.integer(d_167958071 == "353358909"),
        mouthwash = as.integer(d_684635302 == "353358909"),
        all = as.integer(blood == 1 & urine == 1 & mouthwash == 1),
        none = as.integer(blood == 0 & urine == 0 & mouthwash == 0)
      )
    
    # Summarize data-- sum each collection type
    all_data <- summarise(biocol_data, 
                          Blood = sum(blood),
                          Urine = sum(urine),
                          Mouthwash = sum(mouthwash),
                          all_three = sum(all),
                          None = sum(none)) %>%
      pivot_longer(cols = everything(), names_to = "CollectionType", values_to = "Count") %>%
      arrange(desc(Count))
    
    # Rename 'all_three_samples' to 'All Three Samples' for display
    all_data$CollectionType <- case_match(all_data$CollectionType,
                                          "all_three" ~ "All Three Samples",
                                          .default = all_data$CollectionType)
    
    #descending order of bars in the chart
    all_data$CollectionType <- factor(all_data$CollectionType, levels = all_data$CollectionType)
    
    #plot
    fig <- plot_ly(all_data, x = ~CollectionType, y = ~Count, type = 'bar', 
                   color = ~CollectionType, colors = "Paired", showlegend = TRUE) %>%
      layout(
        title = "Distribution of Biospecimen Collections By Type",
        xaxis = list(title = "", showticklabels = FALSE),
        yaxis = list(title = "Count"),
        legend = list(title = list(text = 'Collection Type')),
        margin = list(t = 50)
      )
    
    fig
  }
}
