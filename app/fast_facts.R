fast_facts <- function(verified_data = data) {
  library(dplyr)
  library(lubridate)
  
  # Convert Verified_wkdate to Date format
  verified_data$verified_date <- as.Date(verified_data$Verified_wkdate, format = "%Y-%m-%d")
  
  current_month <- as.Date("2024-03-01")
  
  # Filter data for the current month
  current_verified_data <- filter(verified_data, verified_date >= current_month)
  
  # Number of newly verified participants
  n_verified <- length(unique(current_verified_data$Connect_ID))
  
  # Number of participants verified this month
  fact_1 <- paste0("This month, we have verified ", n_verified, " new participants.")
  
  # Number of new male and female participants
  n_male <- n_distinct(filter(current_verified_data, sex == "Male")$Connect_ID)
  n_female <- n_distinct(filter(current_verified_data, sex == "Female")$Connect_ID)
  fact_3 <- paste0("Among them, ", n_male, " are male and ", n_female, " are female.")
  
  # Determine the most used active campaign type
  campaign_counts <- table(current_verified_data$active_camptype)
  most_common_campaign_code <- as.numeric(names(which.max(campaign_counts)))
  
  # Map campaign code to campaign name
  campaign_names <- c("Random" = 926338735,
                      "Screening appointment" = 348281054,
                      "Non-screening appointment" = 324692899,
                      "Demographic Group" = 351257378,
                      "Aging out of study" = 647148178,
                      "Geographic group" = 834544960,
                      "Post-Screening Selection" = 682916147,
                      "Technology adapters" = 153365143,
                      "Low-income/health professional shortage areas" = 663706936,
                      "Research Registry" = 208952854,
                      "Pop up" = 296312382,
                      "Other" = 181769837,
                      "None of these apply" = 398561594,
                      "NA/Unknown" = NA)
  
  most_common_campaign_name <- names(campaign_names[campaign_names == most_common_campaign_code])
  
  # Number of participants recruited by the most common campaign
  n_recruited_by_campaign <- max(campaign_counts)
  
  # Second fact
  fact_2 <- paste0("The most active campaign type was '", most_common_campaign_name, 
                   "', recruiting ", n_recruited_by_campaign, " participants.")
  
  # Determine the most frequently reported income
  income_counts <- table(current_verified_data$income)
  income_counts <- income_counts[income_counts > 0 & !names(income_counts) %in% c("NA", "Unknown")]  # Exclude 'NA' and 'UNKNOWN'
  
  if (length(income_counts) == 0) {  # In case all are NA or UNKNOWN
    most_common_income <- "Not available"
  } else {
    most_common_income <- names(which.max(income_counts))
  }
  
  fact_4 <- paste0("The most frequently reported income among newly verified participants is '", most_common_income, "'.")
  
  # Combine and return facts
  return(c(fact_1, fact_2, fact_3, fact_4))
}
