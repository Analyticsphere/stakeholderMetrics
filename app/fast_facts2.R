fast_facts <- function(verified_data = data) {
  library(dplyr)
  library(lubridate)
  
  # Convert Verified_wkdate to Date format
  verified_data$verified_date <- as.Date(verified_data$Verified_wkdate, format = "%Y-%m-%d")
  current_month <- as.Date("2024-06-01")
  current_verified_data <- filter(verified_data, verified_date >= current_month)
  
  # Number of newly verified participants
  n_verified <- length(unique(current_verified_data$Connect_ID))
  
  # Number of new male and female participants
  n_male <- n_distinct(filter(current_verified_data, sex == "Male")$Connect_ID)
  n_female <- n_distinct(filter(current_verified_data, sex == "Female")$Connect_ID)
  
  # Define campaign_names within the function
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
  
  # Determine the most used active campaign type
  campaign_counts <- table(current_verified_data$active_camptype)
  most_common_campaign_code <- as.numeric(names(which.max(campaign_counts)))
  most_common_campaign_name <- names(campaign_names[campaign_names == most_common_campaign_code])
  n_recruited_by_campaign <- max(campaign_counts)
  
  # Determine the most frequently reported income
  income_counts <- table(current_verified_data$income)
  income_counts <- income_counts[income_counts > 0 & !names(income_counts) %in% c("NA", "Unknown")]
  if (length(income_counts) == 0) {
    most_common_income <- "Not available"
  } else {
    most_common_income <- names(which.max(income_counts))
    # Remove '/year' from the most common income
    most_common_income <- gsub("/year", "", most_common_income)
  }
  
  return(list(
    total_verified = n_verified,
    male_verified = n_male,
    female_verified = n_female,
    common_campaign = paste(most_common_campaign_name, "(", n_recruited_by_campaign, "participants)"),
    common_income = most_common_income
  ))
}
