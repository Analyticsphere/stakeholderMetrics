
#this is to allow for a variety of filtering options (site/race etc.)
activity_plot <- function(activity_data = data){
library(tidyverse) 
library(dplyr) 
library(plotly)

  # Check if the filtered dataset is empty
  if (nrow(activity_data) <= 9) {
    # Return a message indicating not enough data
    return(plotly::plot_ly() %>% 
             layout(title = "Not Enough Data to Display This Chart"))
  } else {
  
#aggregation
activity_data$Verified_wkdate <- as.Date(activity_data$Verified_wkdate, format = "%Y-%m-%d")  # adjust the format as per your data
activity_data$verified <- ifelse(is.na(activity_data$Verified_wkdate), 0, 1)
activity_data$blood <- ifelse(is.na(activity_data$Blood_wk), 0, 1)
activity_data$all_survey_complete <- ifelse(is.na(activity_data$CompleteDate), 0, 1)
activity_data$only_blood <- ifelse((is.na(activity_data$CompleteDate)&!is.na(activity_data$Blood_wk)), 1, 0)
activity_data$blood_and_all_survey <- ifelse((activity_data$blood ==1 & activity_data$all_survey_complete==1), 1, 0)
activity_data$only_all_survey_complete <- ifelse((activity_data$blood ==0 & activity_data$all_survey_complete==1), 1, 0)
  
  
agg <- aggregate(list(activity_data$verified, activity_data$blood,
                      activity_data$all_survey_complete, activity_data$only_blood,
                      activity_data$blood_and_all_survey,
                      activity_data$only_all_survey_complete),
                 list(verifiedcount = activity_data$Verified_wkdate) , FUN=sum)
  
colnames(agg) <- c("verified_date", "verified", "blood",
                   "all_survey_complete", "only_blood",
                   "blood_and_all_survey", "only_all_survey_complete")
  
#cumulative variables
agg$cumul_verified <- cumsum(agg$verified)
agg$cumul_blood <- cumsum(agg$blood)
agg$cumul_all_survey_complete <- cumsum(agg$all_survey_complete)
agg$cumul_only_blood <- cumsum(agg$only_blood)
agg$cumul_blood_and_all_survey <- cumsum(agg$blood_and_all_survey)
agg$cumul_only_all_survey_complete <- cumsum(agg$only_all_survey_complete)
agg$cumul_verified_no_activity <- agg$cumul_verified -
  ((agg$cumul_only_blood + agg$cumul_only_all_survey_complete) + agg$cumul_blood_and_all_survey)
  
  
# Sorting the data by VerifiedWeekDate
agg <- agg[order(as.Date(agg$verified_date)),]
  
# Melt the data frame to long format
agg <- melt(agg, id.vars = "verified_date", 
              measure.vars = c("cumul_verified", "cumul_blood", 
                               "cumul_all_survey_complete", "cumul_only_blood", 
                               "cumul_blood_and_all_survey", "cumul_only_all_survey_complete", 
                               "cumul_verified_no_activity"),
              variable.name = "activity", value.name = "value")
  
# Create a factor with specified levels and labels
agg$activity <- factor(agg$activity, 
                         levels = c("cumul_verified", "cumul_blood", "cumul_all_survey_complete", 
                                    "cumul_only_blood", "cumul_blood_and_all_survey", 
                                    "cumul_only_all_survey_complete", "cumul_verified_no_activity"),
                         labels = c("Verified", "Blood", "All Modules Complete", "Only Blood", 
                                    "Blood and All Modules", "Only All Modules", "Verified, No Activity"))
  
 # Rename the columns
colnames(agg) <- c("verified_date", "activity", "value")
  
  
#identify number of colors to use  
unique_activities <- unique(agg$activity)
n_colors <- length(unique_activities)

# Ensure you have a sufficient number of colors for your activities
cols <- select_colors(color_palette, n_colors)

# Map colors to activities to ensure consistency
color_mapping <- setNames(cols, unique_activities)

# Ensure 'verified_date' is in Date format
agg$verified_date <- as.Date(agg$verified_date)

# Generate unique monthly dates
# This finds the first date of each month present in your data
unique_monthly_dates <- seq(min(agg$verified_date), max(agg$verified_date), by="month")
unique_monthly_dates <- unique(format(unique_monthly_dates, "%Y-%m-01"))
unique_monthly_dates <- as.Date(unique_monthly_dates)


Fig_all.plotly <- plot_ly() %>%
    add_lines(data = agg, x = ~as.Date(verified_date), color = ~activity, colors = color_mapping,
              y = ~value) %>%
    layout(
      title = list(text = c("Cumulative Number of Participants by Study Activities")),
      xaxis = list(title = list(text ="Date"),
                   tickvals = unique_monthly_dates,
                   ticktext = format(unique_monthly_dates, "%y-%m-%d"),
                   showline = TRUE),
      yaxis = list(title = list(text ="Number of Participants"),
                   showline = TRUE),
      legend = list(x = 0, y = 1, traceorder = "normal"),
      font = list(family = "Noto Sans"),
      showlegend = TRUE,
      margin = list(t = 50)
    )
  
  # Print the plotly plot
  Fig_all.plotly
  }
}




