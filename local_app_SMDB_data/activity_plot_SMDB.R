#activities plot
library(reshape2)
library(bigrquery)
library(dplyr)
library(plotly)
library(glue)


#this reactive function will ensure that data from plot 1 is pulled and plotted
#before it is overwritten for plot2
activity_plot <- function(){
activity_plot_project <- 'nih-nci-dceg-connect-prod-6d04'
activity_plot_dataset <- 'StakeHolderMetrics_tmp'
activity_plot_table   <- 'figure1_activity'
activity_plot_variables <- '*'
combined_query <- glue("SELECT ", paste(activity_plot_variables, collapse = ", ")," FROM `", activity_plot_project, ".", activity_plot_dataset, ".", activity_plot_table, "`" , sep = " ")
# Download data
data <- bq_table_download(bq_project_query(activity_plot_project, query = combined_query), bigint = "integer64")

#convert dates correctly
data$VerifiedWeekDate <- as.Date(data$VerifiedWeekDate, format = "%Y-%m-%d")  # adjust the format as per your data

#complete = individuals who have completed both blood and modules
data$VerifiedNoActivityCount <- data$VerifiedCount- (data$ModuleCount+data$BloodCount - data$CompleteCount)

#survey or blood
data$SurveyOrBloodCount <- (data$BloodCount+data$ModuleCount)-data$CompleteCount




# Sorting the data by VerifiedWeekDate
data <- data[order(as.Date(data$VerifiedWeekDate)),]
  
# Replace NA with 0 for each variable
data$VerifiedCount[is.na(data$VerifiedCount)] <- 0
data$BloodCount[is.na(data$BloodCount)] <- 0
data$ModuleCount[is.na(data$ModuleCount)] <- 0
data$CompleteCount[is.na(data$CompleteCount)] <- 0
data$VerifiedNoActivityCount[is.na(data$VerifiedNoActivityCount)] <- 0
data$SurveyOrBloodCount[is.na(data$SurveyOrBloodCount)] <- 0

# Then calculate the cumulative sums
data$CumulativeVerifiedCount <- cumsum(data$VerifiedCount)
data$CumulativeBloodCount <- cumsum(data$BloodCount)
data$CumulativeModuleCount <- cumsum(data$ModuleCount)
data$CumulativeCompleteCount <- cumsum(data$CompleteCount)
data$CumulativeVerifiedNoActivityCount <- cumsum(data$VerifiedNoActivityCount)
data$CumulativeSurveyOrBloodCount <- cumsum(data$SurveyOrBloodCount)

# Melt the data frame to long format
activity_data <- melt(data, id.vars = "VerifiedWeekDate", 
                  measure.vars = c("CumulativeVerifiedCount", "CumulativeBloodCount", 
                                   "CumulativeModuleCount", "CumulativeCompleteCount", 
                                   "CumulativeVerifiedNoActivityCount", "CumulativeSurveyOrBloodCount"),
                  variable.name = "activity", value.name = "count")

# Extract unique monthly dates from data
unique_monthly_dates <- unique(as.Date(format(activity_data$VerifiedWeekDate, "%Y-%m-01")))
currentDate <- Sys.Date()
hospital_label <- "None"
#plotly plot
Fig_all.plotly <- plot_ly() %>%
  add_lines(data = activity_data, x = ~as.Date(VerifiedWeekDate), color = ~activity, colors = c("lightblue", "purple", "red", "blue", "green", "#BEBADA"),
            y = ~count) %>%
  layout(
    title = list(text = paste0("Cumulative Number of Participants by Study Activities \n as of ", currentDate, " hospital filter: ", hospital_label),
                 font = list(size = 10)),
    xaxis = list(title = "Date", tickvals = unique_monthly_dates, ticktext = format(unique_monthly_dates, "%y-%m-%d"), showline = TRUE),
    yaxis = list(title = "Number of Participants", showline = TRUE),
    legend = list(x = 0, y = 1, traceorder = "normal", font = list(family = "sans-serif", size = 12, color = "black")),
    showlegend = TRUE
  )

# Print the plotly plot
Fig_all.plotly

}




#this is to allow for a variety of filtering options (site/race etc.)
activity_plot_2 <- function(site = NA){
  activity_plot_project <- 'nih-nci-dceg-connect-prod-6d04'
  activity_plot_dataset <- 'StakeHolderMetrics_tmp'
  activity_plot_table   <- 'figure1_activity_CID'
  activity_plot_variables <- '*'
  combined_query <- glue("SELECT ", paste(activity_plot_variables, collapse = ", ")," FROM `", activity_plot_project, ".", activity_plot_dataset, ".", activity_plot_table, "`" , sep = " ")
  # Download data
  data <- bq_table_download(bq_project_query(activity_plot_project, query = combined_query), bigint = "integer64")
  
  #convert dates correctly
  data$VerifiedWeekDate <- as.Date(data$VerifiedWeekDate, format = "%Y-%m-%d")  # adjust the format as per your data
  
  #complete = individuals who have completed both blood and modules
  data$VerifiedNoActivityCount <- data$VerifiedCount- (data$ModuleCount+data$BloodCount - data$CompleteCount)
  
  #survey or blood
  data$SurveyOrBloodCount <- (data$BloodCount+data$ModuleCount)-data$CompleteCount
  
  
  
  
  # Sorting the data by VerifiedWeekDate
  data <- data[order(as.Date(data$VerifiedWeekDate)),]
  
  # Replace NA with 0 for each variable
  data$VerifiedCount[is.na(data$VerifiedCount)] <- 0
  data$BloodCount[is.na(data$BloodCount)] <- 0
  data$ModuleCount[is.na(data$ModuleCount)] <- 0
  data$CompleteCount[is.na(data$CompleteCount)] <- 0
  data$VerifiedNoActivityCount[is.na(data$VerifiedNoActivityCount)] <- 0
  data$SurveyOrBloodCount[is.na(data$SurveyOrBloodCount)] <- 0
  
  # Then calculate the cumulative sums
  data$CumulativeVerifiedCount <- cumsum(data$VerifiedCount)
  data$CumulativeBloodCount <- cumsum(data$BloodCount)
  data$CumulativeModuleCount <- cumsum(data$ModuleCount)
  data$CumulativeCompleteCount <- cumsum(data$CompleteCount)
  data$CumulativeVerifiedNoActivityCount <- cumsum(data$VerifiedNoActivityCount)
  data$CumulativeSurveyOrBloodCount <- cumsum(data$SurveyOrBloodCount)
  
  # Melt the data frame to long format
  activity_data <- melt(data, id.vars = "VerifiedWeekDate", 
                        measure.vars = c("CumulativeVerifiedCount", "CumulativeBloodCount", 
                                         "CumulativeModuleCount", "CumulativeCompleteCount", 
                                         "CumulativeVerifiedNoActivityCount", "CumulativeSurveyOrBloodCount"),
                        variable.name = "activity", value.name = "count")
  
  # Extract unique monthly dates from data
  unique_monthly_dates <- unique(as.Date(format(activity_data$VerifiedWeekDate, "%Y-%m-01")))
  currentDate <- Sys.Date()
  hospital_label <- "None"
  #plotly plot
  Fig_all.plotly <- plot_ly() %>%
    add_lines(data = activity_data, x = ~as.Date(VerifiedWeekDate), color = ~activity, colors = c("lightblue", "purple", "red", "blue", "green", "#BEBADA"),
              y = ~count) %>%
    layout(
      title = list(text = paste0("Cumulative Number of Participants by Study Activities \n as of ", currentDate, " hospital filter: ", hospital_label),
                   font = list(size = 10)),
      xaxis = list(title = "Date", tickvals = unique_monthly_dates, ticktext = format(unique_monthly_dates, "%y-%m-%d"), showline = TRUE),
      yaxis = list(title = "Number of Participants", showline = TRUE),
      legend = list(x = 0, y = 1, traceorder = "normal", font = list(family = "sans-serif", size = 12, color = "black")),
      showlegend = TRUE
    )
  
  # Print the plotly plot
  Fig_all.plotly
  
}





                                            