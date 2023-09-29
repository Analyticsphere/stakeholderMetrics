#plot2-- activities by participant
{
 # project <- 'nih-nci-dceg-connect-prod-6d04'
#  dataset <- 'FlatConnect'
#  table   <- 'participants_JP'
  
  
#  sql     <- glue('SELECT * ',
#                  'FROM `{project}.{dataset}.{table}` ','', paste0("WHERE d_821247024 = '197316935' ") , 'LIMIT 10000')
  
  # Authenticate to BigQuery
 # bigrquery::bq_auth()
  
  # Download data
#  tb      <- bigrquery::bq_project_query(project, query=sql)
#  data    <- bigrquery::bq_table_download(tb, 
#                                          bigint="integer64")
  print("data downloaded from gcp")
  data = readRDS("/Users/sansalerj/Desktop/rshiny_app/GCP_local_copy.rds")
  
  #only using the variables that kelsey and i spoke about and are outlined in the excel spreadsheet. 
  #verified, no activities: 100767870=no and 878865966=no, Date: 914594314
  #survey only (all baseline surveys): 100767870=yes, Date: maximum date(d_264644252, d_770257102, d_832139544, d_517311251)
  #blood only: 100767870=no and 878865966=yes, Date:	minimum date(d_173836415_d_266600170_d_561681068, d_173836415_d_266600170_d_982213346, d_173836415_d_266600170_d_398645039, d_173836415_d_266600170_d_822274939)
  #survey + blood : 100767870=yes and 878865966=yes, Date:maximum date(blood only date, survey only date)
  d2 <- data.frame(verified_no_activities = ifelse(data$d_100767870 == 104430631 & data$d_878865966 == 104430631, 1, 0),
                   survey_only = ifelse(data$d_100767870 == 353358909, 1, 0),
                   blood_only = ifelse(data$d_100767870 == 104430631 & data$d_878865966== 353358909, 1, 0), 
                   d_264644252=as.Date(data$d_264644252),d_770257102= as.Date(data$d_770257102),d_832139544=as.Date(data$d_832139544),d_517311251=as.Date(data$d_517311251),
                   d_914594314=as.Date(data$d_914594314), d_173836415_d_266600170_d_561681068=as.Date(data$d_173836415_d_266600170_d_561681068),
                   d_173836415_d_266600170_d_982213346=as.Date(data$d_173836415_d_266600170_d_982213346), d_173836415_d_266600170_d_822274939=as.Date(data$d_173836415_d_266600170_d_822274939),
                   d_173836415_d_266600170_d_398645039= as.Date(data$d_173836415_d_266600170_d_398645039),
                   race = case_when(data$state_d_684926335 == 635279662 | data$state_d_849518448 == 768826601 | data$state_d_119643471 == 635279662 ~ "White" ,#768826601
                                    data$state_d_684926335 %in% c(232334767,401335456) | data$state_d_849518448 == 181769837 |
                                      data$state_d_119643471 == 232334767| data$state_d_119643471  ==211228524|data$state_d_119643471 ==308427446| data$state_d_119643471  ==432722256| data$state_d_119643471  ==232663805| data$state_d_119643471  ==785578696| data$state_d_119643471  ==200929978| data$state_d_119643471  ==490725843| data$state_d_119643471  == 965998904 ~ "Other", #181769837
                                    data$state_d_684926335 == 178420302  | data$state_d_849518448 ==178420302 | data$state_d_119643471 == 986445321| data$state_d_119643471  == 746038746| data$state_d_119643471  == 178420302 | (is.na(data$state_d_119643471) & data$d_827220437== 657167265) ~ "Unknown"),
                   age = case_when(data$state_d_934298480 == 124276120 ~ "40-45",
                                   data$state_d_934298480 == 450985724 ~ "46-50",
                                   data$state_d_934298480 == 363147933 ~ "51-55",
                                   data$state_d_934298480 == 636706443 ~ "56-60",
                                   data$state_d_934298480 == 771230670 ~ "61-65"))
  print("d2 created, now creating variables")
  #by age
  #using the verification date as the censor date
  d2$age_date <- as.Date(cut(d2$d_914594314,"week"))
  d2$a <- 1
  
  agg <- aggregate(d2$a, list(d2$age_date, d2$age), FUN = sum)
  agg <- reshape(agg, idvar = "Group.1", timevar = "Group.2", direction = "wide")
  colnames(agg) <- c("date", "num_40_45", "num_46_50", "num_51_55", "num_56_60", "num_61_65")
  agg$date <- as.Date(agg$date)
  a <- agg[order(agg$date),]
  
  ######################################################################
  #week date
  weekDate <- as.Date(cut(Sys.Date(), "week"))
histogram()


# Create a histogram using Plotly
plotly_histogram <- plot_ly(d2, x = ~age, type = 'histogram', marker = list(color = 'lightblue'))

# Customize the layout
plotly_histogram <- plotly_histogram %>%
  layout(
    title = "Ages of Verified Participants",
    xaxis = list(title = "Age"),
    yaxis = list(title = "Frequency"),
    showlegend = FALSE,
    legend = list(x = 0.8, y = 1)
  )

# Display the Plotly histogram
plotly_histogram


# Plot the histogram
hist(hist_data, main = "Histogram of All Ages",
     xlab = "Age", ylab = "Frequency", col = "lightblue", border = "black")




  
}







#base age line plot
#plot2, age
output$plot2 <- renderPlot({
  data = readRDS("/Users/sansalerj/Desktop/rshiny_app/GCP_local_copy.rds")
  
  #only using the variables that kelsey and i spoke about and are outlined in the excel spreadsheet. 
  #verified, no activities: 100767870=no and 878865966=no, Date: 914594314
  #survey only (all baseline surveys): 100767870=yes, Date: maximum date(d_264644252, d_770257102, d_832139544, d_517311251)
  #blood only: 100767870=no and 878865966=yes, Date:	minimum date(d_173836415_d_266600170_d_561681068, d_173836415_d_266600170_d_982213346, d_173836415_d_266600170_d_398645039, d_173836415_d_266600170_d_822274939)
  #survey + blood : 100767870=yes and 878865966=yes, Date:maximum date(blood only date, survey only date)
  d2 <- data.frame(verified_no_activities = ifelse(data$d_100767870 == 104430631 & data$d_878865966 == 104430631, 1, 0),
                   survey_only = ifelse(data$d_100767870 == 353358909, 1, 0),
                   blood_only = ifelse(data$d_100767870 == 104430631 & data$d_878865966== 353358909, 1, 0), 
                   d_264644252=as.Date(data$d_264644252),d_770257102= as.Date(data$d_770257102),d_832139544=as.Date(data$d_832139544),d_517311251=as.Date(data$d_517311251),
                   d_914594314=as.Date(data$d_914594314), d_173836415_d_266600170_d_561681068=as.Date(data$d_173836415_d_266600170_d_561681068),
                   d_173836415_d_266600170_d_982213346=as.Date(data$d_173836415_d_266600170_d_982213346), d_173836415_d_266600170_d_822274939=as.Date(data$d_173836415_d_266600170_d_822274939),
                   d_173836415_d_266600170_d_398645039= as.Date(data$d_173836415_d_266600170_d_398645039),
                   race = case_when(data$state_d_684926335 == 635279662 | data$state_d_849518448 == 768826601 | data$state_d_119643471 == 635279662 ~ "White" ,#768826601
                                    data$state_d_684926335 %in% c(232334767,401335456) | data$state_d_849518448 == 181769837 |
                                      data$state_d_119643471 == 232334767| data$state_d_119643471  ==211228524|data$state_d_119643471 ==308427446| data$state_d_119643471  ==432722256| data$state_d_119643471  ==232663805| data$state_d_119643471  ==785578696| data$state_d_119643471  ==200929978| data$state_d_119643471  ==490725843| data$state_d_119643471  == 965998904 ~ "Other", #181769837
                                    data$state_d_684926335 == 178420302  | data$state_d_849518448 ==178420302 | data$state_d_119643471 == 986445321| data$state_d_119643471  == 746038746| data$state_d_119643471  == 178420302 | (is.na(data$state_d_119643471) & data$d_827220437== 657167265) ~ "Unknown"),
                   age = case_when(data$state_d_934298480 == 124276120 ~ "40-45",
                                   data$state_d_934298480 == 450985724 ~ "46-50",
                                   data$state_d_934298480 == 363147933 ~ "51-55",
                                   data$state_d_934298480 == 636706443 ~ "56-60",
                                   data$state_d_934298480 == 771230670 ~ "61-65"))
  print("d2 created, now creating variables")
  #by age
  #using the verification date as the censor date
  d2$age_date <- as.Date(cut(d2$d_914594314,"week"))
  d2$a <- 1
  
  agg <- aggregate(d2$a, list(d2$age_date, d2$age), FUN = sum)
  agg <- reshape(agg, idvar = "Group.1", timevar = "Group.2", direction = "wide")
  colnames(agg) <- c("date", "num_40_45", "num_46_50", "num_51_55", "num_56_60", "num_61_65")
  agg$date <- as.Date(agg$date)
  a <- agg[order(agg$date),]
  
  ######################################################################
  #week date
  weekDate <- as.Date(cut(Sys.Date(), "week"))
  
  plot(x = a$date, y = a$num_40_45, type = 'l', col = 1, ylim = c(0,100), ylab = "Number of Verified Participants", xlab = "Verified Date", main = "Number of Verified Participants by Age", lwd = 3)
  lines(x = a$date, y = a$num_46_50, col = 2, lwd = 3)
  lines(x = a$date, y = a$num_51_55, col = 3, lwd = 3)
  lines(x = a$date, y = a$num_56_60, col = 4, lwd = 3)
  legend("topleft", legend = c("40-45", "46-50", "51-55","56-60"), col = c(1,2,3,4), lty = c(1,1,1,1), lwd = 3)
})