## app.R ##
library(shinydashboard)
library(bigrquery)
library(glue)

library(bigrquery)
library(glue)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(tidyr)
library(magrittr)
library(plyr)
library(lubridate)
library(purrr)
library(reshape2)
library(forcats)
library(ggrepel)
library(stringr)


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                box(plotOutput("plot2", height = 250)),
                box(plotOutput("plot3", height = 250)),
                box(plotOutput("plot4", height = 250)),
                box(plotlyOutput("plot5", height = 450)),
                box(plotlyOutput("plot6", height = 450)),
                box(plotOutput("plot7", height = 450)),
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)

server <- function(input, output) {
  #plot1
  {  #COVID vaccination status
    project <- 'nih-nci-dceg-connect-dev'
    dataset <- 'FlatConnect'
    table   <- 'bioSurvey_v1_JP'
    sql     <- glue('SELECT D_890156588 ',
                    'FROM `{project}.{dataset}.{table}` ',
                    'WHERE Connect_ID IS NOT NULL ',
                    'LIMIT 100')
    
    # Authenticate to BigQuery
    bigrquery::bq_auth()
    2
    
    # Download data
    tb      <- bigrquery::bq_project_query(project, query=sql)
    data    <- bigrquery::bq_table_download(tb, 
                                            bigint="integer64", 
                                            page_size=1000)
    
    vax_status <- data.frame(vax_status = as.numeric(data$D_890156588))
    output$plot1 <- renderPlot({ hist(vax_status$vax_status, main = c("Histogram of COVID Vaccination Status"),
                                      xlab = "Yes/No Vaccine Status")})    
  }
  #plot2
  {
    project <- 'nih-nci-dceg-connect-dev'
    dataset <- 'FlatConnect'
    table   <- 'clinicalBioSurvey_v1_JP'
    sql     <- glue('SELECT D_860011428 ',
                    'FROM `{project}.{dataset}.{table}` ',
                    'WHERE Connect_ID IS NOT NULL ',
                    'LIMIT 100')
    
    # Authenticate to BigQuery
    bigrquery::bq_auth()
    2
    
    # Download data
    tb      <- bigrquery::bq_project_query(project, query=sql)
    data    <- bigrquery::bq_table_download(tb, 
                                            bigint="integer64", 
                                            page_size=1000)
    num_covid_inf <- data.frame(num_covid_inf=as.integer(data$D_860011428))
    output$plot2 <- renderPlot({ hist(num_covid_inf$num_covid_inf, main = c("Histogram of COVID Infections"),
                                      xlab = "Number of COVID case per Individual")})
  }
  #plot3
  {  #notification data
    project <- 'nih-nci-dceg-connect-stg-5519'
    dataset <- 'FlatConnect'
    table   <- 'notifications_JP'
    sql     <- glue('SELECT notification_time, attempt, notificationType ',
                    'FROM `{project}.{dataset}.{table}` ',
                    'WHERE token IS NOT NULL ',
                    'LIMIT 100')
    
    # Authenticate to BigQuery
    bigrquery::bq_auth()
    2
    
    # Download data
    tb      <- bigrquery::bq_project_query(project, query=sql)
    data    <- bigrquery::bq_table_download(tb, 
                                            bigint="integer64", 
                                            page_size=1000)
    notifications <- data.frame(notification_time=as.Date(data$notification_time), 
                                attempt = as.numeric(substr(data$attempt,1,1)), 
                                notification_type = data$notificationType)
    br <- unique(notifications$notification_time)
    
    output$plot3 <- renderPlot({ hist(notifications$notification_time, main = c("Histogram of Notification Times"),
                                      labels = FALSE, breaks = length(br), xlab = "Dates")})  
  }
  #plot4
  {  output$plot4 <- renderPlot({ hist(notifications$attempt, main = c("Histogram of Notification Attempts"),
                                       labels = FALSE, breaks = c(0,1,2,3), xlab = "Attempt Number")})  
  }  
  #plot5 
  {
    project <- 'nih-nci-dceg-connect-prod-6d04'
    dataset <- 'FlatConnect'
    table   <- 'participants_JP'
    
    
    sql     <- glue('SELECT * ',
                    'FROM `{project}.{dataset}.{table}` ','', paste0("WHERE d_821247024 = '197316935' ") , 'LIMIT 500')
    
    # Authenticate to BigQuery
    bigrquery::bq_auth()
    
    # Download data
    tb      <- bigrquery::bq_project_query(project, query=sql)
    data    <- bigrquery::bq_table_download(tb, 
                                            bigint="integer64")
    print("data downloaded from gcp")
    
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
    
    
    d2$survey_and_blood <- ifelse(d2$survey_only ==1 & d2$blood_only ==1, 1, 0)
    d2$verified_no_activities_date <- if_else(d2$verified_no_activities ==1, as.Date(cut(d2$d_914594314,"week")), as.Date("1970-01-01"))
    d2$survey_date <- (pmax(as.Date(cut(d2$d_264644252, "week")),
                            as.Date(cut(d2$d_770257102, "week")),
                            as.Date(cut(d2$d_832139544, "week")),
                            as.Date(cut(d2$d_517311251,"week")), na.rm = TRUE))
    
    d2$blood_date <- (pmin(as.Date(cut(d2$d_173836415_d_266600170_d_561681068, "week")),
                           as.Date(cut(d2$d_173836415_d_266600170_d_982213346, "week")),
                           as.Date(cut(d2$d_173836415_d_266600170_d_398645039, "week")),
                           as.Date(cut(d2$d_173836415_d_266600170_d_822274939, "week")),na.rm=TRUE))
    
    d2$s_or_b_date <- (pmax(as.Date(cut(d2$blood_date, "week")), as.Date(cut(d2$survey_date, "week")), na.rm=TRUE))
    
    d2$survey_only_date <- if_else(d2$survey_only==1 & d2$blood_only ==0,d2$survey_date ,as.Date("1970-01-01"))
    d2$blood_only_date <- if_else(d2$blood_only==1 & d2$survey_only ==0,d2$blood_date ,as.Date("1970-01-01"))
    d2$survey_and_blood_date <- if_else(d2$blood_only ==1 & d2$survey_only ==1, d2$s_or_b_date,as.Date("1970-01-01"))
    print("done making dates, now aggregating variables")
    
    #aggregate verified participants
    verified_no_activities_by_date <- data.frame(aggregate(d2$verified_no_activities, list(d2$verified_no_activities_date), FUN = sum))
    verified_no_activities_by_date <- verified_no_activities_by_date[verified_no_activities_by_date$Group.1 > "2019-01-01" &verified_no_activities_by_date$Group.1 <as.Date(cut(Sys.Date(), "week")),]
    verified_no_activities_by_date$type <- "Verified, No Activities"
    colnames(verified_no_activities_by_date) <- c("date", "number_participants", "type")
    print("verified_no_activities_by_date done")
    #aggregate survey only individuals
    survey_only_by_date <- aggregate(d2$survey_only, list(d2$survey_only_date), FUN = sum)
    survey_only_by_date <- survey_only_by_date[survey_only_by_date$Group.1 > "2019-01-01" & survey_only_by_date$Group.1 < as.Date(cut(Sys.Date(), "week")),]
    survey_only_by_date$type <- "Survey Only"
    colnames(survey_only_by_date) <- c("date", "number_participants", "type")
    print("survey_only_by_date done")
    
    #aggregate blood only individuals
    blood_only_by_date <- aggregate(d2$blood_only, list(d2$blood_only_date), FUN = sum)
    blood_only_by_date <- blood_only_by_date[blood_only_by_date$Group.1 > "2019-01-01" & blood_only_by_date$Group.1 < as.Date(cut(Sys.Date(), "week")),]
    blood_only_by_date$type <- "Blood Only"
    colnames(blood_only_by_date) <- c("date", "number_participants", "type")
    #aggregate survey + blood individuals
    #all <- rbind(verified_no_activities_by_date, survey_only_by_date, blood_only_by_date)
    #merged
    a <- merge(verified_no_activities_by_date, survey_only_by_date, by = "date")
    a$verified_no_activities_number <- a$number_participants.x
    a$survey_only_number <- a$number_participants.y
    all <- merge(a, blood_only_by_date, by = "date")
    all$blood_only_number <- all$number_participants 
    print("merged")
    #merge all types together 
    
    ######################################################################
    #week date
    weekDate <- as.Date(cut(Sys.Date(), "week"))
    print("plotting")
    #output$plot5 <- renderPlot({ggplot(all, aes(x = date, y = number_participants)) +
    #  geom_line(aes(color = type))+
    #  labs(title=str_wrap(paste("Validated Participant Activity Completion by Type and Date, as of:",weekDate, sep=" "),  60),
    #       x = "Activity Completion Date (weekly)", y = "Number of Validated Participants") +
    #  theme(panel.background = element_blank(),
    #        legend.title = element_text(size=8),
    #        legend.position = "left",
    #        axis.line = element_line(linewidth = 0.2),
    #        axis.text.x = element_text(hjust = 0.5,size = 8, face = "bold"),  plot.title = element_text(hjust = 0.5,size = 12, face = "bold"))})
    output$plot5 <- renderPlotly({
     plot_ly(all, x=~date, y = ~verified_no_activities_number, name = "verified no activities", type = 'scatter', mode = 'lines')%>% 
        add_trace(y = ~survey_only_number, name = "survey only", type = 'scatter', mode = 'lines')%>%
        add_trace(y = ~blood_only_number, name = "blood only", type = 'scatter', mode = 'lines')
      })
    
  }
  #plot6
  {
    project <- 'nih-nci-dceg-connect-prod-6d04'
    dataset <- 'FlatConnect'
    table   <- 'participants_JP'
    sql     <- glue('SELECT * ',
                    'FROM `{project}.{dataset}.{table}` ','', paste0("WHERE d_821247024 = '197316935' ") , 'LIMIT 2000')
    # Authenticate to BigQuery
    bigrquery::bq_auth()
    # Download data
    tb      <- bigrquery::bq_project_query(project, query=sql)
    data    <- bigrquery::bq_table_download(tb, 
                                            bigint="integer64")
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
    #verified participants by race
    #using the verification date as the censor date
    d2$race_date <- as.Date(cut(d2$d_914594314,"week"))
    #aggregate data by week, count the number of races present
    #aggregate verified participants
    d2$white <- ifelse(d2$race=="White", 1, 0)
    white_by_date <- data.frame(aggregate(d2$white, list(d2$race_date), FUN=sum))
    white_by_date <- white_by_date[white_by_date$Group.1 > "2019-01-01" &white_by_date$Group.1 <as.Date(cut(Sys.Date(), "week")),]
    white_by_date$race <- "White"
    colnames(white_by_date) <- c("date", "number_participants", "race")
    #other
    d2$other <- ifelse(d2$race=="Other", 1, 0)
    other_by_date <- data.frame(aggregate(d2$other, list(d2$race_date), FUN=sum))
    other_by_date <- other_by_date[other_by_date$Group.1 > "2019-01-01" &other_by_date$Group.1 <as.Date(cut(Sys.Date(), "week")),]
    other_by_date$race <- "Other"
    colnames(other_by_date) <- c("date", "number_participants", "race")
    #unknown
    d2$unknown <- ifelse(d2$race=="Unknown", 1, 0)
    unknown_by_date <- data.frame(aggregate(d2$unknown, list(d2$race_date), FUN=sum))
    unknown_by_date <- unknown_by_date[unknown_by_date$Group.1 > "2019-01-01" &unknown_by_date$Group.1 <as.Date(cut(Sys.Date(), "week")),]
    unknown_by_date$race <- "Unknown"
    colnames(unknown_by_date) <- c("date", "number_participants", "race")
    #aggregate survey + blood individuals
    #all <- rbind(white_by_date, other_by_date, unknown_by_date)
    #merge all types together 
    a <- merge(white_by_date, other_by_date, by = "date")
    a$white_participants <- a$number_participants.x 
    a$other_participants <- a$number_participants.y
    all <- merge(a, unknown_by_date, by = "date")
    all$unknown_participants <- all$number_participants
    
    
    ######################################################################
    #week date
    weekDate <- as.Date(cut(Sys.Date(), "week"))
#    output$plot6 <- renderPlot({    ggplot(all, aes(x = date, y = number_participants)) +
#        geom_line(aes(color = race))+
#        labs(title=str_wrap(paste("Validated Participant Race, as of:",weekDate, sep=" "),  60),
#             x = "Activity Completion Date (weekly)", y = "Number of Validated Participants") +
#        theme(panel.background = element_blank(),
#              legend.title = element_text(size=8),
#              legend.position = "left",
#              axis.line = element_line(linewidth = 0.2),
#              axis.text.x = element_text(hjust = 0.5,size = 8, face = "bold"),  plot.title = element_text(hjust = 0.5,size = 12, face = "bold"))})
    output$plot6 <- renderPlotly({
      plot_ly(all, x=~date, y = ~white_participants, name = "White", type = 'scatter', mode = 'lines')%>% 
        add_trace(y = ~other_participants, name = "Other", type = 'scatter', mode = 'lines')%>%
        add_trace(y = ~unknown_participants, name = "Unknown", type = 'scatter', mode = 'lines')%>%
        layout()
    })
    
     }
  #plot7
  {
    project <- 'nih-nci-dceg-connect-prod-6d04'
    dataset <- 'FlatConnect'
    table   <- 'participants_JP'
    
    
    sql     <- glue('SELECT * ',
                    'FROM `{project}.{dataset}.{table}` ','', paste0("WHERE d_821247024 = '197316935' ") , 'LIMIT 2000')
    
    # Authenticate to BigQuery
    bigrquery::bq_auth()
    2
    
    # Download data
    tb      <- bigrquery::bq_project_query(project, query=sql)
    data    <- bigrquery::bq_table_download(tb, 
                                            bigint="integer64")
    
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
    
    #by age
    #using the verification date as the censor date
    d2$age_date <- as.Date(cut(d2$d_914594314,"week"))
    d2$a <- 1
    
    agg <- aggregate(d2$a, list(d2$age_date, d2$age), FUN = sum)
    agg <- reshape(agg, idvar = "Group.1", timevar = "Group.2", direction = "wide")
    colnames(agg) <- c("date", "num_40_45", "num_46_50", "num_51_55", "num_56_60", "num_61_65")
    agg$date <- as.Date(agg$date)
    a <- agg[order(agg$date),]
    
    #aggregate data by week, count the number of races present
    #aggregate verified participants
    'd2$age_40_45 <- ifelse(d2$age=="40-45", 1, 0)
    age_40_45_date <- data.frame(aggregate(d2$age_40_45, list(d2$age_date), FUN=sum))
    age_40_45_date <- age_40_45_date[age_40_45_date$Group.1 > "2019-01-01" &age_40_45_date$Group.1 <as.Date(cut(Sys.Date(), "week")),]
    age_40_45_date$age <- "40-45"
    colnames(age_40_45_date) <- c("date", "number_participants", "age")
    #aggregate verified participants
    d2$age_46_50 <- ifelse(d2$age=="46-50", 1, 0)
    age_46_50_date <- data.frame(aggregate(d2$age_46_50, list(d2$age_date), FUN=sum))
    age_46_50_date <- age_46_50_date[age_46_50_date$Group.1 > "2019-01-01" &age_46_50_date$Group.1 <as.Date(cut(Sys.Date(), "week")),]
    age_46_50_date$age <- "46-50"
    colnames(age_46_50_date) <- c("date", "number_participants", "age")
    #aggregate verified participants
    d2$age_51_55 <- ifelse(d2$age=="51-55", 1, 0)
    age_51_55_date <- data.frame(aggregate(d2$age_51_55, list(d2$age_date), FUN=sum))
    age_51_55_date <- age_51_55_date[age_51_55_date$Group.1 > "2019-01-01" &age_51_55_date$Group.1 <as.Date(cut(Sys.Date(), "week")),]
    age_51_55_date$age <- "51-55"
    colnames(age_51_55_date) <- c("date", "number_participants", "age")
    
    #aggregate verified participants
    d2$age_56_60 <- ifelse(d2$age=="56-60", 1, 0)
    age_56_60_date <- data.frame(aggregate(d2$age_56_60, list(d2$age_date), FUN=sum))
    age_56_60_date <- age_56_60_date[age_56_60_date$Group.1 > "2019-01-01" &age_56_60_date$Group.1 <as.Date(cut(Sys.Date(), "week")),]
    age_56_60_date$age <- "56-60"
    colnames(age_56_60_date) <- c("date", "number_participants", "age")
    
    
    #aggregate verified participants
    d2$age_61_65 <- ifelse(d2$age=="61-65", 1, 0)
    age_61_65_date <- data.frame(aggregate(d2$age_61_65, list(d2$age_date), FUN=sum))
    age_61_65_date <- age_61_65_date[age_61_65_date$Group.1 > "2019-01-01" &age_61_65_date$Group.1 <as.Date(cut(Sys.Date(), "week")),]
    age_61_65_date$age <- "61-65"
    colnames(age_61_65_date) <- c("date", "number_participants", "age")
    
    #aggregate survey + blood individuals
    #all <- rbind(age_40_45_date, age_46_50_date, age_51_55_date, age_56_60_date, age_61_65_date)
    #merge all types together 
'
    
    ######################################################################
    #week date
    weekDate <- as.Date(cut(Sys.Date(), "week"))
#    output$plot7 <- renderPlot({ggplot(all, aes(x = date, y = number_participants)) +
#        geom_line(aes(color = age))+
#        labs(title=str_wrap(paste("Validated Participant Age, as of:",weekDate, sep=" "),  60),
#             x = "Verification Week", y = "Number of Validated Participants") +
#        theme(panel.background = element_blank(),
#              legend.title = element_text(size=8),
#              legend.position = "left",
#              axis.line = element_line(linewidth = 0.2),
#              axis.text.x = element_text(hjust = 0.5,size = 8, face = "bold"),  plot.title = element_text(hjust = 0.5,size = 12, face = "bold"))})
    #output$plot7 <- renderPlotly({
    #  plot_ly(a, x=~date, y = ~num_40_45, name = "40-45", type = 'scatter', mode = 'lines')%>% 
    #    add_trace(y = ~num_46_50, name = "46-50", type = 'scatter', mode = 'lines')%>%
    #    add_trace(y = ~unknown_participants, name = "Unknown", type = 'scatter', mode = 'lines')
    #})
    output$plot7 <- renderPlot({
      plot(x = a$date, y = a$num_40_45, type = 'l', ylim = c(0,125), ylab = "Number of Verified Participants", xlab = "Date")
      lines(x = a$date, y = a$num_46_50, col = "red")
      lines(x = a$date, y = a$num_51_55, col = "blue")
    })
    
  }
  
  
  
  
}

shinyApp(ui, server)