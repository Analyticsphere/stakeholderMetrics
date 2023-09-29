#all base plot shiny app
## app.R ##
library(shinydashboard)
library(bigrquery)
library(glue)
library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(tidyr)
library(magrittr)
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
                box(plotlyOutput("plot1", height = 450)),
                box(plotlyOutput("plot2", height = 450)),
                box(plotOutput("plot3", height =450)),
                box(plotlyOutput("plot4", height =450))
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
#plot1 activities by verified individual
 # source("/Users/sansalerj/Desktop/rshiny_app/activities_by_participant.R")
 # output$plot1 <- renderPlot({
 #   activities_by_participant()
#  })
  

  #activities by participant
  output$plot1 <- renderPlotly({  data = readRDS("/Users/sansalerj/Desktop/rshiny_app/GCP_local_copy.rds")
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

  #aggregate verified participants
  verified_no_activities_by_date <- data.frame(aggregate(d2$verified_no_activities, list(d2$verified_no_activities_date), FUN = sum))
  verified_no_activities_by_date <- verified_no_activities_by_date[verified_no_activities_by_date$Group.1 > "2019-01-01" &verified_no_activities_by_date$Group.1 <as.Date(cut(Sys.Date(), "week")),]
  verified_no_activities_by_date$type <- "Verified, No Activities"
  colnames(verified_no_activities_by_date) <- c("date", "number_participants", "type")
  #aggregate survey only individuals
  survey_only_by_date <- aggregate(d2$survey_only, list(d2$survey_only_date), FUN = sum)
  survey_only_by_date <- survey_only_by_date[survey_only_by_date$Group.1 > "2019-01-01" & survey_only_by_date$Group.1 < as.Date(cut(Sys.Date(), "week")),]
  survey_only_by_date$type <- "Survey Only"
  colnames(survey_only_by_date) <- c("date", "number_participants", "type")

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
  a$number_participants.x <- NULL
  a$survey_only_number <- a$number_participants.y
  a$number_participants.y <- NULL
  a$type.x <- NULL
  a$type.y <- NULL
  all <- merge(a, blood_only_by_date, by= "date")
  all$blood_only_number <- all$number_participants 
  all$number_participants  <- NULL
  all$type <- NULL
  all$date <- as.Date(all$date)
  #merge all types together 
  
  ######################################################################
  #week date
  weekDate <- as.Date(cut(Sys.Date(), "week"))
  print("plotting")
  #output$plot5 <- renderPlot({)})
  fig <- plot_ly(all, x = ~date, y = ~verified_no_activities_number, name = "verified no activities", type = 'scatter', mode = 'lines')   %>% 
    add_trace(y = ~survey_only_number, x = ~date, name = "survey only", mode = 'lines') %>%
    add_trace(y = ~blood_only_number, x = ~date, name = "blood only", mode = 'lines') %>%
    layout(title = "Activities of Verified Individuals",
           xaxis = list(title = "Reported Activity Completion Date"),
           yaxis = list (title = "Number of Verified Individuals"))
  })
  
  output$plot2 <- renderPlotly({
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
    
  })
  
  output$plot3 <- renderPlot({
    dt_all_races_summary=readRDS("/Users/sansalerj/Desktop/rshiny_app/race_summary_data.rds")
    library(RColorBrewer)
    library(ggrepel)
    #display.brewer.all(colorblindFriendly = TRUE) 
    mycolors <- c("#053061","#999999","#F16913","#FD8D3C","#FFD92F","#0072B2","#009E73","grey42","plum3","darkorchid4", "green4")
    names(mycolors) <- levels(dt_all_races_summary$race)
    
    print("creating race_m1plot")
    race_M1plot <- dt_all_races_summary %>%
      mutate(csum = rev(cumsum(rev(n))), 
             pos = n/2 + lead(csum, 1),
             pos = if_else(is.na(pos), n/2, pos),
             percentage = n/sum(n)) %>% 
      ggplot(aes(x = "", y = n, fill = fct_inorder(race))) + 
      scale_fill_manual(values = mycolors,name = "Race Ethnicity") +
      scale_colour_manual(values= mycolors) +
      labs(x = "", y = "", title = "Race/Ethnicity of Participants Who Completed BOH Section of First Survey",
           fill = "race_ethnic") +   
      geom_col(width = 3.5, color = 1) +
      geom_label_repel(aes(y = pos,
                           label = pct, 
                           fill = race),
                       size = 3,color="white",
                       nudge_x = 3,
                       show.legend = FALSE) +
      labs(  fill = "Subtype" ) +
      coord_polar(theta = "y") +   theme_void() 
    race_M1plot
  })
  
  source("/Users/sansalerj/Desktop/recreating jing plot/activities_plot.R")
  output$plot4 <- renderPlotly({activities_plot()})
  
}

shinyApp(ui, server)