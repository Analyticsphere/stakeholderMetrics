#all base plot shiny app
## app.R ##
rm(list=ls())
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
                box(plotlyOutput("plot3", height =750)))
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)

server <- function(input, output) {
  
  #download GCP data once for all plots
  #first define the variables we want and from what tables they reside
  source("/Users/sansalerj/Desktop/rshiny_app/get_gcp_data.R")
  #combining variables for activity plot and age plot
  {modules <- c("d_100767870", "d_949302066", "d_536735468", "d_663265240", "d_976570371", "d_517311251", "d_832139544", "d_264644252", "d_770257102")
  bio.col <- c("d_684635302", "d_878865966", "d_167958071", "d_173836415_d_266600170_d_915179629", "d_173836415_d_266600170_d_718172863",
               "d_173836415_d_266600170_d_592099155", "d_173836415_d_266600170_d_561681068", "d_173836415_d_266600170_d_847159717",
               "d_173836415_d_266600170_d_448660695", "d_173836415_d_266600170_d_139245758", "d_173836415_d_266600170_d_541311218",
               "d_173836415_d_266600170_d_224596428", "d_173836415_d_266600170_d_740582332", "d_173836415_d_266600170_d_982213346",
               "d_173836415_d_266600170_d_398645039", "d_173836415_d_266600170_d_822274939")
  clc.bldtm <- c("d_173836415_d_266600170_d_769615780", "d_173836415_d_266600170_d_822274939", "d_173836415_d_266600170_d_398645039",
                 "d_173836415_d_266600170_d_982213346", "d_173836415_d_266600170_d_740582332")
  clc.urinetm <- c("d_173836415_d_266600170_d_139245758", "d_173836415_d_266600170_d_224596428", "d_173836415_d_266600170_d_541311218",
                   "d_173836415_d_266600170_d_939818935", "d_173836415_d_266600170_d_740582332")
    age <- c("state_d_934298480", "d_914594314")
    race <- c("d_821247024", "d_914594314",  "d_827220437","d_512820379","d_949302066" , "d_517311251")
  
   var.list <- c("token", "Connect_ID", "d_821247024", "d_914594314", "d_512820379", "state_d_158291096", "d_471593703", "d_827220437",
                "d_130371375_d_266600170_d_787567527", "d_130371375_d_266600170_d_731498909", bio.col, modules, age, race)
  # Define the variables from the second query
  project <- 'nih-nci-dceg-connect-prod-6d04'
  dataset <- 'FlatConnect'
  table <- 'participants_JP'
  
  variables = paste(var.list, collapse = ", ")
  }  
  #filter default is set to where connect_id is not missing and d_821247024 = 197316935
  get_data <- get_gcp_data(variables, dataset, table, project = project, filter)
  
  #activities by participant
  source("/Users/sansalerj/Desktop/recreating jing plot/activities_plot.R")
  output$plot1 <- renderPlotly({activities_plot(data = get_data)})
  
  source("/Users/sansalerj/Desktop/rshiny_app/base_age_plot.R")
  output$plot2 <- renderPlotly({age_plot(data = get_data)})
  
#  source("/Users/sansalerj/Desktop/rshiny_app/race_plot.R")
#  output$plot3 <- renderPlotly({race_plot()})
  

  
}

shinyApp(ui, server)