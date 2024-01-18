rm(list=ls())
library(shiny)
library(shinythemes)
library(httr)
library(bigrquery)
library(glue)
library(plotly)
library(shinydashboard)
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
library(expss) #this is used in the activity plot

server <- function(input, output, session){
  
  data_project <- 'nih-nci-dceg-connect-bq2-prod'
  data_dataset <- 'StakeHolderMetrics_RS'
  data_table   <- 'complete_table'
  
  combined_query <- glue("SELECT * FROM `", data_project, ".", data_dataset, ".", data_table, "`" , sep = " ")
  # Download data
  data <- bq_table_download(bq_project_query(data_project, query = combined_query), bigint = "integer64")

  
  #plot1
  #source("/Users/sansalerj/Desktop/local_app/activities_plot.R", local = TRUE)
  source("~/Desktop/local_app_SMDB_data/activity_plot_bq2.R", local = TRUE)
  output$plot1 <- renderPlotly({activity_plot_2(activity_data = data, selected_hospital = input$siteFilter, selected_sex = input$sexFilter)})
  
  #plot 2
  source("/Users/sansalerj/Desktop/local_app_SMDB_data/base_age_plot.R", local = TRUE)
  output$plot2 <- renderPlotly({age_plot(age_data = data, selected_hospital = input$siteFilter, selected_sex = input$sexFilter)})
  
  #using the BQ2 data for today, theres something wrong w the other plot 
  source("/Users/sansalerj/Desktop/local_app_SMDB_data/race_plot2_SMDB.R", local = TRUE)
  output$plot3 <- renderPlotly({race_plot2(race_data = data, selected_hospital = input$siteFilter, selected_sex = input$sexFilter)})
  
  #using the BQ2 data for today, theres something wrong w the other plot 
  source("/Users/sansalerj/Desktop/local_app_SMDB_data/male_female_distribution.R", local = TRUE)
  output$plot4 <- renderPlotly({sex_distribution(sex_data = data, selected_hospital = input$siteFilter)})
  
  # Reactive expression for title
  titleReactive <- reactive({
    selectedHospital <- input$siteFilter
    selectedSex <- input$sexFilter
    glue("Interactive Plot Dashboard - Site: {selectedHospital}, Gender: {selectedSex}")
  })
  
  # Send the reactive title to UI
  output$dynamicTitle <- renderText({titleReactive()})

  }


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Connect for Cancer Plot Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plot Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Filters", tabName = "filters", icon = icon("sliders"),
               badgeLabel = "new", badgeColor = "green"),
      menuItem("Advanced Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Settings", tabName = "settings", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems(
      # Dashboard Tab
      tabItem(tabName= "dashboard",
              h2(uiOutput("dynamicTitle")),
              fluidRow(
                box(plotlyOutput("plot1", height = 350), width = 6)),
              fluidRow(
                box(plotlyOutput("plot2", height = 350), width = 6)),
              fluidRow(
                box(plotlyOutput("plot3", height = 350), width = 10)),
              fluidRow(
                box(plotlyOutput("plot4", height = 450), width = 6)),
              ),
      
      # Filters Tab
      tabItem(tabName = "filters",
              fluidRow(
                box(title = "Filter Options", status = "primary", solidHeader = TRUE,
                    selectInput("siteFilter", "Choose Site:",
                                choices = c("All Hospitals" = ".",
                                            "HealthPartners"= 531629870,
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
                                            selected = "All Hospitals"),
                    actionButton("applyHospitalFilter", "Apply Filters")
                ),
                box(title = "Filter Options", status = "primary", solidHeader = TRUE,
                    selectInput("sexFilter", "Choose Gender:",
                                choices = c("All" = ".",
                                            "Male"= 654207589,
                                            "Female"=536341288, 
                                            "Other"=576796184),
                                selected = "All"),
                    actionButton("applySexFilters", "Apply Filters")
                )
              )),

      # Advanced Analysis Tab
      tabItem(tabName = "analysis",
              h2("Advanced Data Analysis"),
              fluidRow(
                box(title = "Analysis Tools", status = "warning", solidHeader = TRUE,
                    p("Analysis tools and visualizations will be displayed here.")
                )
              )),
      
      # Settings Tab
      tabItem(tabName = "settings",
              h2("Settings"),
              fluidRow(
                box(title = "Configuration", status = "info", solidHeader = TRUE,
                    p("Dashboard settings and configurations.")
                )
              ))
    )
  )
)


shinyApp(ui, server)