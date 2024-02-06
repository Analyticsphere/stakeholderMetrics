rm(list=ls())
library(shiny)
library(shinythemes)
library(httr)
library(bigrquery)
library(glue)
library(plotly)
library(shinydashboard)
library(glue)
library(plotly)
library(tidyverse)
library(lubridate)
library(purrr)
library(forcats)
library(expss)
library(jsonlite)
library(httr)

server <- function(input, output, session){
  
#call data once for entire dashboard
#authentication step for Posit
#this code was written by D Russ
#source("./get_authenticaion.R")
#get_authentication(service_account_key = "SERVICE_ACCT_KEY")

#clean, re-label and generate some variables to plot
source("./clean_data.R")
source("./get_data.R")
data <- clean_data(data = get_data())

wd <- "./"
source(paste0("./activity_plot.R"), local = TRUE)
output$plot1 <- renderPlotly({activity_plot(activity_data = data, selected_hospital = input$siteFilter,
                                                selected_sex = input$sexFilter, selected_age = input$ageFilter,
                                                selected_race = input$raceFilter,selected_campaign = input$campaignFilter,
                                                selected_biospec = input$biospecFilter, selected_surveycomplete = input$surveycompleteFilter)})
  
source(paste0(wd,"age_plot.R"), local = TRUE)
output$plot2 <- renderPlotly({age_plot(age_data = data, selected_hospital = input$siteFilter,
                                         selected_sex = input$sexFilter, selected_age = input$ageFilter,
                                         selected_race = input$raceFilter, selected_campaign = input$campaignFilter,
                                         selected_biospec = input$biospecFilter, selected_surveycomplete = input$surveycompleteFilter)})
  
source(paste0(wd,"race_plot2_SMDB.R"), local = TRUE)
output$plot3 <- renderPlotly({race_plot2(race_data = data, selected_hospital = input$siteFilter,
                                           selected_sex = input$sexFilter, selected_age = input$ageFilter,
                                           selected_race = input$raceFilter, selected_campaign = input$campaignFilter,
                                           selected_biospec = input$biospecFilter, selected_surveycomplete = input$surveycompleteFilter)})
  
source(paste0(wd,"male_female_distribution.R"), local = TRUE)
output$plot4 <- renderPlotly({sex_distribution(sex_data = data, selected_hospital = input$siteFilter,
                                                 selected_age = input$ageFilter, selected_race = input$raceFilter,
                                                 selected_campaign = input$campaignFilter,
                                                 selected_biospec = input$biospecFilter, selected_surveycomplete = input$surveycompleteFilter)})
  
source(paste0(wd,"biospecimen_collection_distribution.R"), local = TRUE)
output$plot5 <- renderPlotly({biospecimen_collection_distribution(biocol_data = data, selected_hospital = input$siteFilter,
                                                                     selected_age = input$ageFilter, selected_race = input$raceFilter,
                                                                     selected_campaign = input$campaignFilter,
                                                                     selected_biospec = input$biospecFilter, selected_surveycomplete = input$surveycompleteFilter)})

source(paste0(wd,"completed_survey.R"), local = TRUE)
output$plot6 <- renderPlotly({completed_survey(survey_data = data, selected_hospital = input$siteFilter,
                                                 selected_age = input$ageFilter, selected_race = input$raceFilter,
                                                 selected_campaign = input$campaignFilter,
                                                 selected_biospec = input$biospecFilter, selected_surveycomplete = input$surveycompleteFilter)})
source(paste0(wd,"generate_arima_forecast_plot.R"), local = TRUE)
output$plotForecast1 <- renderPlotly({generate_arima_forecast_plot(data, date_col= "verified_date",
                                      value_col = "cumul_verified", h = 52, series_name = "Verified Participants")})
source(paste0(wd,"physical_activites_plot.R"), local = TRUE)
output$plotForecast2 <- renderPlot({physical_activities_plot(data)})
  
# Reactive expression for title
titleReactive <- reactive({
    selectedHospital <- input$siteFilter
    selectedSex <- input$sexFilter
    selectedAge <- input$ageFilter
    selectedRace <- input$raceFilter
    glue("Interactive Plot Dashboard")
})

# Send the reactive title to UI
  output$dynamicTitle <- renderText({titleReactive()})
  
}


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Connect for Cancer Prevention Stakeholder Metrics Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plot Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Forecasts", tabName = "forecast", icon = icon("sliders"), badgeLabel = "new", badgeColor = "green")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              h2(uiOutput("dynamicTitle")),
              fluidRow(
                box(solidHeader = TRUE,
                    selectInput("siteFilter", "Choose Site:",
                                choices = c("All Hospitals" = ".",
                                            "HealthPartners" = 531629870,
                                            "Henry Ford Health System" = 548392715,
                                            "Kaiser Permanente Colorado" = 125001209,
                                            "Kaiser Permanente Georgia" = 327912200,
                                            "Kaiser Permanente Hawaii" = 300267574,
                                            "Kaiser Permanente Northwest" = 452412599,
                                            "Marshfield Clinic Health System" = 303349821,
                                            "Sanford Health" = 657167265, 
                                            "University of Chicago Medicine" = 809703864,
                                            "National Cancer Institute" = 517700004,
                                            "National Cancer Institute" = 13, "Other" = 181769837),
                                selected = "All Hospitals"),
                    actionButton("applyHospitalFilter", "Apply Filters")
                ),
                box(solidHeader = TRUE,
                    selectInput("sexFilter", "Choose Gender:",
                                choices = c("All" = ".",
                                            "Male" = "Male",
                                            "Female" = "Female", 
                                            "Other" = "Other"),
                                selected = "All"),
                    actionButton("applySexFilters", "Apply Filters")
                ),
                box(solidHeader = TRUE,
                    selectInput("ageFilter", "Choose Age Bucket:",
                                choices = c("All" = ".",
                                            "40-45" = "40-45",
                                            "46-50" = "46-50", 
                                            "51-55" = "51-55",
                                            "56-60" = "56-60",
                                            "61-65" = "61-65",
                                            "66-70" = "66-70",
                                            "UNKNOWN" = "UNKNOWN"),
                                selected = "All"),
                    actionButton("applyAgeFilters", "Apply Filters")
                ),
                box(solidHeader = TRUE,
                    selectInput("raceFilter", "Choose Race:",
                                choices = c("All" = ".",
                                            names(table(data$Race_Ethnic))),
                                selected = "All"),
                    actionButton("applyRaceFilters", "Apply Filters")
                ),
                box(solidHeader = TRUE, 
                    selectInput("campaignFilter", "Choose Campaign:",
                                choices = c("All" = ".",
                                            "Random" = 926338735,
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
                                            "NA/Unknown" = NA),
                                selected = "All"),
                                actionButton("applyCampaignFilters", "Apply Filters")),
                    box(solidHeader = TRUE, 
                        selectInput("biospecFilter", "Choose Biospecimen Collection Type:",
                                    choices = c("All" = ".",
                                                "All 3 Sample Donations" =  "All 3 Sample Donations",
                                                "Blood & Urine" = "Blood & Urine",
                                                "Blood & Mouthwash" = "Blood & Mouthwash",
                                                "Mouthwash & Urine" = "Mouthwash & Urine",
                                                "Blood Only" = "Blood Only",
                                                "Urine Only" = "Urine Only",
                                                "Mouthwash Only" = "Mouthwash Only",
                                                "No Samples" = "No Samples"),
                                    selected = "All"),
                        actionButton("applyBiospFilters", "Apply Filters")),
                box(solidHeader = TRUE, 
                    selectInput("surveycompleteFilter", "Choose Survey Completion Level:",
                                choices = c("All" = ".",
                                            "BOH only" =  "BOH only",
                                            "BOH and MRE" = "BOH and MRE",
                                            "BOH and SAS" = "BOH and SAS", 
                                            "BOH and LAW" = "BOH and LAW",
                                            "BOH, MRE, and SAS" = "BOH, MRE, and SAS",
                                            "BOH, MRE, and LAW" = "BOH, MRE, and LAW",
                                            "BOH, SAS, and LAW" = "BOH, SAS, and LAW",
                                            "No Survey Sections" = "No Survey Sections"),
                                selected = "All"),
                    actionButton("applySurvCompleteFilters", "Apply Filters")),
              fluidRow(
                box(plotlyOutput("plot1", height = 350), width = 12),
                box(plotlyOutput("plot2", height = 350), width = 12),
                box(plotlyOutput("plot3", height = 350), width = 12),
                box(plotlyOutput("plot4", height = 450), width = 12),
                box(plotlyOutput("plot5", height = 450), width = 12),
                box(plotlyOutput("plot6", height = 650), width = 12)
              )
      )
    ),
  # Add the "Forecasts" tab item
  tabItem(tabName = "forecast",
          h2("Forecasts"),
          fluidRow(
            box(plotlyOutput("plotForecast1", height = 350), width = 12),
            box(plotOutput("plotForecast2", height = 350), width = 12))
  )
    )
)
)
shinyApp(ui, server)