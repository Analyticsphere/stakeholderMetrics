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

#source the custom aesthetic values for fonts and colors
source("./customCSS.R", local = TRUE)
custom_aesthetics <- customCSS()

server <- function(input, output, session){
  
  #call data once for entire dashboard
  #authentication step for Posit
  #this code was written by D Russ
  #  source("./get_authentication.R", local = TRUE)
  #  get_authentication(service_account_key = "SERVICE_ACCT_KEY")

#load verified data
verified_data <- reactive({
      source("./clean_data.R", local = TRUE)
      source("./get_data.R", local=TRUE)
      data <- get_data() # Fetch your data
      clean_data(data, type = "verified")
})

  
  # Define a reactive expression that filters the data
    filtered_verified_data <- reactive({
        req(verified_data())
        if(input$applyFilters){
          verified_data()%>%
            filter((input$siteFilter == "." | d_827220437 == input$siteFilter) &
                 (input$sexFilter == "." | sex == input$sexFilter) &
                 (input$ageFilter == "." | Age == input$ageFilter)&
                 (input$raceFilter == "." | race == input$raceFilter)&
                 (input$campaignFilter == "." | active_camptype == input$campaignFilter)&
                 (input$biospecFilter == "." | biocol_type == input$biospecFilter)&
                 (input$surveycompleteFilter == "." | Msrv_complt == input$surveycompleteFilter))
        }else{
          verified_data()
        }
})
    
    source("./activity_plot.R", local = TRUE)
    output$plot1 <- renderPlotly({activity_plot(activity_data=filtered_verified_data())})
    
    source("./age_plot.R", local = TRUE)
    output$plot2 <- renderPlotly({age_plot(age_data = filtered_verified_data())})
    
    source("./race_plot.R", local = TRUE)
    output$plot3 <- renderPlotly({race_plot(race_data = filtered_verified_data())})
    
    source("./sex_distribution.R", local = TRUE)
    output$plot4 <- renderPlotly({sex_distribution(sex_data = filtered_verified_data())})
    
    source("./biospecimen_collection_distribution.R", local = TRUE)
    output$plot5 <- renderPlotly({biospecimen_collection_distribution(biocol_data = filtered_verified_data())})
    
    source("./completed_survey.R", local = TRUE)
    output$plot6 <- renderPlotly({completed_survey(survey_data = filtered_verified_data())})
    

invited_participant_data <- reactive({
      source("./clean_data.R", local = TRUE)
      source("./get_data.R", local=TRUE)
      invited_participant_data <- clean_data(get_data(project =
                                                      "nih-nci-dceg-connect-bq2-prod",
                                                      dataset = "StakeHolderMetrics_RS",
                                                      table = "invited_participants_complete"),
                                                      type = "invited")
})


filtered_IP_data <- reactive({
        req(invited_participant_data())
        if(input$applyIPfilters){
          invited_participant_data()%>%
            filter((input$IPsiteFilter == "." | site == input$IPsiteFilter) &
                 (input$IPsexFilter == "." | sex == input$IPsexFilter) &
                 (input$IPageFilter == "." | Age == input$IPageFilter)&
                 (input$IPraceFilter == "." | race == input$IPraceFilter))
        }else{
          invited_participant_data()
        }
})

    source("./age_plot.R", local = TRUE)
    output$invited_plot1 <- renderPlotly({age_plot(age_data = filtered_IP_data())})
    
    source("./race_plot.R", local = TRUE)
    output$invited_plot2 <- renderPlotly({race_plot(race_data = filtered_IP_data())})


}
  
  # Define UI
  ui <- dashboardPage(
    dashboardHeader(title = "Connect for Cancer Prevention"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Plot Dashboard", tabName = "dashboard"),
        menuItem("Invited Participant Dashboard", tabName = "invited_participants")
      )
    ),
    dashboardBody(
      tags$head(tags$style(HTML(custom_aesthetics))), 
      tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(
                  column(4,
                         box(solidHeader = FALSE, title = "Choose Filters:", width = 12,
                             selectInput("siteFilter", "Site",
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
                             selectInput("sexFilter", "Gender",
                                         choices = c("All" = ".",
                                                     "Male" = "Male",
                                                     "Female" = "Female", 
                                                     "Other" = "Other"),
                                         selected = "All"),
                             selectInput("ageFilter", "Age",
                                         choices = c("All" = ".",
                                                     "40-45" = "40-45",
                                                     "46-50" = "46-50", 
                                                     "51-55" = "51-55",
                                                     "56-60" = "56-60",
                                                     "61-65" = "61-65",
                                                     "66-70" = "66-70",
                                                     "UNKNOWN" = "UNKNOWN"),
                                         selected = "All"),
                             selectInput("raceFilter", "Race",
                                         choices = c("All" = ".",
                                                     "American Indian or Alaska Native" = "American Indian or Alaska Native",
                                                     "Asian" = "Asian", 
                                                     "Black, African American, or African" = "Black, African American, or African",
                                                     "Hawaiian or other Pacific Islander" = "Hawaiian or other Pacific Islander",
                                                     "Hispanic, Latino, or Spanish" = "Hispanic, Latino, or Spanish",
                                                     "Middle Eastern or North African" = "Middle Eastern or North African",
                                                     "Multi-race" = "Multi-race",
                                                     "Other" = "Other",
                                                     "Skipped this question" = "Skipped this question",
                                                     "UNKNOWN" = "UNKNOWN", 
                                                     "White" = "White"),
                                         selected = "All"),
                             selectInput("campaignFilter", "Campaign",
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
                             selectInput("biospecFilter", "Biospecimen Collection Type",
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
                             selectInput("surveycompleteFilter", "Survey Completion Level",
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
                             actionButton("applyFilters", "Apply Filters"))),
                         column(width = 8,
                                fluidRow(
                                  box(plotlyOutput("plot1", height = 350), width = 12),
                                  box(plotlyOutput("plot2", height = 350), width = 12),
                                  box(plotlyOutput("plot3", height = 350), width = 12),
                                  box(plotlyOutput("plot4", height = 450), width = 12),
                                  box(plotlyOutput("plot5", height = 450), width = 12),
                                  box(plotlyOutput("plot6", height = 650), width = 12))
                                )
                  ) 
                ), 
                tabItem(tabName = "invited_participants",
                        fluidRow(
                          column(4,
                                 box(solidHeader = FALSE,
                                     selectInput("IPageFilter", "Age Range:",
                                                 choices = c("All" = ".",
                                                             "40-45" = "40-45",
                                                             "46-50" = "46-50", 
                                                             "51-55" = "51-55",
                                                             "56-60" = "56-60",
                                                             "61-65" = "61-65",
                                                             "66-70" = "66-70",
                                                             "UNKNOWN" = "UNKNOWN"),
                                                 selected = "All"),
                                     selectInput("IPsexFilter", "Choose Gender:",
                                                 choices = c("All" = ".",
                                                             "Male" = "Male",
                                                             "Female" = "Female", 
                                                             "Other" = "Other"),
                                                 selected = "All"),
                                     selectInput("IPraceFilter", "Choose Race:",
                                                 choices = c("All" = ".",
                                                             "OTHER" = "OTHER",
                                                             "UNKNOWN" = "UNKNOWN", 
                                                             "WHITE, NON-HISPANIC" = "WHITE, NON-HISPANIC",
                                                             "NA" = "NA"),
                                                 selected = "All"),
                                     selectInput("IPsiteFilter", "Choose Site:",
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
                                     actionButton("applyIPfilters", "Apply Filters"))),
                        column(8,
                               fluidRow(
                                 box(plotlyOutput("invited_plot1", height = 350), width = 12),
                                 box(plotlyOutput("invited_plot2", height = 350), width = 12))
                               )
                        )
                )
      )
    )
)
shinyApp(ui, server)
