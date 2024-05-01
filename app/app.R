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

#source the custom color palette
#use the "select_colors" function to choose colors
source("./color_palette.R", local = TRUE)
color_palette <- color_palette()


server <- function(input, output, session){
  
  #call data once for entire dashboard
  #authentication step for Posit
  #this code was written by D Russ
  #   source("./get_authentication.R", local = TRUE)
  #    get_authentication(service_account_key = "SERVICE_ACCT_KEY")
  
  #load verified data
  verified_data <- reactive({
    source("./clean_data.R", local = TRUE)
    source("./get_data.R", local=TRUE)
    data <- get_data() # Fetch your data
    data <- clean_data(data, type = "verified")
  })
  
  
  # Define a reactive expression that filters the data
  filtered_verified_data <- reactive({
    req(verified_data())
    if(input$applyFilters){
      verified_data()%>%
        filter((input$siteFilter == "." | d_827220437 == input$siteFilter) &
                 (input$sexFilter == "." | sex == input$sexFilter) &
                 (input$ageFilter == "." | age == input$ageFilter)&
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
  
  source("./biospecimen_collection_barchart.R", local = TRUE)
  output$plot5b <- renderPlotly({biospecimen_collection_barchart(biocol_data = filtered_verified_data())})
  
  source("./completed_survey.R", local = TRUE)
  output$plot6 <- renderPlotly({completed_survey(survey_data = filtered_verified_data())})
  
  source("./completed_survey_barchart.R", local = TRUE)
  output$plot6b <- renderPlotly({completed_survey_barchart(survey_data = filtered_verified_data())})
  
  source("./income_distribution.R", local = TRUE)
  output$plot7 <- renderPlotly({income_distribution(income_data = filtered_verified_data())})
  
  
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
  
  source("./age_stacked_bar_chart.R", local = TRUE)
  output$invited_plot1 <- renderPlotly({age_stacked_bar_chart(ip_age_data = filtered_IP_data(), v_age_data = filtered_verified_data())})
  
  source("./age_percentage_bar_chart.R", local = TRUE)
  output$invited_plot1b <- renderPlotly({age_percentage_bar_chart(ip_age_data = filtered_IP_data(), v_age_data = filtered_verified_data())})
  
  source("./race_stacked_bar_chart.R", local = TRUE)
  output$invited_plot2 <- renderPlotly({race_stacked_bar_chart(ip_race_data = filtered_IP_data(), v_race_data = filtered_verified_data())})
  
  source("./race_percentage_bar_chart.R", local = TRUE)
  output$invited_plot2b <- renderPlotly({race_percentage_bar_chart(ip_race_data = filtered_IP_data(), v_race_data = filtered_verified_data())})
  
  source("./sex_stacked_bar_chart.R", local = TRUE)
  output$invited_plot3 <- renderPlotly({sex_stacked_bar_chart(ip_sex_data = filtered_IP_data(), v_sex_data = filtered_verified_data())})
  
  source("./sex_percentage_bar_chart.R", local = TRUE)
  output$invited_plot3b <- renderPlotly({sex_percentage_bar_chart(ip_sex_data = filtered_IP_data(), v_sex_data = filtered_verified_data())})
  
  fast_facts_reactive <- reactive({
    source("./fast_facts2.R", local = TRUE)
    verified_data <- verified_data()  # Assuming this is how you get your verified data
    fast_facts(verified_data = verified_data)
  })
  
  output$totalVerifiedBox <- renderValueBox({
    facts <- fast_facts_reactive()
    valueBox(value = tags$p(paste0(facts$total_verified), style = "font-size:75%;"),
             subtitle = tags$p("Participants", style = "font-size: 95%"),
             icon = tags$i(class = "fas fa-users", style = "font-size: 40px"))
  })
  
  
  output$maleVerifiedBox <- renderValueBox({
    facts <- fast_facts_reactive()
    valueBox(value = tags$p(paste0(facts$male_verified), style = "font-size: 75%;"),
             subtitle = tags$p("Male", style = "font-size: 95%"),
             icon = tags$i(class = "fas fa-mars", style = "font-size: 40px"))
  })
  
  output$femaleVerifiedBox <- renderValueBox({
    facts <- fast_facts_reactive()
    valueBox(value = tags$p(paste0(facts$female_verified), style = "font-size: 75%;"),
             subtitle = tags$p("Female", style = "font-size: 95%"),
             icon = tags$i(class = "fas fa-venus", style = "font-size: 40px"))
  })
  
  
  output$commonIncomeBox <- renderValueBox({
    facts <- fast_facts_reactive()
    # Split the string at the hyphen
    income_parts <- unlist(strsplit(facts$common_income, "-"))
    # Prepare the two lines with the hyphen and line break
    first_line <- paste0(income_parts[1], "-")
    second_line <- income_parts[2]
    # Combine lines with a line break for the subtitle
    formatted_subtitle <- paste0(first_line, second_line)
    valueBox(value = tags$p(formatted_subtitle, style = "font-size: 40%;"),
             subtitle = tags$p("Most Common Annual Income", style = "font-size: 75%"),
             icon = tags$i(class = "fas fa-money-bill", style = "font-size: 40px"))
  })
  
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
                column(12, align = "center", 
                       tags$h3(style = "text-align: center;font-size: 36px;color: black;", 
                               HTML(glue("Verified Participant Dashboard as of {format(Sys.Date(), '%B %d, %Y')}")))
                )
              ),
              fluidRow(
                # Wrap the Fast facts section in a div with a border
                div(class="col-md-4", 
                    style = "padding: 20px;text-align: left",
                    h3("Newly Verified March Participant Fast Facts", style = "color: black;"),
                    valueBoxOutput("totalVerifiedBox"),
                    valueBoxOutput("maleVerifiedBox"),
                    valueBoxOutput("femaleVerifiedBox"),
                    valueBoxOutput("commonIncomeBox")
                ),
                # Map graphic column
                column(width = 8,
                       tags$img(src = "map_site_color_Mar2023.jpg",
                                style = "width:50%; height:auto;
                                  display:block; margin-left:auto; margin-right:auto;"),
                       tags$figcaption(style = "text-align:center;
                                         font-style:italic;margin-bottom: 140px",
                                       "Map of Catchment Coverage by Site as of March 2023")
                )
              ),
              # Insert a new fluidRow for the horizontal bar
              fluidRow(
                column(12,
                       h1("Demographic Statistics", style = "color: black"),
                       div(style ="height: 15px; background-color: black; margin: 20px 0;"),  # Adjust the height and color
                       
                )
              ),
              fluidRow(
                column(4,
                       box(solidHeader = FALSE, title = "Choose Filters:", width = 12,
                           selectInput("siteFilter", "Site",
                                       choices = c("All" = ".",
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
                                       selected = "All"),
                           selectInput("sexFilter", "Gender",
                                       choices = c("All" = ".",
                                                   "Male" = "Male",
                                                   "Female" = "Female", 
                                                   "Nonbinary" = "Nonbinary",
                                                   "Unknown" = "Unknown"),
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
                div(class = "grid-container",style ="width:40%display:inline-block",
                    column(width = 4,
                           fluidRow(
                             div(class = "plot-container", plotlyOutput("plot7")),
                             div(class = "plot-container", plotlyOutput("plot2")))
                    )
                ),
                div(class = "grid-container",style ="width:40%display:inline-block",
                    column(width = 4,
                           fluidRow(
                             div(class = "pie-plot-container", plotlyOutput("plot3")),
                             div(class = "pie-plot-container", plotlyOutput("plot4")))
                    )
                )
              ), 
              # Insert a new fluidRow for the horizontal bar
              fluidRow(
                column(12,
                       h1("Survey Completion Statistics", style = "color: black"),
                       div(style ="height: 15px; background-color: black; margin: 20px 0;"),  # Adjust the height and color
                       
                )
              ),
              fluidRow(
                column(4,
                       div(class = "plot-container", plotlyOutput("plot5b")),
                       div(class = "plot-container", plotlyOutput("plot6b"))
                ),
                column(8,
                       fluidRow(
                         div(class = "pie-plot-container", plotlyOutput("plot5")),
                         div(class = "pie-plot-container", plotlyOutput("plot6")),
                         div(class = "plot-container", plotlyOutput("plot1"))
                       )
                )
              )
      ),
      tabItem(tabName = "invited_participants",
              fluidRow(
                column(12, align = "center", 
                       tags$h3(style = "text-align: center;", 
                               HTML(glue("Invited Participant Dashboard as of {format(Sys.Date(), '%B %d, %Y')}")))
                )
              ),
              fluidRow(
                column(4,
                       box(solidHeader = FALSE,
                           selectInput("IPageFilter", "Age Category:",
                                       choices = c("All" = ".",
                                                   "40-45" = "40-45",
                                                   "46-50" = "46-50", 
                                                   "51-55" = "51-55",
                                                   "56-60" = "56-60",
                                                   "61-65" = "61-65",
                                                   "66-70" = "66-70",
                                                   "UNKNOWN" = "UNKNOWN"),
                                       selected = "All"),
                           selectInput("IPsexFilter", "Gender:",
                                       choices = c("All" = ".",
                                                   "Male" = "Male",
                                                   "Female" = "Female", 
                                                   "Other" = "Other"),
                                       selected = "All"),
                           selectInput("IPraceFilter", "Race:",
                                       choices = c("All" = ".",
                                                   "OTHER" = "OTHER",
                                                   "UNKNOWN" = "UNKNOWN", 
                                                   "WHITE, NON-HISPANIC" = "WHITE, NON-HISPANIC",
                                                   "NA" = "NA"),
                                       selected = "All"),
                           selectInput("IPsiteFilter", "Site:",
                                       choices = c("All" = ".",
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
                                       selected = "All"),
                           actionButton("applyIPfilters", "Apply Filters"))),
                column(4, plotlyOutput("invited_plot1")),
                column(4, plotlyOutput("invited_plot1b"))),
              fluidRow(
                column(4, ),
                column(4, plotlyOutput("invited_plot2")),
                column(4, plotlyOutput("invited_plot2b"))),
              fluidRow(
                column(4, ),
                column(4, plotlyOutput("invited_plot3")),
                column(4, plotlyOutput("invited_plot3b")))
      )
    )
  )
)


shinyApp(ui, server)