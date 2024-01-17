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

#this is the server call
server <- function(input, output, session){
  
  #plot1
  source("/Users/sansalerj/Desktop/local_app_SMDB_data/activity_plot_SMDB.R", local = TRUE)
  output$plot1 <- renderPlotly({activity_plot()})

  #plot2
  source("/Users/sansalerj/Desktop/local_app_SMDB_data/age_plot_SMDB.R", local = TRUE)
  output$plot2 <- renderPlotly({age_plot()})
  
    

  # #plot2
  #   print("beginning plot2")
  #     age_plot_project <- "nih-nci-dceg-connect-prod-6d04"
  #     age_plot_dataset <- 'StakeHolderMetrics_tmp'
  #     age_plot_table   <- 'figure1_activity'
  #     age_plot_var <- c("d_914594314", "state_d_934298480", "d_827220437")
  #    source("/Users/sansalerj/Desktop/local_app/get_gcp_data.R", local = TRUE)
  #    age_plot_data <- reactive({get_gcp_data(age_plot_var, age_plot_dataset, age_plot_table, project = age_plot_project)})
  #    source("/Users/sansalerj/Desktop/local_app/base_age_plot.R", local = TRUE)
  #    output$plot2 <- renderPlotly({age_plot(data = age_plot_data(), selected_hospital = input$siteFilter)})
  # #plot3
  #    #data is pulled within the race_plot2 file
  #     source("/Users/sansalerj/Desktop/local_app/race_plot2.R", local = TRUE)
  #     output$plot3 <- renderPlotly({race_plot()})
  #     
  source("/Users/sansalerj/Desktop/local_app_SMDB_data/race_plot2_SMDB.R", local = TRUE)
        output$plot3 <- renderPlotly({race_plot()})

  
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
              h2("Interactive Plot Dashboard"),
              fluidRow(
                box(plotlyOutput("plot1", height = 350), width = 6),
                box(plotlyOutput("plot2", height = 350), width = 6)
              ),
              fluidRow(
                box(plotlyOutput("plot3", height = 450), width = 12)
              )),
      
      # Filters Tab
      tabItem(tabName = "filters",
              fluidRow(
                box(title = "Filter Options", status = "primary", solidHeader = TRUE,
                    selectInput("siteFilter", "Choose Site:",
                                choices = c("All Hospitals" = NA,
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
                    actionButton("applyFilters", "Apply Filters")
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



# 
# if (interactive()) {
#   # testing url
#   options(shiny.port = 8100)
#   APP_URL <- "http://localhost:8100/"
#   client_secret_path = "./client_secret_470984582294-evfoejth2p303leittlmnnerceuecb2l.apps.googleusercontent.com.json"
#   app <- gargle::gargle_oauth_client_from_json(client_secret_path, name = NULL)
# } else {
#   # deployed URL
#   APP_URL <- "https://appshare-dev.cancer.gov/content/77c103cc-b5e7-4734-b762-ba7728b3e30f"
#   ## need to get the client secret/
#   client_secret_path = "./APPclient_secret_810867956467-j10cm0amob7njgum8e65288h2tlra39m.apps.googleusercontent.com.json"
#   app <- gargle::gargle_oauth_client_from_json(client_secret_path, name = NULL)
# }
# 



# 
# ###################################
# #shouldnt have to edit anything below this line
# ##################################
# 
# 
# 
# api = httr::oauth_endpoints("google")
# scope <- "https://www.googleapis.com/auth/bigquery"
# 
# print(app)
# print(api)
# 
# 
# 
# has_auth_code <- function(params) {
#   # params is a list object containing the parsed URL parameters. Return TRUE if
#   # based on these parameters, it looks like auth codes are present that we can
#   # use to get an access token. If not, it means we need to go through the OAuth
#   # flow.
#   return(!is.null(params$code))
# }
# 
# uiFunc <- function(req) {
#   print(" ... in uiFunc ...")
#   urlQueryString <- parseQueryString(req$QUERY_STRING)
#   if (!has_auth_code(urlQueryString)) {
#     url <- oauth2.0_authorize_url(api, app, scope = scope)
#     redirect <- sprintf("location.replace(\"%s\");", url)
#     message(".. no code ... ",redirect)
#     tags$script(HTML(redirect))
#   } else {
#     message(".. have code ... ")
#     print(parseQueryString(req$QUERY_STRING))
#     ui
#   }
# }
# 
# 
# server <- function(input, output, session) {
#   
#   # It appears that for OAuth to work the first part of the
#   # oauth dance is performed on the client side, and the 
#   # second part is performed on the server.
#   #
#   # you have to leave the "code" in the url because 
#   # the token is saved in the scope of this function.
#   print(" ... in server ...")
#   
#   params <- parseQueryString(isolate(session$clientData$url_search))
#   message("SERVER: params: ",params)
#   if (!has_auth_code(params)) {
#     return()
#   }
#   
#   # Manually create a token
#   token <- oauth2.0_token(
#     app = app,
#     endpoint = api,
#     credentials = oauth2.0_access_token(api, app, params$code),
#     cache = FALSE
#   )
#   updateQueryString("?",mode = "push")
#   bq_auth(token=token)
#   
#   
#   ### add a function 
#   collectData(input,output,session)
# }

shinyApp(ui, server)