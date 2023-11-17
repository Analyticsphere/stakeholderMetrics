library(shiny)
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


collectData <- function(input, output, session){
  
  # project <- "nih-nci-dceg-connect-stg-5519"
  #  dataset <- 'FlatConnect'
  #  table   <- 'participants_JP'
  #  projectid = glue(project,dataset, table, sep = ".")
  #  tbl <- bq_project_query( project,
  #                           'SELECT Connect_ID FROM `nih-nci-dceg-connect-stg-5519.FlatConnect.participants_JP` WHERE Connect_ID IS NOT NULL LIMIT 100')
  #  results <- bq_table_download(tbl, n_max = 100)
  #  output$table<-DT::renderDataTable(DT::datatable(results))
  # bq_project <- "nih-nci-dceg-druss"
  # tbl <- bq_project_query( bq_project,
  #                          'SELECT unique_key,complaint_description,status,created_date,last_update_date FROM `bigquery-public-data.austin_311.311_service_requests` where not status="Closed" LIMIT 100')
  # results <- bq_table_download(tbl, n_max = 100)
  # output$table<-DT::renderDataTable(DT::datatable(results))
  
  
  
  #plot1
    source("./get_gcp_data.R", local = TRUE)
  # {modules <- c("d_100767870", "d_949302066", "d_536735468", "d_663265240", "d_976570371", "d_517311251", "d_832139544", "d_264644252", "d_770257102")
  #  bio.col <- c("d_684635302", "d_878865966", "d_167958071", "d_173836415_d_266600170_d_915179629", "d_173836415_d_266600170_d_718172863",
  #               "d_173836415_d_266600170_d_592099155", "d_173836415_d_266600170_d_561681068", "d_173836415_d_266600170_d_847159717",
  #               "d_173836415_d_266600170_d_448660695", "d_173836415_d_266600170_d_139245758", "d_173836415_d_266600170_d_541311218",
  #               "d_173836415_d_266600170_d_224596428", "d_173836415_d_266600170_d_740582332", "d_173836415_d_266600170_d_982213346",
  #               "d_173836415_d_266600170_d_398645039", "d_173836415_d_266600170_d_822274939")
  #  clc.bldtm <- c("d_173836415_d_266600170_d_769615780", "d_173836415_d_266600170_d_822274939", "d_173836415_d_266600170_d_398645039",
  #                 "d_173836415_d_266600170_d_982213346", "d_173836415_d_266600170_d_740582332")
  #  clc.urinetm <- c("d_173836415_d_266600170_d_139245758", "d_173836415_d_266600170_d_224596428", "d_173836415_d_266600170_d_541311218",
  #                   "d_173836415_d_266600170_d_939818935", "d_173836415_d_266600170_d_740582332")
  #    age <- c("state_d_934298480", "d_914594314")
  #    race <- c("d_821247024", "d_914594314",  "d_827220437","d_512820379","d_949302066" , "d_517311251")
  # 
  #    var_list_activity_plot <- c("token", "Connect_ID", "d_821247024", "d_914594314", "d_512820379", "state_d_158291096", "d_471593703", "d_827220437",
  #                "d_130371375_d_266600170_d_787567527", "d_130371375_d_266600170_d_731498909","state_d_934298480", bio.col, modules, age, race)
  #    var_list_activity_plot <- var_list_activity_plot[!duplicated(var_list_activity_plot)]
  # 
  # # Define the variables from the second query
  #  project <- 'nih-nci-dceg-connect-stg-5519'
  #  dataset <- 'FlatConnect'
  #  table <- 'participants_JP'
  # 
  #  variables = paste(var_list_activity_plot, collapse = ", ")
  #  }
  #filter default is set to where connect_id is not missing and d_821247024 = 197316935
  #get_data <- get_gcp_data(variables, dataset, table, project = project)
  
  #activities by participant
     #source("./activities_plot.R", local = TRUE)
     #output$plot1 <- renderPlotly({activities_plot(data = get_data, variables = var_list_activity_plot)})
  
  
  #plot2
    print("beginning plot2")
    project <- "nih-nci-dceg-connect-stg-5519"
    dataset <- 'FlatConnect'
    table   <- 'participants_JP'
    var <- c("d_914594314", "state_d_934298480")
    source("./get_gcp_data.R", local = TRUE)
    get_data <- get_gcp_data(var, dataset, table, project = project)
    source("./base_age_plot.R", local = TRUE)
    output$plot2 <- renderPlotly({age_plot(data = get_data)})
    
    #plot3
     source("./race_plot2.R", local = TRUE)
     output$plot3 <- renderPlotly({race_plot()})
  
}

#define UI
ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotlyOutput("plot2", height = 250),
      box(plotlyOutput("plot3", height = 450))
      )
    )
  )
)



if (interactive()) {
  # testing url
  options(shiny.port = 8100)
  APP_URL <- "http://localhost:8100/"
  client_secret_path = "./client_secret_470984582294-evfoejth2p303leittlmnnerceuecb2l.apps.googleusercontent.com.json"
  app <- gargle::gargle_oauth_client_from_json(client_secret_path, name = NULL)
} else {
  # deployed URL
  APP_URL <- "https://appshare-dev.cancer.gov/content/77c103cc-b5e7-4734-b762-ba7728b3e30f"
  ## need to get the client secret/
  client_secret_path = "./APPclient_secret_810867956467-j10cm0amob7njgum8e65288h2tlra39m.apps.googleusercontent.com.json"
  app <- gargle::gargle_oauth_client_from_json(client_secret_path, name = NULL)
}





###################################
#shouldnt have to edit anything below this line
##################################



api = httr::oauth_endpoints("google")
scope <- "https://www.googleapis.com/auth/bigquery"

print(app)
print(api)



has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth codes are present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
}

uiFunc <- function(req) {
  print(" ... in uiFunc ...")
  urlQueryString <- parseQueryString(req$QUERY_STRING)
  if (!has_auth_code(urlQueryString)) {
    url <- oauth2.0_authorize_url(api, app, scope = scope)
    redirect <- sprintf("location.replace(\"%s\");", url)
    message(".. no code ... ",redirect)
    tags$script(HTML(redirect))
  } else {
    message(".. have code ... ")
    print(parseQueryString(req$QUERY_STRING))
    ui
  }
}


server <- function(input, output, session) {
  
  # It appears that for OAuth to work the first part of the
  # oauth dance is performed on the client side, and the 
  # second part is performed on the server.
  #
  # you have to leave the "code" in the url because 
  # the token is saved in the scope of this function.
  print(" ... in server ...")
  
  params <- parseQueryString(isolate(session$clientData$url_search))
  message("SERVER: params: ",params)
  if (!has_auth_code(params)) {
    return()
  }
  
  # Manually create a token
  token <- oauth2.0_token(
    app = app,
    endpoint = api,
    credentials = oauth2.0_access_token(api, app, params$code),
    cache = FALSE
  )
  updateQueryString("?",mode = "push")
  bq_auth(token=token)
  
  
  ### add a function 
  collectData(input,output,session)
}

shinyApp(uiFunc,server)