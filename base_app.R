# Base application
library(shiny)
library(bigrquery)
library(shinydashboard)
library(glue)
library(httr)
library(googleAuthR)
library(auth0)
options(auth0_config_file = "./_auth0.yml")

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      actionButton("start_auth", "Authenticate with Google"),
      uiOutput("auth_button"),
      textOutput("user_info"))
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content"),

      )
    )
  )
)

server <- function(input, output, session) {
  
  project <- 'nih-nci-dceg-connect-stg-5519'
  dataset <- 'FlatConnect'
  table <- 'participants_JP'
  sql <- glue('SELECT Connect_ID ',
              'FROM `{project}.{dataset}.{table}` ',
              'WHERE Connect_ID IS NOT NULL ',
              'LIMIT 100')
  print("project loaded")
  
  
  output$credential_info <- renderPrint({
    session$userData$AUTH0_CLIENTID
  })
  
  #here we pass token to bigrquery within bq_auth()
  
  #this token is requested by auth0 to GCP
  #GCP then provides a token to auth0 and gives it to us
  #then we pass this token to bq_auth
  auth_token <- session$userData$auth0_credentials$access_token
  bq_auth(token = auth_token)
  token_access <- bigrquery::bq_has_token()
  print(token_access)
  
  output$response <- renderPrint({
    content(response, as="text")
  })

  data_reactive <- eventReactive(input$get_data, {
    # Authenticate to BigQuery
    #option 1:
   # print("button clicked")
  #  print(paste0("this session is interactive:", interactive()))
  #  bigrquery::bq_auth(email = NA)
  #  {    
    #option 2:
    #bigrquery::bq_auth(use_oob = TRUE)
    
    #option 3:
    #bigrquery::bq_auth(email = NA, use_oob = TRUE)
    
    #option 4: 
   # options(gargle_oauth_client_type = "web")
    #bq_auth(path = NULL, email = TRUE)
    # Download data
    if(!is.null(creds)){
    tb <- bigrquery::bq_project_query(project, query=sql)
    data <- bigrquery::bq_table_download(tb, 
                                         bigint="integer64", 
                                         page_size=1000)}})

  output$data_table <- renderTable({
    data_reactive()
  })
}

shinyAppAuth0(ui, server)
#shinyAuth0App(ui, server)
#shinyApp(ui,server)
