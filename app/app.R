# Dependencies -----------------------------------------------------------------
## Libraries -------------------------------------------------------------------
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
library(forecast)


## UDFs ------------------------------------------------------------------------
source("./aspect_box.R", local = TRUE)    # generate boxes with standard sizes 
source("./color_palette.R", local = TRUE) # custom color palette for connect
color_palette <- color_palette()



# Server =======================================================================
server <- function(input, output, session){
  
  
  ## Authentication step for Posit ---------------------------------------------
  # written by D Russ 
  is_posit_connect <- Sys.getenv("RSTUDIO_PRODUCT") == "CONNECT"
  if (is_posit_connect) {
    source("./get_authentication.R", local = TRUE)
    get_authentication(service_account_key = "SERVICE_ACCT_KEY")
  }
  
  # call data once for entire dashboard
  
  ## Load verified data --------------------------------------------------------
  verified_data <- reactive({
    source("./clean_data.R", local = TRUE)
    source("./get_data.R", local=TRUE)
    data <- get_data() # Fetch your data
    data <- clean_data(data, type = "verified")
  })
  
  ## Reactive filter function --------------------------------------------------
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
  
  ## Calculating the number of verified participants by the filtering ----------
  row_count <- reactive({
    nrow(filtered_verified_data())
  })
  
  output$rowCountText <- renderText({
    paste("Number of Verified Participants with Current Filters Applied: \n", row_count())
  })

  ## Load aggregated IP data ---------------------------------------------------
  aggregated_IP_data <- reactive({
    source("./clean_data.R", local = TRUE)
    source("./get_data.R", local=TRUE)
    data <- get_data(table = "participantFunnelGraph") # Fetch your data
  })
  
  ## Generate Verified Participant Figures -------------------------------------
  
  source("./get_active_filters.R", local = TRUE)
  source("./apply_plotly_config.R", local = TRUE)
  
  # Activity Plot
  source("./activity_plot.R", local = TRUE)
  output$plot1 <- renderPlotly({
    filters <- get_active_filters(input)  # Get active filters from user input
    plot <- activity_plot(activity_data = filtered_verified_data())  # Generate the plot
    apply_plotly_config(plot, filters, "activity_plot")  # Apply the config
  })
  
  # Age Plot
  source("./age_plot.R", local = TRUE)
  output$plot2 <- renderPlotly({
    filters <- get_active_filters(input)  # Get active filters from user input
    plot <- age_plot(age_data = filtered_verified_data())  # Generate the plot
    apply_plotly_config(plot, filters, "age_plot")  # Apply the config
  })
  
  # Race Plot
  source("./race_plot.R", local = TRUE)
  output$plot3 <- renderPlotly({
    filters <- get_active_filters(input)  # Get active filters from user input
    plot <- race_plot(race_data = filtered_verified_data())  # Generate the plot
    apply_plotly_config(plot, filters, "race_plot")  # Apply the config
  })
  
  # Sex Distribution
  source("./sex_distribution.R", local = TRUE)
  output$plot4 <- renderPlotly({
    filters <- get_active_filters(input)  # Get active filters from user input
    plot <- sex_distribution(sex_data = filtered_verified_data())  # Generate the plot
    apply_plotly_config(plot, filters, "sex_distribution")  # Apply the config
  })
  
  # Biospecimen Collection Distribution
  source("./biospecimen_collection_distribution.R", local = TRUE)
  output$plot5 <- renderPlotly({
    filters <- get_active_filters(input)  # Get active filters from user input
    plot <- biospecimen_collection_distribution(biocol_data = filtered_verified_data())  # Generate the plot
    apply_plotly_config(plot, filters, "biospecimen_collection_distribution")  # Apply the config
  })
  
  # Biospecimen Collection Barchart
  source("./biospecimen_collection_barchart.R", local = TRUE)
  output$plot5b <- renderPlotly({
    filters <- get_active_filters(input)  # Get active filters from user input
    plot <- biospecimen_collection_barchart(biocol_data = filtered_verified_data())  # Generate the plot
    apply_plotly_config(plot, filters, "biospecimen_collection_barchart")  # Apply the config
  })
  
  # Completed Survey
  source("./completed_survey.R", local = TRUE)
  output$plot6 <- renderPlotly({
    filters <- get_active_filters(input)  # Get active filters from user input
    plot <- completed_survey(survey_data = filtered_verified_data())  # Generate the plot
    apply_plotly_config(plot, filters, "completed_survey")  # Apply the config
  })
  
  # Completed Survey Barchart
  source("./completed_survey_barchart.R", local = TRUE)
  output$plot6b <- renderPlotly({
    filters <- get_active_filters(input)  # Get active filters from user input
    plot <- completed_survey_barchart(survey_data = filtered_verified_data())  # Generate the plot
    apply_plotly_config(plot, filters, "completed_survey_barchart")  # Apply the config
  })
  
  # Income Distribution
  source("./income_distribution.R", local = TRUE)
  output$plot7 <- renderPlotly({
    filters <- get_active_filters(input)  # Get active filters from user input
    plot <- income_distribution(income_data = filtered_verified_data())  # Generate the plot
    apply_plotly_config(plot, filters, "income_distribution")  # Apply the config
  })
  
  # Module Completion Time BOH
  source("./module_completion_time.R", local = TRUE)
  output$plot8 <- renderPlotly({
    filters <- get_active_filters(input)  # Get active filters from user input
    plot <- module_completion_time(data = filtered_verified_data(), survey = "BOH")  # Generate the plot
    apply_plotly_config(plot, filters, "module_completion_time_BOH")  # Apply the config
  })
  
  # Module Completion Time SAS
  source("./module_completion_time.R", local = TRUE)
  output$plot9 <- renderPlotly({
    filters <- get_active_filters(input)  # Get active filters from user input
    plot <- module_completion_time(data = filtered_verified_data(), survey = "SAS")  # Generate the plot
    apply_plotly_config(plot, filters, "module_completion_time_SAS")  # Apply the config
  })
  
  # Module Completion Time MRE
  source("./module_completion_time.R", local = TRUE)
  output$plot10 <- renderPlotly({
    filters <- get_active_filters(input)  # Get active filters from user input
    plot <- module_completion_time(data = filtered_verified_data(), survey = "MRE")  # Generate the plot
    apply_plotly_config(plot, filters, "module_completion_time_MRE")  # Apply the config
  })
  
  # Module Completion Time LAW
  source("./module_completion_time.R", local = TRUE)
  output$plot11 <- renderPlotly({
    filters <- get_active_filters(input)  # Get active filters from user input
    plot <- module_completion_time(data = filtered_verified_data(), survey = "LAW")  # Generate the plot
    apply_plotly_config(plot, filters, "module_completion_time_LAW")  # Apply the config
  })
  
  # Participant Status Funnel
  source("./participant_status_funnel2.R", local = TRUE)
  output$plotFunnel <- renderPlotly({
    filters <- get_active_filters(input)  # Get active filters from user input
    plot <- participant_status(status_data = aggregated_IP_data())  # Generate the plot
    apply_plotly_config(plot, filters, "participant_status_funnel")  # Apply the config
  })
  
  
  # source("./activity_plot.R", local = TRUE)
  # output$plot1 <- renderPlotly({activity_plot(activity_data = filtered_verified_data())})
  # 
  # source("./age_plot.R", local = TRUE)
  # output$plot2 <- renderPlotly({age_plot(age_data = filtered_verified_data())})
  # 
  # source("./race_plot.R", local = TRUE)
  # output$plot3 <- renderPlotly({race_plot(race_data = filtered_verified_data())})
  # 
  # source("./sex_distribution.R", local = TRUE)
  # output$plot4 <- renderPlotly({sex_distribution(sex_data = filtered_verified_data())})
  # 
  # source("./biospecimen_collection_distribution.R", local = TRUE)
  # output$plot5 <- renderPlotly({biospecimen_collection_distribution(biocol_data = filtered_verified_data())})
  # 
  # source("./biospecimen_collection_barchart.R", local = TRUE)
  # output$plot5b <- renderPlotly({biospecimen_collection_barchart(biocol_data = filtered_verified_data())})
  # 
  # source("./completed_survey.R", local = TRUE)
  # output$plot6 <- renderPlotly({completed_survey(survey_data = filtered_verified_data())})
  # 
  # source("./completed_survey_barchart.R", local = TRUE)
  # output$plot6b <- renderPlotly({completed_survey_barchart(survey_data = filtered_verified_data())})
  # 
  # source("./income_distribution.R", local = TRUE)
  # output$plot7 <- renderPlotly({income_distribution(income_data = filtered_verified_data())})
  # 
  # source("./module_completion_time.R", local = TRUE)
  # output$plot8 <- renderPlotly({module_completion_time(data = filtered_verified_data(), survey = "BOH")})
  # 
  # source("./module_completion_time.R", local = TRUE)
  # output$plot9 <- renderPlotly({module_completion_time(data= filtered_verified_data(), survey = "SAS")})
  # 
  # source("./module_completion_time.R", local = TRUE)
  # output$plot10 <- renderPlotly({module_completion_time(data= filtered_verified_data(), survey = "MRE")})
  # 
  # source("./module_completion_time.R", local = TRUE)
  # output$plot11 <- renderPlotly({module_completion_time(data= filtered_verified_data(), survey = "LAW")})
  # 
  # source("./participant_status_funnel2.R", local = TRUE)
  # output$plotFunnel <- renderPlotly({participant_status(status_data = aggregated_IP_data())})
  
  ## Load verified participants date -------------------------------------------
  invited_participant_data <- reactive({
    source("./clean_data.R", local = TRUE)
    source("./get_data.R", local=TRUE)
    invited_participant_data <- clean_data(get_data(project = "nih-nci-dceg-connect-bq2-prod",
                                                    dataset = "StakeHolderMetrics_RS",
                                                    table = "invited_participants_complete"),
                                           type = "invited")})
  
  
  filtered_IP_data <- reactive({
    req(invited_participant_data())
    if(input$applyIPfilters){
      invited_participant_data()%>%
        filter((input$IPsiteFilter == "." | site == input$IPsiteFilter) &
                 (input$IPsexFilter == "." | sex == input$IPsexFilter) &
                 (input$IPageFilter == "." | age == input$IPageFilter)&
                 (input$IPraceFilter == "." | race == input$IPraceFilter))
    }else{
      invited_participant_data()
    }
  })
  
  #calculating the number of invited participants by filter
  IP_row_count <- reactive({
    nrow(filtered_IP_data())
  })
  
  output$IP_rowCountText <- renderText({
    paste("Number of Invited Participants with Current Filters Applied: \n", IP_row_count())
  })
  
  # Generate Invited Participants Figures --------------------------------------
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
  
  
  # Load aggregate metrics data ------------------------------------------------
  aggregate_recruitment_data <- reactive({
    source("./get_data.R", local=TRUE)
    source("./clean_data.R", local=TRUE)
    aggregate_recruitment_data <- clean_data(get_data(project ="nih-nci-dceg-connect-bq2-prod",
                                                    dataset = "StakeHolderMetrics_RS",
                                                    table = "aggregate_recruitment"),
                                                    type = "aggregate")
  })

  
  #HP aggregate data
   source("./aggregate_plots/HealthPartners/verified_by_race.R", local = TRUE)
    output$hp_aggregate_plot1 <- renderPlotly({
    verified_by_race_hp(data = aggregate_recruitment_data())
  })   
   source("./aggregate_plots/HealthPartners/response_ratio_by_race.R", local = TRUE)
   output$hp_aggregate_plot1b <- renderPlotly({
     response_ratio_by_race_hp(data = aggregate_recruitment_data())})
   
   source("./aggregate_plots/HealthPartners/verified_by_sex.R", local = TRUE)
   output$hp_aggregate_plot2 <- renderPlotly({verified_by_sex_hp(data = aggregate_recruitment_data())})
   
   source("./aggregate_plots/HealthPartners/response_ratio_by_sex.R", local = TRUE)
   output$hp_aggregate_plot2b <- renderPlotly({response_ratio_by_sex_hp(data = aggregate_recruitment_data())})
   
   source("./aggregate_plots/HealthPartners/verified_by_insurance.R", local = TRUE)
   output$hp_aggregate_plot3 <- renderPlotly({verified_by_insurance_hp(data = aggregate_recruitment_data())})
    
   source("./aggregate_plots/HealthPartners/response_ratio_by_insurance.R", local = TRUE)
   output$hp_aggregate_plot3b <- renderPlotly({response_ratio_by_insurance_hp(data = aggregate_recruitment_data())})
 
   source("./aggregate_plots/HealthPartners/verified_by_ses.R", local = TRUE)
   output$hp_aggregate_plot4 <- renderPlotly({verified_by_ses_hp(data = aggregate_recruitment_data())})
   
   source("./aggregate_plots/HealthPartners/response_ratio_by_ses.R", local = TRUE)
   output$hp_aggregate_plot4b <- renderPlotly({response_ratio_by_ses_hp(data = aggregate_recruitment_data())})
   
   source("./aggregate_plots/HealthPartners/verified_by_ruca.R", local = TRUE)
   output$hp_aggregate_plot5 <- renderPlotly({verified_by_ruca_hp(data = aggregate_recruitment_data())})

   source("./aggregate_plots/HealthPartners/response_ratio_by_ruca.R", local = TRUE)
   output$hp_aggregate_plot5b <- renderPlotly({response_ratio_by_ruca_hp(data = aggregate_recruitment_data())})
   

   # #SF aggregate data
   source("./aggregate_plots/SanfordHealth/verified_by_race.R", local = TRUE)
   output$sf_aggregate_plot1 <- renderPlotly({verified_by_race_sf(data = aggregate_recruitment_data())
   })
    
    source("./aggregate_plots/SanfordHealth/response_ratio_by_race.R", local = TRUE)
    output$sf_aggregate_plot1b <- renderPlotly({response_ratio_by_race_sf(data = aggregate_recruitment_data())})

   source("./aggregate_plots/SanfordHealth/verified_by_sex.R", local = TRUE)
   output$sf_aggregate_plot2 <- renderPlotly({verified_by_sex_sf(data = aggregate_recruitment_data())})

   source("./aggregate_plots/SanfordHealth/response_ratio_by_sex.R", local = TRUE)
   output$sf_aggregate_plot2b <- renderPlotly({response_ratio_by_sex_sf(data = aggregate_recruitment_data())})

   source("./aggregate_plots/SanfordHealth/verified_by_insurance.R", local = TRUE)
   output$sf_aggregate_plot3 <- renderPlotly({verified_by_insurance_sf(data = aggregate_recruitment_data())})

   source("./aggregate_plots/SanfordHealth/response_ratio_by_insurance.R", local = TRUE)
   output$sf_aggregate_plot3b <- renderPlotly({response_ratio_by_insurance_sf(data = aggregate_recruitment_data())})

   source("./aggregate_plots/SanfordHealth/verified_by_ses.R", local = TRUE)
   output$sf_aggregate_plot4 <- renderPlotly({verified_by_ses_sf(data = aggregate_recruitment_data())})

   source("./aggregate_plots/SanfordHealth/response_ratio_by_ses.R", local = TRUE)
   output$sf_aggregate_plot4b <- renderPlotly({response_ratio_by_ses_sf(data = aggregate_recruitment_data())})

   source("./aggregate_plots/SanfordHealth/verified_by_ruca.R", local = TRUE)
   output$sf_aggregate_plot5 <- renderPlotly({verified_by_ruca_sf(data = aggregate_recruitment_data())})

   source("./aggregate_plots/SanfordHealth/response_ratio_by_ruca.R", local = TRUE)
   output$sf_aggregate_plot5b <- renderPlotly({response_ratio_by_ruca_sf(data = aggregate_recruitment_data())})


    #MF aggregate data
   source("./aggregate_plots/MarshfieldClinicHealthSystem/verified_by_race.R", local = TRUE)
   output$mf_aggregate_plot1 <- renderPlotly({verified_by_race_mf(data = aggregate_recruitment_data())})

   source("./aggregate_plots/MarshfieldClinicHealthSystem/response_ratio_by_race.R", local = TRUE)
   output$mf_aggregate_plot1b <- renderPlotly({response_ratio_by_race_mf(data = aggregate_recruitment_data())})

   source("./aggregate_plots/MarshfieldClinicHealthSystem/verified_by_sex.R", local = TRUE)
   output$mf_aggregate_plot2 <- renderPlotly({verified_by_sex_mf(data = aggregate_recruitment_data())})

   source("./aggregate_plots/MarshfieldClinicHealthSystem/response_ratio_by_sex.R", local = TRUE)
   output$mf_aggregate_plot2b <- renderPlotly({response_ratio_by_sex_mf(data = aggregate_recruitment_data())})

   source("./aggregate_plots/MarshfieldClinicHealthSystem/verified_by_insurance.R", local = TRUE)
   output$mf_aggregate_plot3 <- renderPlotly({verified_by_insurance_mf(data = aggregate_recruitment_data())})

   source("./aggregate_plots/MarshfieldClinicHealthSystem/response_ratio_by_insurance.R", local = TRUE)
   output$mf_aggregate_plot3b <- renderPlotly({response_ratio_by_insurance_mf(data = aggregate_recruitment_data())})

   source("./aggregate_plots/MarshfieldClinicHealthSystem/verified_by_ses.R", local = TRUE)
   output$mf_aggregate_plot4 <- renderPlotly({verified_by_ses_mf(data = aggregate_recruitment_data())})

   source("./aggregate_plots/MarshfieldClinicHealthSystem/response_ratio_by_ses.R", local = TRUE)
   output$mf_aggregate_plot4b <- renderPlotly({response_ratio_by_ses_mf(data = aggregate_recruitment_data())})

   source("./aggregate_plots/MarshfieldClinicHealthSystem/verified_by_ruca.R", local = TRUE)
   output$mf_aggregate_plot5 <- renderPlotly({verified_by_ruca_mf(data = aggregate_recruitment_data())})

   source("./aggregate_plots/MarshfieldClinicHealthSystem/response_ratio_by_ruca.R", local = TRUE)
   output$mf_aggregate_plot5b <- renderPlotly({response_ratio_by_ruca_mf(data = aggregate_recruitment_data())})


   #UC aggregate data
   source("./aggregate_plots/UniversityofChicago/verified_by_race.R", local = TRUE)
   output$uc_aggregate_plot1 <- renderPlotly({verified_by_race_uc(data = aggregate_recruitment_data())})

   source("./aggregate_plots/UniversityofChicago/response_ratio_by_race.R", local = TRUE)
   output$uc_aggregate_plot1b <- renderPlotly({response_ratio_by_race_uc(data = aggregate_recruitment_data())})

   source("./aggregate_plots/UniversityofChicago/verified_by_sex.R", local = TRUE)
   output$uc_aggregate_plot2 <- renderPlotly({verified_by_sex_uc(data = aggregate_recruitment_data())})

   source("./aggregate_plots/UniversityofChicago/response_ratio_by_sex.R", local = TRUE)
   output$uc_aggregate_plot2b <- renderPlotly({response_ratio_by_sex_uc(data = aggregate_recruitment_data())})

   source("./aggregate_plots/UniversityofChicago/verified_by_insurance.R", local = TRUE)
   output$uc_aggregate_plot3 <- renderPlotly({verified_by_insurance_uc(data = aggregate_recruitment_data())})

   source("./aggregate_plots/UniversityofChicago/response_ratio_by_insurance.R", local = TRUE)
   output$uc_aggregate_plot3b <- renderPlotly({response_ratio_by_insurance_uc(data = aggregate_recruitment_data())})

   source("./aggregate_plots/UniversityofChicago/verified_by_ses.R", local = TRUE)
   output$uc_aggregate_plot4 <- renderPlotly({verified_by_ses_uc(data = aggregate_recruitment_data())})

   source("./aggregate_plots/UniversityofChicago/response_ratio_by_ses.R", local = TRUE)
   output$uc_aggregate_plot4b <- renderPlotly({response_ratio_by_ses_uc(data = aggregate_recruitment_data())})

   source("./aggregate_plots/UniversityofChicago/verified_by_ruca.R", local = TRUE)
   output$uc_aggregate_plot5 <- renderPlotly({verified_by_ruca_uc(data = aggregate_recruitment_data())})

   source("./aggregate_plots/UniversityofChicago/response_ratio_by_ruca.R", local = TRUE)
   output$uc_aggregate_plot5b <- renderPlotly({response_ratio_by_ruca_uc(data = aggregate_recruitment_data())})
   
   
   #HF aggregate data
   source("./aggregate_plots/HenryFord/verified_by_race.R", local = TRUE)
   output$hf_aggregate_plot1 <- renderPlotly({verified_by_race_hf(data = aggregate_recruitment_data())})
   
   source("./aggregate_plots/HenryFord/response_ratio_by_race.R", local = TRUE)
   output$hf_aggregate_plot1b <- renderPlotly({response_ratio_by_race_hf(data = aggregate_recruitment_data())})
   
   source("./aggregate_plots/HenryFord/verified_by_sex.R", local = TRUE)
   output$hf_aggregate_plot2 <- renderPlotly({verified_by_sex_hf(data = aggregate_recruitment_data())})
   
   source("./aggregate_plots/HenryFord/response_ratio_by_sex.R", local = TRUE)
   output$hf_aggregate_plot2b <- renderPlotly({response_ratio_by_sex_hf(data = aggregate_recruitment_data())})
   
   source("./aggregate_plots/HenryFord/verified_by_insurance.R", local = TRUE)
   output$hf_aggregate_plot3 <- renderPlotly({verified_by_insurance_hf(data = aggregate_recruitment_data())})
   
   source("./aggregate_plots/HenryFord/response_ratio_by_insurance.R", local = TRUE)
   output$hf_aggregate_plot3b <- renderPlotly({response_ratio_by_insurance_hf(data = aggregate_recruitment_data())})
   
   source("./aggregate_plots/HenryFord/verified_by_ses.R", local = TRUE)
   output$hf_aggregate_plot4 <- renderPlotly({verified_by_ses_hf(data = aggregate_recruitment_data())})
   
   source("./aggregate_plots/HenryFord/response_ratio_by_ses.R", local = TRUE)
   output$hf_aggregate_plot4b <- renderPlotly({response_ratio_by_ses_hf(data = aggregate_recruitment_data())})
   
   source("./aggregate_plots/HenryFord/verified_by_ruca.R", local = TRUE)
   output$hf_aggregate_plot5 <- renderPlotly({verified_by_ruca_hf(data = aggregate_recruitment_data())})
   
   source("./aggregate_plots/HenryFord/response_ratio_by_ruca.R", local = TRUE)
   output$hf_aggregate_plot5b <- renderPlotly({response_ratio_by_ruca_hf(data = aggregate_recruitment_data())})
   
   
## Generate data for Fast Facts value boxes ------------------------------------
  fast_facts_reactive <- reactive({
    source("./fast_facts2.R", local = TRUE)
    verified_data <- verified_data()  # Assuming this is how you get your verified data
    fast_facts(verified_data = verified_data)
  })
  
  output$totalVerifiedBox <- renderValueBox({
    facts <- fast_facts_reactive()
    valueBox(value = tags$p(paste0(facts$total_verified), style = "font-size:75%;"),
             subtitle = tags$p("Participants", style = "font-size: 95%"),
             icon = tags$i(class = "fas fa-users", style = "font-size: 40px; color: #FDBE19"))
  })
  
  
  output$maleVerifiedBox <- renderValueBox({
    facts <- fast_facts_reactive()
    valueBox(value = tags$p(paste0(facts$male_verified), style = "font-size: 75%;"),
             subtitle = tags$p("Male", style = "font-size: 95%"),
             icon = tags$i(class = "fas fa-mars", style = "font-size: 40px; color: #FDBE19"))
  })
  
  output$femaleVerifiedBox <- renderValueBox({
    facts <- fast_facts_reactive()
    valueBox(value = tags$p(paste0(facts$female_verified), style = "font-size: 75%;"),
             subtitle = tags$p("Female", style = "font-size: 95%"),
             icon = tags$i(class = "fas fa-venus", style = "font-size: 40px; color: #FDBE19"))
  })
  
  
  output$commonIncomeBox <- renderValueBox({
    facts <- fast_facts_reactive()
    # Split the string at the hyphen
    income_parts <- unlist(strsplit(facts$common_income, "-"))
    # Prepare the two lines with the hyphen and line break
    first_line <- paste0(income_parts[1], "-")
    second_line <- income_parts[2]
    # Combine lines with a line break for the subtitle
    formatted_subtitle <- paste0("$",first_line, second_line)
    valueBox(value = tags$p(formatted_subtitle, style = "font-size: 75%;"),
             subtitle = tags$p("Median Annual Income", style = "font-size: 95%"),
             icon = tags$i(class = "fas fa-money-bill", style = "font-size: 40px; color: #FDBE19"))
  })
  
  
  # output$raceVerifiedBox <- renderValueBox({
  #   facts <- fast_facts_reactive()
  #   valueBox(value = tags$p(paste0(facts$common_race), style = "font-size: 75%;"),
  #            subtitle = tags$p("Most Common Race", style = "font-size: 95%"),
  #            icon = tags$i(class = "fas fa-users", style = "font-size: 40px; color: #FDBE19"))
  # })
}





# UI ===========================================================================
ui <- dashboardPage(title="Stakeholder Metrics Dashboard", 
  
  
## Header ----------------------------------------------------------------------
  dashboardHeader(title = img(src="connect_logo.png", height = 120/2.8, width = 404/2.8, align = "center")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Verified Participants",     tabName = "verified_participants"),
      menuItem("Invited Participants",      tabName = "invited_participants"),
      menuItem("Site-reported Recruitment", tabName = "site_reported_participants")
    )
  ),


## Body ------------------------------------------------------------------------
  dashboardBody(
    
    # Link to external CSS file
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),

    # Wrap the main content in the fixed-width container
    div(class = "fixed-width-container",
      tabItems(
        

        
### Tab: Verified participants -------------------------------------------------
        tabItem(tabName = "verified_participants",
                
                # Title
                fluidRow(
                  column(12, 
                         h1("Stakeholder Metrics Dashboard", style = "text-align: center;"),
                         br(),
                         h4(HTML(glue("Verified Participant Data as of {format(Sys.Date(), '%B %d, %Y')}")), style = "text-align: center;"),
                         br(), br()
                         )
                ),
                
                
#### Row: Value Boxes and Workflow Plot in One Row -----------------------------
                
                # fluidRow(
                #   column(width = 3, valueBoxOutput("totalVerifiedBox", width = 12)),
                #   column(width = 3, valueBoxOutput("maleVerifiedBox", width = 12)),
                #   column(width = 3, valueBoxOutput("femaleVerifiedBox", width = 12)),
                #   column(width = 3, valueBoxOutput("commonIncomeBox", width = 12))
                # ),

                fluidRow(
                  column(4,
                         box(title = NULL, width = 12, solidHeader = FALSE, status = "primary",
                             column(width = 12, offset = 0,
                                    valueBoxOutput("totalVerifiedBox", width = 12),
                                    valueBoxOutput("maleVerifiedBox", width = 12),
                                    valueBoxOutput("femaleVerifiedBox", width = 12),
                  #                   valueBoxOutput("commonIncomeBox", width = 12),
                                    tags$figcaption("Note: participant-reported data.",
                                                    style = "text-align:center; font-style:italic")
                             )
                         )
                  ),
                  
                  # Using aspect_box() for the plot with a 16x9 aspect ratio
                  column(8,
                         aspect_box(
                           title = "Participant Workflow by Site", 
                           content = plotlyOutput("plotFunnel", height = "100%", width = "100%"),
                           aspect_ratio = "16x9",  # You can adjust the aspect ratio here
                         )
                  )
                ),
                
#### Row: Map ------------------------------------------------------------------
                fluidRow(
                  column(12, offset=0,
                         box(title = "Map of Catchment Coverage", width = 12,  solidHeader = TRUE, status = "primary",
                             tags$img(src = "map_site_color_Mar2023.jpg", style = "width:100%; height:auto;"),
                             tags$figcaption("Map of Catchment Coverage by Site as of March 2023",
                                             style = "text-align:center; font-style:italic")
                         )
                  )
                ),
                
#### Row: Demographic Statistics Section Break ---------------------------------
                
                fluidRow(
                  column(12, br(), br(), 
                         h2("Demographic Statistics"),
                         div(style ="height: 2px; background-color: black; margin: 20px 0;"),
                         br()
                  )
                ),
                
#### Row: Filters Section ------------------------------------------------------
                fluidRow(
                  column(12,  # Full-width outer container
                         box(title = "Select Filters:", status = "primary", width = 12, solidHeader = FALSE, 
                             fluidRow(
                               column(4,  # First column
                                      selectInput("siteFilter", "Site", choices = c("All" = ".", "HealthPartners" = 531629870, "Henry Ford Health System" = 548392715, "Kaiser Permanente Colorado" = 125001209, "Kaiser Permanente Georgia" = 327912200, "Kaiser Permanente Hawaii" = 300267574, "Kaiser Permanente Northwest" = 452412599, "Marshfield Clinic Health System" = 303349821, "Sanford Health" = 657167265, "University of Chicago Medicine" = 809703864, "Other" = 181769837), selected = "All"),
                                      selectInput("sexFilter", "Gender", choices = c("All" = ".", "Male" = "Male", "Female" = "Female", "Nonbinary" = "Nonbinary", "Unknown" = "Unknown"), selected = "All"),
                                      selectInput("ageFilter", "Age", choices = c("All" = ".", "40-45" = "40-45", "46-50" = "46-50", "51-55" = "51-55", "56-60" = "56-60", "61-65" = "61-65", "66-70" = "66-70", "Unknown" = "UNKNOWN"), selected = "All")
                               ),
                               column(4,  # Second column
                                      selectInput("raceFilter", "Race", choices = c("All" = ".", "American Indian or Alaska Native" = "American Indian or Alaska Native", "Asian" = "Asian", "Black, African American, or African" = "Black, African American, or African", "Hawaiian or other Pacific Islander" = "Hawaiian or other Pacific Islander", "Hispanic, Latino, or Spanish" = "Hispanic, Latino, or Spanish", "Middle Eastern or North African" = "Middle Eastern or North African", "Multi-race" = "Multi-race", "Other" = "Other", "Skipped this question" = "Skipped this question", "Unknown" = "UNKNOWN", "White" = "White"), selected = "All"),
                                      selectInput("campaignFilter", "Campaign", choices = c("All" = ".", "Random" = 926338735, "Screening appointment" = 348281054, "Non-screening appointment" = 324692899, "Demographic Group" = 351257378, "Aging out of study" = 647148178, "Geographic group" = 834544960, "Post-Screening Selection" = 682916147, "Technology adapters" = 153365143, "Low-income/health professional shortage areas" = 663706936, "Research Registry" = 208952854, "Pop up" = 296312382, "Other" = 181769837, "None of these apply" = 398561594, "NA/Unknown" = NA), selected = "All"),
                                      selectInput("biospecFilter", "Biospecimen Collection Type", choices = c("All" = ".", "All 3 Sample Donations" = "All 3 Sample Donations", "Blood & Urine" = "Blood & Urine", "Blood & Mouthwash" = "Blood & Mouthwash", "Mouthwash & Urine" = "Mouthwash & Urine", "Blood Only" = "Blood Only", "Urine Only" = "Urine Only", "Mouthwash Only" = "Mouthwash Only", "No Samples" = "No Samples"), selected = "All")
                               ),
                               column(4,  # Third column
                                      selectInput("surveycompleteFilter", "Survey Completion Level", choices = c("All" = ".", "BOH only" = "BOH only", "BOH and MRE" = "BOH and MRE", "BOH and SAS" = "BOH and SAS", "BOH and LAW" = "BOH and LAW", "BOH, MRE, and SAS" = "BOH, MRE, and SAS", "BOH, MRE, and LAW" = "BOH, MRE, and LAW", "BOH, SAS, and LAW" = "BOH, SAS, and LAW", "No Survey Sections" = "No Survey Sections"), selected = "All"),
                                      actionButton("applyFilters", "Apply Filters"),
                                      br(),
                                      textOutput("rowCountText")
                               )
                             )
                         )
                  )
                ),
                
#### Row: Income ---------------------------------------------------------------
                fluidRow(
                  column(6,
                         aspect_box(title = "Self-Reported Anual Income", content = plotlyOutput("plot7", height = "100%", width = "100%"), aspect_ratio = "16x9", max_width = 600, min_width = 100), 
                         aspect_box(title = "Self-Reported Race", content = plotlyOutput("plot3", height = "100%", width = "100%"), aspect_ratio = "4x3", max_width = 600, min_width = 100),
                  ),
                  column(6,
                         aspect_box(title = "Self-Reported Age", content = plotlyOutput("plot2", height = "100%", width = "100%"), aspect_ratio = "16x9", max_width = 600, min_width = 100),
                         aspect_box(title = "Site-Reported Sex", content = plotlyOutput("plot4", height = "100%", width = "100%"), aspect_ratio = "4x3", max_width = 600, min_width = 100)
                         )
                  ),

#### Row: Survey Completion Statistics -----------------------------------------
                
                fluidRow(
                  column(12, br(), br(), 
                         h2("Survey Completion Statistics"),
                         div(style ="height: 2px; background-color: black; margin: 20px 0;"),
                         br()
                  )
                ),
                
#### Row: Completion Times -----------------------------------------------------
                fluidRow(
                  column(6,
                         aspect_box(title = "Distribution of Biospecimen Collections by Type", content = plotlyOutput("plot5b", height = "100%", width = "100%"), aspect_ratio = "4x3", max_width = 600, min_width = 100),
                         aspect_box(title = "Survey Completion by Module", content = plotlyOutput("plot6b", height = "100%", width = "100%"), aspect_ratio = "4x3", max_width = 600, min_width = 100)
                  ),
                  column(6,
                         aspect_box(title = "Distribution of Biospecimen Collections", content = plotlyOutput("plot5", height = "100%", width = "100%"), aspect_ratio = "4x3", max_width = 600, min_width = 100),
                         aspect_box(title = "Survey Completion Status", content = plotlyOutput("plot6", height = "100%", width = "100%"), aspect_ratio = "4x3", max_width = 600, min_width = 100)
                  )
                ),
                
                fluidRow(
                  column(2),
                  box(width = 8, solidHeader = FALSE, 
                             column(width = 6,
                                    markdown(
                                            "**BOH**: Background & Overall Health \n
                                             **LAW**: Where You Live & Work"
                                            )
                                    ),
                                    column(width = 6,
                                           markdown(
                                            "**MRE**: Medications, Reproductive Health, Exercise & Sleep \n
                                             **SAS**: Smoking, Alcohol, & Sun Exposure" 
                                           )
                             ),
                          br(), br(),
                      ),
                 
                ),

#### Row: Counts by Study Activity Plot ----------------------------------------
                fluidRow(
                  column(8, offset = 2,
                         aspect_box(title = "Cumulative Count of Participants by Study Activity", content = plotlyOutput("plot1", height = "100%", width = "100%"), aspect_ratio = "16x9")
                  )
                ),
                
#### Row: Final Plots ----------------------------------------------------------
                fluidRow(
                  column(6,
                         aspect_box(title = "BOH Survey Completion Times", content = plotlyOutput("plot8", height = "100%", width = "100%"), aspect_ratio = "16x9"),
                         aspect_box(title = "SAS Survey Completion Times", content = plotlyOutput("plot9", height = "100%", width = "100%"), aspect_ratio = "16x9")
                  ),
                  column(6,
                         aspect_box(title = "MRE Survey Completion Times", content = plotlyOutput("plot10", height = "100%", width = "100%"), aspect_ratio = "16x9"),
                         aspect_box(title = "LAW Survey Completion Times", content = plotlyOutput("plot11", height = "100%", width = "100%"), aspect_ratio = "16x9")
                  )
                )
        ),
        
        


### Tab: Invited Participants  ---------------------------------------------
        tabItem(tabName = "invited_participants",
                
#### Row: Header -------------------------------------------------------
                fluidRow(
                  column(12, align = "center", 
                         tags$h3(style = "text-align: center;", 
                                 HTML(glue("Invited Participant Data as of {format(Sys.Date(), '%B %d, %Y')}"))),
                         br(),
                         tags$h5(style = "text-align: center;", 
                                 HTML(glue("Note: All invited participant data is site-reported")))
                  )
                ),
                
#### Row: Filters --------------------------------------------------------------
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
                             actionButton("applyIPfilters", "Apply Filters"),
                             br(),
                             textOutput("IP_rowCountText"))),
                  column(4, plotlyOutput("invited_plot1")),
                  column(4, plotlyOutput("invited_plot1b"))),
                
#### Row: --------------------------------------------------------
                fluidRow(
                  column(4, ),
                  column(4, plotlyOutput("invited_plot2")),
                  column(4, plotlyOutput("invited_plot2b"))),
                
#### Row: --------------------------------------------------------
                fluidRow(
                  column(4, ),
                  column(4, plotlyOutput("invited_plot3")),
                  column(4, plotlyOutput("invited_plot3b")))
        ),
        
        
### Tab: Site-reported participants -------------------------------------------
        tabItem(tabName = "site_reported_participants",
                fluidRow(
                  column(12, align = "center", 
                         tags$h3(style = "text-align: center;", 
                                 HTML(glue("Site-reported Recruitment")))
                  )
                ),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(12, align = "center", 
                         tags$h5(style = "text-align: center;", 
                                 HTML(glue("Note: Extreme outliers have been removed.
                                           All values shown are cumulative.")))
                  )
                ),
#### Row: ----------------------------------------------------------------------
                 fluidRow(
                   column(12,
                          h4("HealthPartners-reported Aggregate Recruitment Metrics", style = "color: black"),
                          div(style ="height: 7px; background-color: black; margin: 20px 0;"),  # Adjust the height and color
                          
                   )
                 ),
#### Row: ----------------------------------------------------------------------
                 fluidRow(
                   column(6, plotlyOutput("hp_aggregate_plot1")),
                   column(6, plotlyOutput("hp_aggregate_plot1b"))),
#### Row: ----------------------------------------------------------------------
                 fluidRow(
                   column(6, plotlyOutput("hp_aggregate_plot2")),
                   column(6, plotlyOutput("hp_aggregate_plot2b"))),
#### Row: ----------------------------------------------------------------------
                 fluidRow(
                   column(6, plotlyOutput("hp_aggregate_plot3")),
                   column(6, plotlyOutput("hp_aggregate_plot3b"))),
#### Row: ----------------------------------------------------------------------
                 fluidRow(
                   column(6, plotlyOutput("hp_aggregate_plot4")),
                   column(6, plotlyOutput("hp_aggregate_plot4b"))),
#### Row: ----------------------------------------------------------------------
                 fluidRow(
                   column(6, plotlyOutput("hp_aggregate_plot5")),
                   column(6, plotlyOutput("hp_aggregate_plot5b"))),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(12,
                         h4("Sanford-reported Aggregate Recruitment Metrics", style = "color: black"),
                         div(style ="height: 7px; background-color: black; margin: 20px 0;"),  # Adjust the height and color
                  )
                ),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(6, plotlyOutput("sf_aggregate_plot1")),
                  column(6, plotlyOutput("sf_aggregate_plot1b"))),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(6, plotlyOutput("sf_aggregate_plot2")),
                  column(6, plotlyOutput("sf_aggregate_plot2b"))),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(6, plotlyOutput("sf_aggregate_plot3")),
                  column(6, plotlyOutput("sf_aggregate_plot3b"))),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(6, plotlyOutput("sf_aggregate_plot4")),
                  column(6, plotlyOutput("sf_aggregate_plot4b"))),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(6, plotlyOutput("sf_aggregate_plot5")),
                  column(6, plotlyOutput("sf_aggregate_plot5b"))),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(12,
                         h4("Marshfield-reported Aggregate Recruitment Metrics", style = "color: black"),
                         div(style ="height: 7px; background-color: black; margin: 20px 0;"),  # Adjust the height and color
                  )
                ),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(6, plotlyOutput("mf_aggregate_plot1")),
                  column(6, plotlyOutput("mf_aggregate_plot1b"))),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(6, plotlyOutput("mf_aggregate_plot2")),
                  column(6, plotlyOutput("mf_aggregate_plot2b"))),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(6, plotlyOutput("mf_aggregate_plot3")),
                  column(6, plotlyOutput("mf_aggregate_plot3b"))),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(6, plotlyOutput("mf_aggregate_plot4")),
                  column(6, plotlyOutput("mf_aggregate_plot4b"))),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(6, plotlyOutput("mf_aggregate_plot5")),
                  column(6, plotlyOutput("mf_aggregate_plot5b"))),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(12,
                         h4("University of Chicago-reported Aggregate Recruitment Metrics", style = "color: black"),
                         div(style ="height: 7px; background-color: black; margin: 20px 0;"),  # Adjust the height and color
                  )
                ),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(6, plotlyOutput("uc_aggregate_plot1")),
                  column(6, plotlyOutput("uc_aggregate_plot1b"))),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(6, plotlyOutput("uc_aggregate_plot2")),
                  column(6, plotlyOutput("uc_aggregate_plot2b"))),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(6, plotlyOutput("uc_aggregate_plot3")),
                  column(6, plotlyOutput("uc_aggregate_plot3b"))),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(6, plotlyOutput("uc_aggregate_plot4")),
                  column(6, plotlyOutput("uc_aggregate_plot4b"))),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(6, plotlyOutput("uc_aggregate_plot5")),
                  column(6, plotlyOutput("uc_aggregate_plot5b"))),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(12,
                         h4("Henry Ford Health-reported Aggregate Recruitment Metrics", style = "color: black"),
                         div(style ="height: 7px; background-color: black; margin: 20px 0;"),  # Adjust the height and color
                  )
                ),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(6, plotlyOutput("hf_aggregate_plot1")),
                  column(6, plotlyOutput("hf_aggregate_plot1b"))),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(6, plotlyOutput("hf_aggregate_plot2")),
                  column(6, plotlyOutput("hf_aggregate_plot2b"))),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(6, plotlyOutput("hf_aggregate_plot3")),
                  column(6, plotlyOutput("hf_aggregate_plot3b"))),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(6, plotlyOutput("hf_aggregate_plot4")),
                  column(6, plotlyOutput("hf_aggregate_plot4b"))),
#### Row: ----------------------------------------------------------------------
                fluidRow(
                  column(6, plotlyOutput("hf_aggregate_plot5")),
                  column(6, plotlyOutput("hf_aggregate_plot5b")))
        )
        )
    )
  )
)


# App ==========================================================================
shinyApp(ui, server)