race_plot<- function(){
  #load libraries
  library(bigrquery)
  library(foreach)
  library(stringr)
  library(magrittr)
  library(arsenal)
  library(gtsummary)
  library(rio)
  library(ggplot2)
  library(gridExtra)
  library(scales)
  library(gt)
  library(tinytex)
  library(data.table) 
  library(tidyverse) 
  library(dplyr) 
  library(reshape) 
  library(listr) 
  library(sqldf) 
  library(lubridate)
  library(stringr)
  library(RColorBrewer)
  library(ggrepel)
  library(DBI)
  library(RSQLite)
  library(glue)
  library(plotly)
  devtools::install_github("tidyverse/reprex")
  options(tinytex.verbose = TRUE)
  #sql pre-merge
  #################################################################
  #################################################################
  #################################################################
  #Define project and dataset information
  project_id <- "nih-nci-dceg-connect-prod-6d04"
  dataset_id <- "FlatConnect"
  table_id1 <- "module1_v1_JP"
  table_id2 <- "module1_v2_JP"
  participant_table_id <- "participants_JP"
  dataset <- glue(project_id,".", dataset_id,".", table_id1)
  
  # Create references to BigQuery tables
  table1 <- bq_table(project_id, dataset_id, table_id1)
  table2 <- bq_table(project_id, dataset_id, table_id2)
  participants_table <- bq_table(project_id, dataset_id, participant_table_id)
  
  # SQL query to join tables using the Connect_ID column and select only the Connect_ID as output
  #inner join, V*
  v_star_query <- sprintf("SELECT t1.Connect_ID FROM `%s.%s.%s` AS t1 INNER JOIN `%s.%s.%s` AS t2 ON t1.Connect_ID = t2.Connect_ID", project_id, dataset_id, table_id1, project_id, dataset_id, table_id2)
  v_star <- bq_table_download(bq_project_query(project_id, v_star_query))
  vs <- c(v_star$Connect_ID)
  formatted_vs <- paste("('", paste(vs, collapse = "', '"), "')")
  
  
  #race variables from module 1 v1 and module 1 v2 tables
  variables <- c("Connect_ID", "D_384191091_D_384191091_D_583826374", "D_384191091_D_384191091_D_636411467", "D_384191091_D_384191091_D_458435048",
                 "D_384191091_D_384191091_D_706998638", "D_384191091_D_384191091_D_973565052", "D_384191091_D_384191091_D_586825330",
                 "D_384191091_D_384191091_D_412790539", "D_384191091_D_384191091_D_807835037", "D_384191091_D_747350323", "D_384191091_D_384191091_D_746038746")
  #identifies population only captured by module 1 v1
  v1_sql <- paste0(
    "SELECT ", paste(variables, collapse = ", "),
    sprintf(" FROM `%s.%s.%s`", project_id, dataset_id, table_id1),
    " WHERE Connect_ID NOT IN ", paste(formatted_vs), " AND (D_384191091_D_384191091_D_583826374 = '1' OR D_384191091_D_384191091_D_636411467 = '1' OR D_384191091_D_384191091_D_458435048 ='1'
  OR D_384191091_D_384191091_D_706998638 = '1' OR D_384191091_D_384191091_D_973565052 ='1' OR D_384191091_D_384191091_D_586825330 ='1' OR D_384191091_D_384191091_D_412790539 = '1' OR D_384191091_D_384191091_D_807835037='1')"
  )
  #population captured by module 1 v 1 ONLY
  v1 <- bq_table_download(bq_project_query(project_id, v1_sql))
  
  
  #do the same thing for V2 
  v2_sql <- paste0(
    "SELECT ", paste(variables, collapse = ", "),
    sprintf(" FROM `%s.%s.%s`", project_id, dataset_id, table_id2),
    " WHERE Connect_ID NOT IN ", paste(formatted_vs), " AND (D_384191091_D_384191091_D_583826374 = '1' OR D_384191091_D_384191091_D_636411467 = '1' OR D_384191091_D_384191091_D_458435048 ='1'
  OR D_384191091_D_384191091_D_706998638 = '1' OR D_384191091_D_384191091_D_973565052 ='1' OR D_384191091_D_384191091_D_586825330 ='1' OR D_384191091_D_384191091_D_412790539 = '1' OR D_384191091_D_384191091_D_807835037='1')"
  )
  # Execute the query
  v2 <- bq_table_download(bq_project_query(project_id, v2_sql))
  
  
  #overlap (middle of ven diagram, i.e. individuals who completed version 1 and version 2)
  v2_overlap_sql <- paste0(
    "SELECT ", paste(variables, collapse = ", "),
    sprintf(" FROM `%s.%s.%s`", project_id, dataset_id, table_id2),
    " WHERE Connect_ID IN ", paste(formatted_vs), " AND (D_384191091_D_384191091_D_583826374 = '1' OR D_384191091_D_384191091_D_636411467 = '1' OR D_384191091_D_384191091_D_458435048 ='1'
  OR D_384191091_D_384191091_D_706998638 = '1' OR D_384191091_D_384191091_D_973565052 ='1' OR D_384191091_D_384191091_D_586825330 ='1' OR D_384191091_D_384191091_D_412790539 = '1' OR D_384191091_D_384191091_D_807835037='1')"
  )
  # Execute the query
  v2_overlap <- bq_table_download(bq_project_query(project_id, v2_overlap_sql))
  
  #put all of the race data together
  module1 <- rbind(v1,v2,v2_overlap)
  

  #using these connect_ids, pull the participant data
  connect_ids_participant_table <- paste("('", paste(unique(module1$Connect_ID), collapse = "', '"), "')")
  
  #downloading a few variables from the participants table
  variables <- c("Connect_ID", "token", "D_512820379", "D_471593703", "state_d_934298480",
                 "D_230663853", "D_335767902", "D_982402227", "D_919254129", "D_699625233",
                 "D_564964481", "D_795827569", "D_544150384", "D_371067537", "D_430551721",
                 "D_821247024", "D_914594314", "state_d_725929722", "D_949302066", "D_517311251",
                 "D_205553981", "D_117249500", "d_430551721", "d_517311251", "d_544150384",
                 "d_564964481", "d_117249500")
  
  participant_query <- paste0(
    "SELECT ", paste(variables, collapse = ", "),
    sprintf(" FROM `%s.%s.%s`", project_id, dataset_id, participant_table_id),
    " WHERE Connect_ID IN ", paste(connect_ids_participant_table), " AND D_949302066 = '231311385'")
  #D_949302066 = BOH survey status, 231311385 = completed
  #download data
  participant_table <- bq_table_download(bq_project_query(project_id, participant_query))
  
  
  #combine the two datasets, only keeping individuals who have data from both datasets
  #need race data for the plot, need participant data for verification variables
  all_data <- inner_join(participant_table, module1, by = "Connect_ID")
#################################################################
#################################################################
#################################################################  
#done downloading bq data and merging, now data filtering
  
  
  
  
  
  
  
  
  #D_512820379 = recruitment type, 486306141 = active, 854703046 = passive
  merged <- all_data %>% filter(D_512820379==486306141 | D_512820379==854703046)
  
  #when consent form was submitted, 353358909 = YES
  merged <- merged %>% filter(D_919254129==353358909)
  
  #D_699625233 = user profile was submitted, 353358909 = YES
  merged <- merged %>% filter(D_699625233==353358909)
  
  
  #Mod1 completed (BOH survey completed)
  #D_949302066 = BOH survey status, 231311385 = completed
  module1 <- merged %>% filter(D_949302066==231311385)
  data_tib_m1 <- as_tibble(module1)
  
  
  
  
  
  
  
  ## Multi-racial
  #D_384191091 = which category describes you? select all that apply (race).
  #D_583826374 = american indian or alaska native
  #D_636411467 = asian/asian american
  #D_458435048 = black, african american
  #D_706998638 = hispanic/latino/spanish
  all_races <- data_tib_m1[ , c("Connect_ID", "D_384191091_D_384191091_D_583826374", "D_384191091_D_384191091_D_636411467", "D_384191091_D_384191091_D_458435048",
                                "D_384191091_D_384191091_D_706998638", "D_384191091_D_384191091_D_973565052", "D_384191091_D_384191091_D_586825330",
                                "D_384191091_D_384191091_D_412790539", "D_384191091_D_384191091_D_807835037", "D_384191091_D_747350323", "D_384191091_D_384191091_D_746038746")] 
  
  multi_race=0   
  #identifying individuals who checked more than 1 box
  for (i in 1:length(module1$Connect_ID)){
    AI=ifelse((module1$D_384191091_D_384191091_D_583826374[[i]]==1 & (module1$D_384191091_D_384191091_D_636411467[[i]]==1 | module1$D_384191091_D_384191091_D_458435048[[i]]==1|
                                                                        module1$D_384191091_D_384191091_D_706998638[[i]]==1 | module1$D_384191091_D_384191091_D_973565052[[i]]==1 |
                                                                        module1$D_384191091_D_384191091_D_586825330[[i]]==1 | module1$D_384191091_D_384191091_D_412790539[[i]]==1 |
                                                                        module1$D_384191091_D_384191091_D_807835037[[i]]==1)), 1, 0)
    As=ifelse((module1$D_384191091_D_384191091_D_636411467[[i]]==1 & (module1$D_384191091_D_384191091_D_583826374[[i]]==1 | module1$D_384191091_D_384191091_D_458435048[[i]]==1|
                                                                        module1$D_384191091_D_384191091_D_706998638[[i]]==1 | module1$D_384191091_D_384191091_D_973565052[[i]]==1 |
                                                                        module1$D_384191091_D_384191091_D_586825330[[i]]==1 | module1$D_384191091_D_384191091_D_412790539[[i]]==1 |
                                                                        module1$D_384191091_D_384191091_D_807835037[[i]]==1)), 1, 0)
    Bl=ifelse((module1$D_384191091_D_384191091_D_458435048[[i]]==1 & (module1$D_384191091_D_384191091_D_583826374[[i]]==1 | module1$D_384191091_D_384191091_D_636411467[[i]]==1|
                                                                        module1$D_384191091_D_384191091_D_706998638[[i]]==1 | module1$D_384191091_D_384191091_D_973565052[[i]]==1 |
                                                                        module1$D_384191091_D_384191091_D_586825330[[i]]==1 | module1$D_384191091_D_384191091_D_412790539[[i]]==1 |
                                                                        module1$D_384191091_D_384191091_D_807835037[[i]]==1)), 1, 0)
    Hs=ifelse((module1$D_384191091_D_384191091_D_706998638[[i]]==1 & (module1$D_384191091_D_384191091_D_583826374[[i]]==1 | module1$D_384191091_D_384191091_D_636411467[[i]]==1|
                                                                        module1$D_384191091_D_384191091_D_458435048[[i]]==1 | module1$D_384191091_D_384191091_D_973565052[[i]]==1 |
                                                                        module1$D_384191091_D_384191091_D_586825330[[i]]==1 | module1$D_384191091_D_384191091_D_412790539[[i]]==1 |
                                                                        module1$D_384191091_D_384191091_D_807835037[[i]]==1)), 1, 0)
    Me=ifelse((module1$D_384191091_D_384191091_D_973565052[[i]]==1 & (module1$D_384191091_D_384191091_D_583826374[[i]]==1 | module1$D_384191091_D_384191091_D_636411467[[i]]==1|
                                                                        module1$D_384191091_D_384191091_D_458435048[[i]]==1 | module1$D_384191091_D_384191091_D_706998638[[i]]==1 |
                                                                        module1$D_384191091_D_384191091_D_586825330[[i]]==1 | module1$D_384191091_D_384191091_D_412790539[[i]]==1 |
                                                                        module1$D_384191091_D_384191091_D_807835037[[i]]==1)), 1, 0)
    Hw=ifelse((module1$D_384191091_D_384191091_D_586825330[[i]]==1 & (module1$D_384191091_D_384191091_D_583826374[[i]]==1 | module1$D_384191091_D_384191091_D_636411467[[i]]==1|
                                                                        module1$D_384191091_D_384191091_D_458435048[[i]]==1 | module1$D_384191091_D_384191091_D_706998638[[i]]==1 |
                                                                        module1$D_384191091_D_384191091_D_973565052[[i]]==1 | module1$D_384191091_D_384191091_D_412790539[[i]]==1 |
                                                                        module1$D_384191091_D_384191091_D_807835037[[i]]==1)), 1, 0)
    Wh=ifelse((module1$D_384191091_D_384191091_D_412790539[[i]]==1 & (module1$D_384191091_D_384191091_D_583826374[[i]]==1 | module1$D_384191091_D_384191091_D_636411467[[i]]==1|
                                                                        module1$D_384191091_D_384191091_D_458435048[[i]]==1 | module1$D_384191091_D_384191091_D_706998638[[i]]==1 |
                                                                        module1$D_384191091_D_384191091_D_586825330[[i]]==1 | module1$D_384191091_D_384191091_D_973565052[[i]]==1 |
                                                                        module1$D_384191091_D_384191091_D_807835037[[i]]==1)), 1, 0)
    Ot=ifelse((module1$D_384191091_D_384191091_D_807835037[[i]]==1 & (module1$D_384191091_D_384191091_D_583826374[[i]]==1 | module1$D_384191091_D_384191091_D_636411467[[i]]==1|
                                                                        module1$D_384191091_D_384191091_D_458435048[[i]]==1 | module1$D_384191091_D_384191091_D_706998638[[i]]==1 |
                                                                        module1$D_384191091_D_384191091_D_586825330[[i]]==1 | module1$D_384191091_D_384191091_D_973565052[[i]]==1 |
                                                                        module1$D_384191091_D_384191091_D_412790539[[i]]==1)), 1, 0)
    multi_race= multi_race + sum(AI+As+Bl+Hs+Me+Hw+Wh+Ot, na.rm=T)
    
  }
  
  
  
  data_tib_m1$multi_racial <- c(rep(1, times=multi_race), rep(0, times=(dim(data_tib_m1)[1]- multi_race)))
  
  # Assign data to 'which_race' variable
  which_race <- data_tib_m1 %>%
    mutate(
      race = case_when(
        multi_racial == 1 ~ "Multi-Racial",
        D_384191091_D_384191091_D_583826374 == 1 ~ "American Indian or Native American",
        D_384191091_D_384191091_D_636411467 == 1 ~ "Asian/Asian American",
        D_384191091_D_384191091_D_458435048 == 1 ~ "Black, African American, or African",
        D_384191091_D_384191091_D_706998638 == 1 ~ "Hispanic, Latino, or Spanish",
        D_384191091_D_384191091_D_973565052 == 1 ~ "Middle Eastern or North African",
        D_384191091_D_384191091_D_586825330 == 1 ~ "Hawaiian or Pacific Islander",
        D_384191091_D_384191091_D_412790539 == 1 ~ "White",
        (D_384191091_D_384191091_D_807835037 == 1 | !is.na(D_384191091_D_747350323)) ~ "Other",
        D_384191091_D_384191091_D_746038746 == 1 ~ "Prefer Not to Answer",
        TRUE ~ "Skipped this question"
      )
    )
  # Calculate percentages by race
  dt_all_races_summary <- which_race %>%
    group_by(race) %>%
    summarize(n = n(), percentage = 100 * n / nrow(.)) %>%
    ungroup() %>%
    select(race, n, percentage)
  
  # Define aesthetically pleasing colors for the pie chart
  mycolors <- c("#053061", "#999999", "#F16913", "#FD8D3C", "#FFD92F", "#0072B2", "#009E73", "grey42", "plum3", "darkorchid4", "green4")
  names(mycolors) <- levels(dt_all_races_summary$race)
  
  # Add percentage labels
  dt_all_races_summary$pct <- paste0(round(as.numeric(dt_all_races_summary$percentage), digits = 2), "%")
  curr.date <- Sys.Date()
  race_M1plot <- plot_ly(
    data = dt_all_races_summary,
    labels = ~race,
    values = ~n,
    type = "pie",
    #text = ~pct,
    marker = list(colors = mycolors),
    textinfo = "none",  # Remove labels from inside the plot
    textposition = "outside"  # Place labels outside the plot
  ) %>%
    layout(
      title = paste0("Race of Participants Who Completed BOH Section of First Survey as of ",curr.date),
      legend = list(orientation = "h"),  # Add a legend on the right side (horizontal)
      xaxis = list(showticklabels = FALSE),  # Remove x-axis labels
      yaxis = list(showticklabels = FALSE),  # Remove y-axis labels
      showlegend = TRUE
    )
  
  # Display the pie chart
  race_M1plot
}

