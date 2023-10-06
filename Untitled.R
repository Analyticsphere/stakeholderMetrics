#testing pre-join using big query 
# Define your project and dataset information
library(bigrquery)
library(DBI)
library(RSQLite)
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


#which variables do we want: race variables
variables <- c("Connect_ID", "D_384191091_D_384191091_D_583826374", "D_384191091_D_384191091_D_636411467", "D_384191091_D_384191091_D_458435048",
               "D_384191091_D_384191091_D_706998638", "D_384191091_D_384191091_D_973565052", "D_384191091_D_384191091_D_586825330",
               "D_384191091_D_384191091_D_412790539", "D_384191091_D_384191091_D_807835037", "D_384191091_D_747350323", "D_384191091_D_384191091_D_746038746")
#identifies population only captured by module 1 v1
v1_sql <- paste0(
  "SELECT ", paste(variables, collapse = ", "),
  sprintf(" FROM `%s.%s.%s`", project_id, dataset_id1, table_id1),
  " WHERE Connect_ID NOT IN ", paste(formatted_vs), " AND (D_384191091_D_384191091_D_583826374 = '1' OR D_384191091_D_384191091_D_636411467 = '1' OR D_384191091_D_384191091_D_458435048 ='1'
  OR D_384191091_D_384191091_D_706998638 = '1' OR D_384191091_D_384191091_D_973565052 ='1' OR D_384191091_D_384191091_D_586825330 ='1' OR D_384191091_D_384191091_D_412790539 = '1' OR D_384191091_D_384191091_D_807835037='1')"
)
#population captured by module 1 v 1 ONLY
v1 <- bq_table_download(bq_project_query(project_id, v1_sql))


#do the same thing for V2 
v2_sql <- paste0(
  "SELECT ", paste(variables, collapse = ", "),
  sprintf(" FROM `%s.%s.%s`", project_id, dataset_id2, table_id2),
  " WHERE Connect_ID NOT IN ", paste(formatted_vs), " AND (D_384191091_D_384191091_D_583826374 = '1' OR D_384191091_D_384191091_D_636411467 = '1' OR D_384191091_D_384191091_D_458435048 ='1'
  OR D_384191091_D_384191091_D_706998638 = '1' OR D_384191091_D_384191091_D_973565052 ='1' OR D_384191091_D_384191091_D_586825330 ='1' OR D_384191091_D_384191091_D_412790539 = '1' OR D_384191091_D_384191091_D_807835037='1')"
)
# Execute the query
v2 <- bq_table_download(bq_project_query(project_id, v2_sql))


#overlap (middle of ven diagram)
v2_overlap_sql <- paste0(
  "SELECT ", paste(variables, collapse = ", "),
  sprintf(" FROM `%s.%s.%s`", project_id, dataset_id2, table_id2),
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









