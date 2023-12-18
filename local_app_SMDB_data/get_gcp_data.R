#call data function
#the input of this function is a data table name and a list of variables
get_gcp_data <- function(variables, dataset, table, project = 'nih-nci-dceg-connect-prod-6d04'){
  library(bigrquery)
  library(glue)

  #bq_auth()  
#  combined_query <- glue("SELECT ", paste(variables, collapse = ", ")," FROM `", project, ".", dataset, ".", table, "`", " WHERE Connect_ID IS NOT NULL and d_821247024 = '197316935' LIMIT 5000" , sep = " ")
  combined_query <- glue("SELECT ", paste(variables, collapse = ", ")," FROM `", project, ".", dataset, ".", table, "`" , sep = " ")
  
  # Download data
  data <- bq_table_download(bq_project_query(project, query = combined_query), bigint = "integer64")

}
