#call data function
#the input of this function is a data table name and a list of variables
get_gcp_data <- function(variables, dataset, table, project = 'nih-nci-dceg-connect-prod-6d04', filter){
  library(bigrquery)
  library(glue)
  
  if(filter != 'NA'){
    combined_query <- glue("SELECT ", paste(variables, collapse = ", ")," FROM `", project, ".", dataset, ".", table, "`", paste0(filter, sep = " "), sep = " ")
  }else{combined_query <- glue("SELECT ", paste(variables, collapse = ", ")," FROM `", project, ".", dataset, ".", table, "`", "WHERE Connect_ID IS NOT NULL and d_821247024 = 197316935" , sep = " ")}
  

  # Download data
  data <- bigrquery::bq_project_query(project, query = combined_query)
  data <- bigrquery::bq_table_download(data, bigint = "integer64")

}
