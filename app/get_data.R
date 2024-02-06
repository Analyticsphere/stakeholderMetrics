get_data <- function(){
data_project <- 'nih-nci-dceg-connect-bq2-prod'
data_dataset <- 'StakeHolderMetrics_RS'
data_table   <- 'complete_table'

  
query <- glue("SELECT * FROM `", data_project, ".", data_dataset, ".", data_table, "`" , sep = " ")
data <- bq_table_download(bq_project_query(data_project, query = query), bigint = "integer64")
}
