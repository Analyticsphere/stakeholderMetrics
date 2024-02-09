get_data <- function(project = 'nih-nci-dceg-connect-bq2-prod', dataset = 'StakeHolderMetrics_RS', table = 'complete_table'){

  
query <- glue("SELECT * FROM `", project, ".", dataset, ".", table, "`" , sep = " ")
data <- bq_table_download(bq_project_query(project, query = query), bigint = "integer64")
}
