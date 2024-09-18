get_data <- function(project = 'nih-nci-dceg-connect-prod-6d04', dataset = 'StakeholderMetrics', table = 'complete_verified'){

  
query <- glue("SELECT * FROM `", project, ".", dataset, ".", table, "`" , sep = " ")
data <- bq_table_download(bq_project_query(project, query = query), bigint = "integer64")
}
