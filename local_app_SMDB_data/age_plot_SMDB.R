#age plot using SMDB data
#activities plot
library(reshape2)
library(dplyr)
library(plotly)

#this reactive function will ensure that data from plot 1 is pulled and plotted
#before it is overwritten for plot2
age_plot <- function(){
  age_plot_project <- 'nih-nci-dceg-connect-prod-6d04'
  age_plot_dataset <- 'StakeHolderMetrics_tmp'
  age_plot_table   <- 'figure2_age'
  age_plot_variables <- '*'
  combined_query <- glue("SELECT ", paste(age_plot_variables, collapse = ", ")," FROM `", age_plot_project, ".", age_plot_dataset, ".", age_plot_table, "`" , sep = " ")
  # Download data
  data <- bq_table_download(bq_project_query(age_plot_project, query = combined_query), bigint = "integer64")
  
  #using the verification date as the censor date
  data$a <- 1
  
  agg <- aggregate(data$a, list(data$AgeUP_cat), FUN = sum)
  agg$Group.1 <- as.character(agg$Group.1)
  
  hospital_label='None'
  # Customize the layout
  fig <- plot_ly(agg, x = ~Group.1, y = ~x, type = 'bar', name = 'Age Groups')
  fig <- fig %>% layout(title = list(text=paste0("Ages of Verified Participants as of ", Sys.Date(), "\n hospital filter: ",hospital_label),font = list(size = 10)),
                        xaxis = list(title = 'Age Group'),
                        yaxis = list(title = 'Frequency'))
  fig
}