race_plot<- function(){

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
devtools::install_github("tidyverse/reprex")
options(tinytex.verbose = TRUE)



#################################################################
#pulling data from participant table, verified participants ONLY
{
bq_auth()
project <- "nih-nci-dceg-connect-prod-6d04"
billing <- "nih-nci-dceg-connect-prod-6d04" 
##517311251 Date/time Status of Completion of Background and Overall Health                         SrvBOH_TmComplete_v1r0
##949302066 Flag for Baseline Module Background and Overall Health                        SrvBOH_BaseStatus_v1r0
#downloading 8 variables
#recr_M1 <- bq_project_query(project, query="SELECT token,Connect_ID, d_821247024, d_914594314,  d_827220437,d_512820379,
#                            d_949302066 , d_517311251  FROM  `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants_JP` WHERE  d_821247024='197316935'")
recr_m1 <- bq_table_download(recr_M1,bigint = "integer64")
}
cnames <- names(recr_m1)

# Check that it doesn't match any non-number
numbers_only <- function(x) !grepl("\\D", x)
# to check variables in recr_noinact_wl1
for (i in 1: length(cnames)){
  varname <- cnames[i]
  var<-pull(recr_m1,varname)
  recr_m1[,cnames[i]] <- ifelse(numbers_only(var), as.numeric(as.character(var)), var)
}

#get merged module 1 and module 2 data
#variables to pull from modules 1 v1 and v2:
variables <- c("Connect_ID", "D_384191091_D_384191091_D_583826374", "D_384191091_D_384191091_D_636411467", "D_384191091_D_384191091_D_458435048",
               "D_384191091_D_384191091_D_706998638", "D_384191091_D_384191091_D_973565052", "D_384191091_D_384191091_D_586825330",
               "D_384191091_D_384191091_D_412790539", "D_384191091_D_384191091_D_807835037", "D_384191091_D_747350323", "D_384191091_D_384191091_D_746038746")
project <- "`nih-nci-dceg-connect-prod-6d04`"
source("/Users/sansalerj/Desktop/rshiny_app/get_merged_module_1_data.R")
#this function is only pulling the variables from module 1 v1 and v2 using the variables column specified above 
merged <- get_merged_module_1_data(project = project)

####################################################################################
#downloading a few variables from the participants table
{
parts <- "SELECT Connect_ID, token, D_512820379, D_471593703, state_d_934298480, D_230663853,
D_335767902, D_982402227, D_919254129, D_699625233, D_564964481, D_795827569, D_544150384,
D_371067537, D_430551721, D_821247024, D_914594314,  state_d_725929722, 
D_949302066 , D_517311251, D_205553981, D_117249500, d_430551721, d_517311251, d_544150384, d_564964481, d_117249500 FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants_JP` 
where Connect_ID IS NOT NULL"
parts_table <- bq_project_query(project, parts)
parts_data <- bq_table_download(parts_table, bigint = "integer64")
}
parts_data$Connect_ID <- as.numeric(parts_data$Connect_ID) ###need to convert type- m1... is double and parts is character

merged= left_join(merged, parts_data, by="Connect_ID") 
dim(merged)

#D_512820379 = recruitment type, 486306141 = active, 854703046 = passive
merged <- merged %>% filter(D_512820379==486306141 | D_512820379==854703046)

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
  text = ~pct,
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

