#rm(list = ls())
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
#install(tinytex)
library(tinytex)
library(data.table) ###to write or read and data management 
library(tidyverse) ###for data management
library(dplyr) ###data management
library(reshape)  ###to work on transition from long to wide or wide to long data
library(listr) ###to work on a list of vector, files or..
library(sqldf) ##sql
library(lubridate) ###date time
library(stringr) ###to work on patterns, charaters


options(tinytex.verbose = TRUE)

bq_auth()
dictionary <- rio::import("https://episphere.github.io/conceptGithubActions/aggregate.json",format = "json")
dd <- dplyr::bind_rows(dictionary,.id="CID")
dd <-rbindlist(dictionary,fill=TRUE,use.names=TRUE,idcol="CID")
dd$`Variable Label`[is.na(dd$`Variable Label`)] <- replace_na(dd$'Variable Name')

dd <- as.data.frame.matrix(do.call("rbind",dictionary)) 
dd$CID <- rownames(dd)
#https://shaivyakodan.medium.com/7-useful-r-packages-for-analysis-7f60d28dca98
devtools::install_github("tidyverse/reprex")

project <- "nih-nci-dceg-connect-prod-6d04"
billing <- "nih-nci-dceg-connect-prod-6d04" ##project and billing should be consistent
##517311251 Date/time Status of Completion of Background and Overall Health                         SrvBOH_TmComplete_v1r0
##949302066 Flag for Baseline Module Background and Overall Health                        SrvBOH_BaseStatus_v1r0
recr_M1 <- bq_project_query(project, query="SELECT token,Connect_ID, d_821247024, d_914594314,  d_827220437,d_512820379,
                            d_949302066 , d_517311251  FROM  `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants_JP` WHERE  d_821247024='197316935'")
recr_m1 <- bq_table_download(recr_M1,bigint = "integer64")
cnames <- names(recr_m1)
# Check that it doesn't match any non-number
numbers_only <- function(x) !grepl("\\D", x)
# to check variables in recr_noinact_wl1
for (i in 1: length(cnames)){
  varname <- cnames[i]
  var<-pull(recr_m1,varname)
  recr_m1[,cnames[i]] <- ifelse(numbers_only(var), as.numeric(as.character(var)), var)
}

sql_M1_1 <- bq_project_query(project, query="SELECT * FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.module1_v1_JP` where Connect_ID is not null")
sql_M1_2 <- bq_project_query(project, query="SELECT * FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.module1_v2_JP` where Connect_ID is not null")


M1_V1 <- bq_table_download(sql_M1_1,bigint = "integer64") #1436 #1436 vars: 1507 01112023 
M1_V2 <- bq_table_download(sql_M1_2,bigint = "integer64") #2333 #3033 01112023 var:1531 #6339 obs 1893 vars 05022023

mod1_v1 <- M1_V1
cnames <- names(M1_V1)
###to check variables and convert to numeric
for (i in 1: length(cnames)){
  varname <- cnames[i]
  var<-pull(mod1_v1,varname)
  mod1_v1[,cnames[i]] <- ifelse(numbers_only(var), as.numeric(as.character(var)), var)
}
mod1_v2 <- M1_V2
cnames <- names(M1_V2)
###to check variables and convert to numeric
for (i in 1: length(cnames)){
  varname <- cnames[i]
  var<-pull(mod1_v2,varname)
  mod1_v2[,cnames[i]] <- ifelse(numbers_only(var), as.numeric(as.character(var)), var)
}

M1_V1.var <- colnames(M1_V1)
M1_V2.var <- colnames(M1_V2)
var.matched <- M1_V1.var[which(M1_V1.var %in% M1_V2.var)]
length(var.matched)  #1275 #1278 vars 01112023 #1348 vars 05022023

V1_only_vars <- colnames(M1_V1)[colnames(M1_V1) %nin% var.matched] #232 #229 01112023 #159 05022023
V2_only_vars <- colnames(M1_V2)[colnames(M1_V2) %nin% var.matched] #253 #253 01112023 #545 05022023

length(M1_V1$Connect_ID[M1_V1$Connect_ID %in% M1_V2$Connect_ID])
#[1] 59 with the completion of two versions of Module1 
#[1] 62 with completing both versions of M1 ###double checked 03/28/2023
#68 double checked 05/02/2023

common.IDs <- M1_V1$Connect_ID[M1_V1$Connect_ID %in% M1_V2$Connect_ID]
M1_V1_common <- mod1_v1[,var.matched]

M1_V2_common <- mod1_v2[,var.matched]
M1_V1_common$version <- 1
M1_V2_common$version <- 2

##to check the completion of M1 among these duplicates
partM1_dups <- recr_m1[which(recr_m1$Connect_ID %in% common.IDs),]
table(partM1_dups$d_949302066)

M1_common  <- rbind(M1_V1_common, M1_V2_common) #including 136 duplicates (version 1 and version 2) from 68 participants 05022023
#M1_response <- matrix(data=NA, nrow=118, ncol=967)

m1_v1_only <- mod1_v1[,c("Connect_ID", V1_only_vars)] #230 vars 03282023 #160 vars 05/02/2023
m1_v2_only <- mod1_v2[,c("Connect_ID", V2_only_vars)] #255 vars 03282023 #546 vars 05/02/2023
m1_v1_only$version <- 1
m1_v2_only$version <- 2
#for (i in 1:length)
##to check the completion in each version
length(recr_m1$Connect_ID[which(recr_m1$Connect_ID %in% m1_v1_only$Connect_ID & recr_m1$d_949302066 ==231311385)]) #1364 03282023 # 1370 05022023
length(recr_m1$Connect_ID[which(recr_m1$Connect_ID %in% m1_v2_only$Connect_ID & recr_m1$d_949302066 ==231311385)]) #4870 03282023 # 5731 05022023

#library(janitor)

m1_common <- rbind(M1_V1_common,M1_V2_common)
m1_common_v1 <- base::merge(m1_common, m1_v1_only, by=c("Connect_ID","version"),all.x=TRUE)
m1_combined_v1v2 <- base::merge(m1_common_v1,m1_v2_only,by=c("Connect_ID","version"),all.x=TRUE)
m1_complete <- m1_combined_v1v2[which(m1_combined_v1v2$Connect_ID %in% recr_m1$Connect_ID[which(recr_m1$d_949302066 ==231311385 )]),] #7289 including duplicates 05022023

m1_complete <- m1_complete %>% arrange(desc(version)) 


m1_complete_nodup <- m1_complete[!duplicated(m1_complete$Connect_ID),] 
table(m1_complete_nodup$version)



parts <- "SELECT Connect_ID, token, D_512820379, D_471593703, state_d_934298480, D_230663853,
D_335767902, D_982402227, D_919254129, D_699625233, D_564964481, D_795827569, D_544150384,
D_371067537, D_430551721, D_821247024, D_914594314,  state_d_725929722, 
D_949302066 , D_517311251, D_205553981, D_117249500, d_430551721, d_517311251, d_544150384, d_564964481, d_117249500 FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants_JP` 
where Connect_ID IS NOT NULL"
parts_table <- bq_project_query(project, parts)
parts_data <- bq_table_download(parts_table, bigint = "integer64")

parts_data$Connect_ID <- as.numeric(parts_data$Connect_ID) ###need to convert type- m1... is double and parts is character

merged= left_join(m1_complete_nodup, parts_data, by="Connect_ID") 
dim(merged)

#Has a recruitment flag
merged <- merged %>% filter(D_512820379==486306141 | D_512820379==854703046)


#User consent submitted
merged <- merged %>% filter(D_919254129==353358909)


#User profile submitted
merged <- merged %>% filter(D_699625233==353358909)


#Verified by site
cat("Verified by site:", sum(merged$D_821247024==197316935, na.rm=TRUE))


#Mod1 completed
module1 <- merged %>% filter(D_949302066==231311385)
cat("Module 1 completed:", dim(module1)[[1]])


data_tib_m1 <- as_tibble(module1)
#dim(module1)

knitr::opts_chunk$set(comment = NA)
## Multi-racial
all_races <- data_tib_m1[ , c("Connect_ID", "D_384191091_D_384191091_D_583826374", "D_384191091_D_384191091_D_636411467", "D_384191091_D_384191091_D_458435048",
                              "D_384191091_D_384191091_D_706998638", "D_384191091_D_384191091_D_973565052", "D_384191091_D_384191091_D_586825330",
                              "D_384191091_D_384191091_D_412790539", "D_384191091_D_384191091_D_807835037", "D_384191091_D_747350323", "D_384191091_D_384191091_D_746038746")] 
print("Creating race variables")
multi_race=0    
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



## RACE
print("creating multi race vars")
which_race= data_tib_m1 %>%  mutate(race= case_when(multi_racial==1 ~ "Multi-Racial",
                                                    D_384191091_D_384191091_D_583826374==1 ~ "American Indian or Native American",
                                                    D_384191091_D_384191091_D_636411467==1 ~ "Asian/Asian American",
                                                    D_384191091_D_384191091_D_458435048==1 ~ "Black, African American, or African",
                                                    D_384191091_D_384191091_D_706998638==1 ~ "Hispanic, Latino, or Spanish",
                                                    D_384191091_D_384191091_D_973565052==1 ~ "Middle Eastern or North African",
                                                    D_384191091_D_384191091_D_586825330==1 ~ "Hawaiian or Pacific Islander",
                                                    D_384191091_D_384191091_D_412790539==1 ~ "White",
                                                    (D_384191091_D_384191091_D_807835037==1 | !is.na(D_384191091_D_747350323)) ~ "Other",
                                                    D_384191091_D_384191091_D_746038746==1 ~ "Prefer Not to Answer",
                                                    TRUE  ~ "Skipped this question "))

dt_all_races_summary <- which_race  %>% dplyr::group_by(race) %>% dplyr::summarize(n=n(), percentage=100*n/nrow(.)) %>% dplyr::ungroup()  %>%  dplyr::select(race, n, percentage)
print("all_races_summary table made")
'dt_all_races_summary %>%  gt::gt(rowname_col = "row_lab") %>%  
  fmt_number(columns = "percentage", decimals = 2) %>% 
  tab_header(title = md("Race/Ethnicity of Participants Who Completed BOH Section of First Survey")) %>% 
  cols_label(n= md("**Number of Participants**"), race = md("**Answer**"), percentage = md("**Percentage of Participants**")) %>% 
  # summary_rows( groups=F,
  #   column = c(n,percentage),
  #   fns= list(
  #     Sum= ~sum(.)
  #   ), decimals=0) %>%
  grand_summary_rows(columns=c(n, percentage),fns = ~sum(.,na.rm = T))|>
  tab_options(
    stub.font.weight = "bold"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = race,
    )
  ) #%>% opt_row_striping()
'

library(RColorBrewer)
library(ggrepel)
#display.brewer.all(colorblindFriendly = TRUE) 
mycolors <- c("#053061","#999999","#F16913","#FD8D3C","#FFD92F","#0072B2","#009E73","grey42","plum3","darkorchid4", "green4")
names(mycolors) <- levels(dt_all_races_summary$race)
##this is the best plot I can make

dt_all_races_summary$pct <- paste0(round(as.numeric((dt_all_races_summary$percentage)), digits=2), "%") 
#saveRDS(dt_all_races_summary,"/Users/sansalerj/Desktop/rshiny_app/race_summary_data.rds")
readRDS(race_summary_data.rds)
print("creating race_m1plot")
race_M1plot <- dt_all_races_summary %>%
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos),
         percentage = n/sum(n)) %>% 
  ggplot(aes(x = "", y = n, fill = fct_inorder(race))) + 
  scale_fill_manual(values = mycolors,name = "Race Ethnicity") +
  scale_colour_manual(values= mycolors) +
  labs(x = "", y = "", title = "Race/Ethnicity of Participants Who Completed BOH Section of First Survey",
       fill = "race_ethnic") +   
  geom_col(width = 3.5, color = 1) +
  geom_label_repel(aes(y = pos,
                       label = pct, 
                       fill = race),
                   size = 3,color="white",
                   nudge_x = 3,
                   show.legend = FALSE) +
  labs(  fill = "Subtype" ) +
  coord_polar(theta = "y") +   theme_void() 

print("plot generated, displaying now")
#return(race_M1plot)   #if pie chart is needed again
print(race_M1plot)






