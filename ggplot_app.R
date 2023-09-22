## app.R ##
library(shinydashboard)
library(bigrquery)
library(glue)

library(bigrquery)
library(glue)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(tidyr)
library(magrittr)
library(plyr)
library(lubridate)
library(purrr)
library(reshape2)
library(forcats)
library(ggrepel)
library(stringr)


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                box(plotOutput("plot2", height = 250)),
                box(plotOutput("plot3", height = 250)),
                box(plotOutput("plot4", height = 250)),
                box(plotOutput("plot5", height = 450)),
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)

server <- function(input, output) {
#plot1
{  #COVID vaccination status
    project <- 'nih-nci-dceg-connect-dev'
    dataset <- 'FlatConnect'
    table   <- 'bioSurvey_v1_JP'
    sql     <- glue('SELECT D_890156588 ',
                    'FROM `{project}.{dataset}.{table}` ',
                    'WHERE Connect_ID IS NOT NULL ',
                    'LIMIT 100')
    
    # Authenticate to BigQuery
    bigrquery::bq_auth()
    2
    
    # Download data
    tb      <- bigrquery::bq_project_query(project, query=sql)
    data    <- bigrquery::bq_table_download(tb, 
                                            bigint="integer64", 
                                            page_size=1000)
    
    vax_status <- data.frame(vax_status = as.numeric(data$D_890156588))
    output$plot1 <- renderPlot({ hist(vax_status$vax_status, main = c("Histogram of COVID Vaccination Status"),
                                      xlab = "Yes/No Vaccine Status")})    
  }
#plot2
{
  project <- 'nih-nci-dceg-connect-dev'
  dataset <- 'FlatConnect'
  table   <- 'clinicalBioSurvey_v1_JP'
  sql     <- glue('SELECT D_860011428 ',
                  'FROM `{project}.{dataset}.{table}` ',
                  'WHERE Connect_ID IS NOT NULL ',
                  'LIMIT 100')
  
  # Authenticate to BigQuery
  bigrquery::bq_auth()
  2
  
  # Download data
  tb      <- bigrquery::bq_project_query(project, query=sql)
  data    <- bigrquery::bq_table_download(tb, 
                                          bigint="integer64", 
                                          page_size=1000)
  num_covid_inf <- data.frame(num_covid_inf=as.integer(data$D_860011428))
  output$plot2 <- renderPlot({ hist(num_covid_inf$num_covid_inf, main = c("Histogram of COVID Infections"),
                                    xlab = "Number of COVID case per Individual")})
}
#plot3
{  #notification data
  project <- 'nih-nci-dceg-connect-stg-5519'
  dataset <- 'FlatConnect'
  table   <- 'notifications_JP'
  sql     <- glue('SELECT notification_time, attempt, notificationType ',
                  'FROM `{project}.{dataset}.{table}` ',
                  'WHERE token IS NOT NULL ',
                  'LIMIT 100')
  
  # Authenticate to BigQuery
  bigrquery::bq_auth()
  2
  
  # Download data
  tb      <- bigrquery::bq_project_query(project, query=sql)
  data    <- bigrquery::bq_table_download(tb, 
                                          bigint="integer64", 
                                          page_size=1000)
  notifications <- data.frame(notification_time=as.Date(data$notification_time), 
                              attempt = as.numeric(substr(data$attempt,1,1)), 
                              notification_type = data$notificationType)
  br <- unique(notifications$notification_time)
  
  output$plot3 <- renderPlot({ hist(notifications$notification_time, main = c("Histogram of Notification Times"),
                                    labels = FALSE, breaks = length(br), xlab = "Dates")})  
}
#plot4
{  output$plot4 <- renderPlot({ hist(notifications$attempt, main = c("Histogram of Notification Attempts"),
                                    labels = FALSE, breaks = c(0,1,2,3), xlab = "Attempt Number")})  
}  
#plot5
{  
  project <- 'nih-nci-dceg-connect-prod-6d04'
  dataset <- 'FlatConnect'
  table   <- 'participants_JP'
  sql     <- glue('SELECT * ',
                  'FROM `{project}.{dataset}.{table}` ','', paste0("WHERE d_821247024 = '197316935' ") , 'LIMIT 10000')
  
  # Authenticate to BigQuery
  bigrquery::bq_auth()
  2
  
  # Download data
  tb      <- bigrquery::bq_project_query(project, query=sql)
  data    <- bigrquery::bq_table_download(tb, 
                                         bigint="integer64")
#data cleaning and preparation
{  #creating variables like race, sex, age from data
  d2 <- data %>% mutate(race = case_when(state_d_684926335 == 635279662 | state_d_849518448 == 768826601 | state_d_119643471 == 635279662 ~ "White" ,#768826601
                                         state_d_684926335 %in% c(232334767,401335456) | state_d_849518448 == 181769837 |
                                           state_d_119643471 == 232334767| state_d_119643471  ==211228524|state_d_119643471 ==308427446| state_d_119643471  ==432722256| state_d_119643471  ==232663805| state_d_119643471  ==785578696| state_d_119643471  ==200929978| state_d_119643471  ==490725843| state_d_119643471  == 965998904 ~ "Other", #181769837
                                         state_d_684926335 == 178420302  | state_d_849518448 ==178420302 | state_d_119643471 == 986445321| state_d_119643471  == 746038746| state_d_119643471  == 178420302 | (is.na(state_d_119643471) & d_827220437== 657167265) ~ "Unknown"),
                        
                        sex = case_when(state_d_706256705 == 536341288 | state_d_435027713 == 536341288 ~ "Female",
                                        state_d_706256705 == 654207589 | state_d_435027713 == 654207589 ~ "Male",
                                        state_d_706256705 == 830573274 ~ "Intersex or Other",
                                        state_d_706256705 %in% c(178420302,NA) | state_d_435027713 %in% c(178420302,NA) ~ "Unknown"),
                        age = case_when(state_d_934298480 == 124276120 ~ "40-45",
                                        state_d_934298480 == 450985724 ~ "46-50",
                                        state_d_934298480 == 363147933 ~ "51-55",
                                        state_d_934298480 == 636706443 ~ "56-60",
                                        state_d_934298480 == 771230670 ~ "61-65"),
                        recruit_type = case_when(d_512820379 == 486306141 ~ "Active",
                                                 d_512820379 == 854703046 ~ "Passive",
                                                 d_512820379 == 180583933 & d_821247024 == 197316935 ~ "Not Active But Verified"),
                        verified_type = case_when(d_821247024 == 197316935 & state_d_444699761	== 426360242 & state_d_953614051 == 734437214 ~ "Automated Verification", #RcrtV_Automated_v1r0
                                                  d_821247024 == 197316935 &  state_d_953614051	== 426360242 & (state_d_188797763 %in% c(104430631,NA)) ~ "Manual Verification",#	RcrtV_Mannual_v1r0 without outreach
                                                  d_821247024 == 197316935 & state_d_953614051	== 426360242 & state_d_188797763 == 353358909 ~ "Manual Verification and outreach", #	RcrtV_Mannual_v1r0
                        ),
                        active_camptype = case_when(state_d_667474224 == 926338735 ~ "Random",
                                                    state_d_667474224 == 348281054 ~ "Screening appointment",
                                                    state_d_667474224 == 324692899 ~ "Non-screening appointment",
                                                    state_d_667474224 == 351257378 ~ "Demographic Group",
                                                    state_d_667474224 == 647148178 ~ "Aging out of study",
                                                    state_d_667474224 == 834544960 ~ "Geographic group",
                                                    state_d_667474224 == 682916147 ~ "Post-screening appointment",
                                                    state_d_667474224 == 153365143 ~ "Technology adapters",
                                                    state_d_667474224 == 663706936 ~ "Low-income/health professional shortage areas",
                                                    state_d_667474224 == 181769837 ~ "Other",
                                                    state_d_667474224 == 398561594 ~ "None of these apply",
                                                    is.na(state_d_667474224)  ~ "NA/Unknown"),
                        
                        recrt_steps_v1r0 = case_when((d_230663853 == 104430631) ~"Never Signed-In",
                                                     (d_230663853 == 353358909 & d_919254129 == 104430631) ~ "Signed-in, No Consent",
                                                     (d_230663853 == 353358909 & d_919254129 == 353358909 & d_699625233 == 104430631) ~ "Consented, No Profile",
                                                     (d_230663853 == 353358909 & d_919254129 == 353358909 & d_699625233 == 353358909 & d_821247024 == 875007964) ~ "Profile, Verification Incomplete",
                                                     (d_230663853 == 353358909 & d_919254129 == 353358909 & d_699625233 == 353358909 & d_821247024 %in% c(197316935,219863910,922622075)) ~ "Verification Complete"),
                        Rcrt_Verified_v1r0 = ifelse(is.na(d_821247024) | d_821247024 != 197316935, 104430631, 353358909),
                        verified = case_when(d_821247024 == 875007964 ~ "Not yet verified",
                                             d_821247024 == 197316935  ~ "Verified",
                                             d_821247024 ==  219863910 ~ "Cannot be verified",
                                             d_821247024 ==  922622075 ~ "Duplicate",
                                             d_821247024 ==  160161595 ~ "Outreach timed out"),                                             
                        Rcrt_completion_v1r0 = case_when((d_821247024 == 875007964) ~ 104430631,
                                                         (d_821247024 %in% c(197316935,219863910,922622075)) ~ 353358909),
                        verified = case_when(d_821247024 == 875007964 ~ "Not yet verified",
                                             d_821247024 == 197316935  ~ "Verified",
                                             d_821247024 ==  219863910 ~ "Cannot be verified",
                                             d_821247024 ==  922622075 ~ "Duplicate",
                                             d_821247024 ==  160161595 ~ "Outreach timed out"),
                        
                        Preconsent = case_when(state_d_158291096 == 104430631 ~ "No", #d_158291096	as	RcrtSI_OptOut_v1r0,
                                               state_d_158291096 == 353358909 ~ "Yes"),
                        SSN = case_when(d_311580100 == 353358909 ~ "Full SSN Given",
                                        d_311580100 == 104430631 & d_914639140 == 353358909 ~ "Partial SSN Given",
                                        d_311580100 == 104430631 & d_914639140 == 104430631 ~ "No SSN Given"),
                        CSSigned = case_when(d_230663853 ==353358909 & d_919254129 == 104430631 ~ "Signed-In, no Consent",
                                             d_230663853 == 104430631 & d_919254129 == 104430631 ~ "Never Signed-In",
                                             d_230663853 ==353358909 & d_919254129 == 353358909 ~"Signed-In, Consent Submitted"),
                        biocol_type = case_when(d_878865966 ==353358909 & d_167958071 == 353358909 & d_684635302 == 353358909 ~ "All 3 sample Donations",
                                                d_878865966 ==353358909 & d_167958071 == 353358909 & d_684635302 == 104430631 ~ "Blood & Urine",
                                                d_878865966 ==353358909 & d_167958071 == 104430631 & d_684635302 == 353358909 ~ "Blood & Mouthwash",
                                                d_878865966 ==104430631 & d_167958071 == 353358909 & d_684635302 == 353358909 ~ "Mouthwash & Urine",
                                                d_878865966 ==353358909 & d_167958071 == 104430631 & d_684635302 == 104430631 ~ "Blood Only",
                                                d_878865966 ==104430631 & d_167958071 == 353358909 & d_684635302 == 104430631 ~ "Urine Only",
                                                d_878865966 ==104430631 & d_167958071 == 104430631 & d_684635302 == 353358909 ~ "Mouthwash Only",
                                                d_878865966 ==104430631 & d_167958071 == 104430631 & d_684635302 == 104430631 ~ "No Samples"),
                        Msrv_complt=case_when(d_100767870==353358909 ~ "All 4 Survey Sections",
                                              d_100767870==104430631 & d_949302066 == 231311385 & d_536735468 != 231311385 & d_976570371 != 231311385 & d_663265240 != 231311385 ~ "BOH only",
                                              d_100767870==104430631 & d_949302066 == 231311385 & d_536735468 == 231311385 & d_976570371 != 231311385 & d_663265240 != 231311385 ~ "BOH and MRE",
                                              d_100767870==104430631 & d_949302066 == 231311385 & d_536735468 != 231311385 & d_976570371 == 231311385 & d_663265240 != 231311385 ~ "BOH and SAS",
                                              d_100767870==104430631 & d_949302066 == 231311385 & d_536735468 != 231311385 & d_976570371 != 231311385 & d_663265240 == 231311385 ~ "BOH and LAW",
                                              d_100767870==104430631 & d_949302066 == 231311385 & d_536735468 == 231311385 & d_976570371 == 231311385 & d_663265240 != 231311385 ~ "BOH, MRE, and SAS",
                                              d_100767870==104430631 & d_949302066 == 231311385 & d_536735468 == 231311385 & d_976570371 != 231311385 & d_663265240 == 231311385 ~ "BOH, MRE, and LAW",
                                              d_100767870==104430631 & d_949302066 == 231311385 & d_536735468 != 231311385 & d_976570371 == 231311385 & d_663265240 == 231311385 ~ "BOH, SAS, and LAW",
                                              d_100767870==104430631 & d_949302066 != 231311385 & d_536735468 != 231311385 & d_976570371 != 231311385 & d_663265240 != 231311385 ~ "No Survey Sections"))
  
  
  #these are the variables that will be used in the calculation of verified participant activities
  #No Samples	Blood only	Urine only	Mouthwash only	Blood and Urine	Blood and Mouthwash	Urine and Mouthwash	All 3 Sample Donations
  d2$biocol_type <- factor(d2$biocol_type, levels=c("No Samples","Blood Only","Urine Only","Mouthwash Only","Blood & Urine","Blood & Mouthwash","Mouthwash & Urine","All 3 sample Donations"))
  
  d2$verified <- factor(d2$verified,exclude=NULL, levels=c("Not yet verified","Verified","Cannot be verified","Duplicate","Outreach timed out"))
  
  #creating variables for biospecimen donation, module completion and survey completion
  d2 <- d2 %>% 
    mutate(biospeDonation = ifelse(d_878865966 %in% c(104430631,NA)  & d_167958071 %in% c(104430631,NA) & d_684635302 %in% c(104430631,NA),"No Sample Donations",
                                   ifelse(d_878865966 == 353358909 & d_167958071 == 353358909 & d_684635302 == 353358909,"Completed All 3 Sample Donations", "Completed Some but Not All 3 Sample Donations")),
           biospeComplete = case_when(biospeDonation =="Completed All 3 Sample Donations" | (d_173836415_d_266600170_d_592099155==664882224 & d_878865966 == 353358909 & d_167958071 == 353358909)  ~ 1,
                                      biospeDonation == "No Sample Donations" | ( biospeDonation == "Completed Some but Not All 3 Sample Donations" &  d_173836415_d_266600170_d_592099155 %in% c(534621077,NA)) | d_173836415_d_266600170_d_592099155==664882224 & d_878865966 %in% c(104430631, NA) | d_167958071 %in% c(104430631, NA) ~ 0),
           Module.complete = ifelse(d_100767870==353358909,"Complete","NotComplete"),
           surveybio_complete = case_when(d_100767870==353358909 & biospeComplete==1 ~ "Yes",
                                          d_100767870 == 104430631  | biospeComplete %in% c(0,NA) ~ "No" ),
           surveybld_complete = case_when(d_100767870==353358909 & d_878865966 == 353358909 ~ "Yes",
                                          d_100767870 == 104430631  | d_878865966 %in% c(104430631,NA) ~ "No" ))
  
  d2$surveybio_complete <- factor(d2$surveybio_complete,levels=c("Yes","No"),order=TRUE)
  
  
  
  
  
  #variable selection
  #modules is type of baseline module activity completed-- e.g. baseline survey completed
  modules <- c("d_100767870","d_949302066","d_536735468","d_663265240","d_976570371","d_517311251","d_832139544","d_264644252","d_770257102")
  #bio.col is the type of bio activity completed-- e.g. mouthwash 
  #also includes dates of collection
  bio.col <- c("d_684635302","d_878865966", "d_167958071", "d_173836415_d_266600170_d_915179629", "d_173836415_d_266600170_d_718172863",
               "d_173836415_d_266600170_d_592099155", "d_173836415_d_266600170_d_561681068", "d_173836415_d_266600170_d_847159717",
               "d_173836415_d_266600170_d_448660695", "d_173836415_d_266600170_d_139245758", "d_173836415_d_266600170_d_541311218",
               "d_173836415_d_266600170_d_224596428", "d_173836415_d_266600170_d_740582332", "d_173836415_d_266600170_d_982213346",
               "d_173836415_d_266600170_d_398645039", "d_173836415_d_266600170_d_822274939")
  
  ###clinical collection time:
  #date when actual bio collection was completed
  clc.bldtm <- c("d_173836415_d_266600170_d_769615780","d_173836415_d_266600170_d_822274939",
                 "d_173836415_d_266600170_d_398645039","d_173836415_d_266600170_d_982213346",
                 "d_173836415_d_266600170_d_740582332")
  clc.urinetm <- c("d_173836415_d_266600170_d_139245758","d_173836415_d_266600170_d_224596428",
                   "d_173836415_d_266600170_d_541311218","d_173836415_d_266600170_d_939818935",
                   "d_173836415_d_266600170_d_740582332")
  #verification status, recruitment type 
  var.list <- c("Connect_ID","d_821247024","d_914594314","d_512820379",
                "state_d_158291096","d_471593703","d_827220437",
                "d_130371375_d_266600170_d_787567527",
                "d_130371375_d_266600170_d_731498909", "surveybio_complete", "surveybld_complete", "biocol_type", bio.col,modules)
  
  
  #subsetting by recruitment type
  #d_512820379 = recruitment type, where 486306141 indicates an individual is being actively recruited, we dont want active recruits
  #d_821247024 = match verification after creation of user profile, where 922622075 indicates a duplicate recruit
  veri_resp <- d2[which(d2$d_512820379 != 486306141 | d2$d_821247024 != 922622075 ),var.list] %>%
    mutate(recruit_year= year(ymd_hms(d_471593703)),
           recruit_month = month(ymd_hms(d_471593703)),
           verified_year= year(ymd_hms(d_914594314)),
           verified_month = month(ymd_hms(d_914594314)),
           elgible.time = ymd_hms(d_130371375_d_266600170_d_787567527))
  
  veri_resp$verified_time <- ymd_hms(veri_resp$d_914594314)
  veri_resp$recru_time <- ymd_hms(veri_resp$d_471593703)
  veri_resp$recrstart.date <- as_date(min(veri_resp$recru_time,na.rm=TRUE))
  veri_resp$verified.week <- ceiling(as.numeric(difftime(as_date(veri_resp$verified_time),as_date(veri_resp$recrstart.date),units="days"))/7)
  veri_resp$verified.week.date <- (veri_resp$recrstart.date) + dweeks(veri_resp$verified.week)
  
  #unsure how adding a time variable will affect the d2 table, going to create a new one and see if theres any difference
  d3 <- d2[which(d2$d_512820379 != 486306141 | d2$d_821247024 != 922622075 ),var.list] %>%
    mutate(recruit_year= year(ymd_hms(d_471593703)),
           recruit_month = month(ymd_hms(d_471593703)),
           verified_year= year(ymd_hms(d_914594314)),
           verified_month = month(ymd_hms(d_914594314)),
           elgible.time = ymd_hms(d_130371375_d_266600170_d_787567527))
  
  d3$verified_time <- ymd_hms(d3$d_914594314)
  d3$recru_time <- ymd_hms(d3$d_471593703)
  d3$recrstart.date <- as_date(min(d3$recru_time,na.rm=TRUE))
  d3$verified.week <- ceiling(as.numeric(difftime(as_date(d3$verified_time),as_date(d3$recrstart.date),units="days"))/7)
  d3$verified.week.date <- (d3$recrstart.date) + dweeks(d3$verified.week)
  
  
  
  
  
  
  #d_821247024 = indicator of match verification, where 197316935 is "verified" 
  #d_827220437 = recruitment site
  recruit.site <- veri_resp[which(veri_resp$d_821247024 == 197316935 & veri_resp$d_827220437 >0),] %>% 
    group_by(d_827220437) %>%
    dplyr::summarise(Verified=n())
  
  #aggregation of total recruits by week, we do not need to specify the site
  recruit.time <- veri_resp[which(veri_resp$d_821247024 == 197316935 & veri_resp$d_827220437 >0),] %>% 
    group_by(verified.week.date) %>%
    dplyr::summarise(Verified=n())
  
  #aggregation of total blood sample completion by site
  #d_878865966 = baseline blood sample collected, 353358909 = yes 
  #d_821247024 = indicator of match verification after creation of user profile, 197316935 = verified
  #d_827220437 = site
  blood.site <- d2[which(d2$d_878865966==353358909 & d2$d_821247024 == 197316935 & d2$d_827220437 >0),] %>%
    select(d_827220437,d_821247024, d_878865966) %>%
    group_by(d_827220437) %>%
    dplyr::summarise(Blood=n())
  
  blood.time <- d3[which(d3$d_878865966==353358909 & d3$d_821247024 == 197316935 & d3$d_827220437 >0),] %>%
    select(d_827220437,d_821247024, d_878865966, verified.week.date) %>%
    group_by(verified.week.date) %>%
    dplyr::summarise(Blood=n())
  
  
  
  
  #aggregate of survey completion by site
  survey.site <- filter(d2,d_100767870==353358909 & d_821247024 == 197316935 &d_827220437 >0) %>% 
    group_by(d_827220437) %>%
    dplyr::summarize(Surveys=n())
  #aggregate of survey completion by time
  survey.time <- filter(d3,d_100767870==353358909 & d_821247024 == 197316935 &d_827220437 >0) %>% 
    group_by(verified.week.date) %>%
    dplyr::summarize(Surveys=n())
  
  
  
  #aggregate of surveybio_complete completion by site
  complete.site0 <- filter(d2,surveybio_complete=="Yes" & d_821247024 == 197316935 &d_827220437 >0) %>%
    group_by(d_827220437) %>%
    dplyr::summarize(`Initial Srv+Samples`=n())
  #aggregate of surveybio_complete completion by time
  complete.time0 <- filter(d3,surveybio_complete=="Yes" & d_821247024 == 197316935 &d_827220437 >0) %>%
    group_by(verified.week.date) %>%
    dplyr::summarize(`Initial Srv+Samples`=n())
  
  #aggregate of surveybld_complete completion by site
  complete.site <- filter(d2,surveybld_complete=="Yes" & d_821247024 == 197316935 &d_827220437 >0) %>%
    group_by(d_827220437) %>%
    dplyr::summarize(`Survey + Blood`=n())
  #aggregate of surveybld_complete by verified week
  complete.time <- filter(d3,surveybld_complete=="Yes" & d_821247024 == 197316935 &d_827220437 >0) %>%
    group_by(verified.week.date) %>%
    dplyr::summarize(`Survey + Blood`=n())
  
  recruit.site.time <- list(recruit.time,blood.time,survey.time,complete.time) %>% reduce(full_join, by="verified.week.date")
  #summing all the expected values by site, that will be the expected value for each week
  recruit.site.time <- recruit.site.time %>% mutate(Expected = (1900+1300+790+1900+1600+2247+760+1015+2500),
                                                    `Blood Only` = Blood - `Survey + Blood`,
                                                    `Survey Only` = Surveys - `Survey + Blood`,
                                                    `Verified, no Activities` = Verified-(Surveys + Blood - `Survey + Blood`),
                                                    Expected.toGo = Expected - Verified)
  
  
  recruit.site.time$Expected.toGo <- as.integer(recruit.site.time$Expected.toGo)
  recruit.site.time$Expected <- as.integer(recruit.site.time$Expected)
  verifed_expected_n <- melt(recruit.site.time[,c("Expected.toGo","Verified, no Activities","verified.week.date","Survey Only","Blood Only","Survey + Blood")],id.vars="verified.week.date", 
                             measure.vars=c("Expected.toGo","Verified, no Activities","Survey Only","Blood Only","Survey + Blood"), variable.name="Expected Verified",
                             value.name="Expected_Verified_n")
  
  verifed_expected_n <- verifed_expected_n  %>% group_by(verified.week.date) %>%
    arrange(verified.week.date,fct_rev(`Expected Verified`))%>%
    mutate(cum.count=cumsum(Expected_Verified_n),label.count=cumsum(Expected_Verified_n)-0.1*Expected_Verified_n)
  
  verifed_expected_n$`Expected Verified` <- gsub(".toGo", "",verifed_expected_n$`Expected Verified`)
  verifed_expected_n$`Expected Verified` <- factor(verifed_expected_n$`Expected Verified`, 
                                                   levels=c("Survey + Blood","Blood Only","Survey Only","Verified, no Activities","Expected"))
  
  verifed_expected_n <- verifed_expected_n  %>% group_by(verified.week.date) %>% arrange(verified.week.date,`Expected Verified`)%>% 
    mutate(cum.count=cumsum(Expected_Verified_n),                                                                     
           label.count=cumsum(Expected_Verified_n) *1.02)
  
  verifed_expected_n1 <- verifed_expected_n %>% 
    mutate(cum.Expected =ifelse(`Expected Verified` !="Expected", cum.count, NA ),
           verified.n = ifelse(`Expected Verified` !="Expected",Expected_Verified_n, cumsum(Expected_Verified_n)))
  
  currentDate <- Sys.Date()
  set.seed(42)
  
  verifed_expected_n1$type <- verifed_expected_n1$`Expected Verified`
  filtered <- subset(verifed_expected_n1, verifed_expected_n1$type!= "Expected")
  # Visualization
}
    
  output$plot5 <- renderPlot({ ggplot(filtered, aes(x = verified.week.date, y = verified.n)) + 
      geom_line(aes(color = type, linetype = type))+
      scale_y_continuous(name="Individuals, N", breaks=c(100,200,300,400,500,600) ) +
      labs( title=str_wrap(paste("Newly Validated Connect Participants by Validation Date",currentDate, sep=" "),  60),
            x = "Validation Week Date")+
      theme(panel.background = element_blank(),
            legend.title = element_text(size=8),
            legend.position = "bottom",
            axis.line = element_line(linewidth = 0.2),
            axis.text.x = element_text(hjust = 0.5,size = 8, face = "bold"),  plot.title = element_text(hjust = 0.5,size = 12, face = "bold"))})    
}
  
  
  
  
}

shinyApp(ui, server)