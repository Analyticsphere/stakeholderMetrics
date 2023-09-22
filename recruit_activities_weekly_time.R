#going to try and create a time variable in the verified responses data table
#so that we can have weekly sums of respondants
#re-creating jing plot
#recreating plot 3 found here: https://nih.app.box.com/file/1304110870904
#jings code to create this report is here: https://github.com/jeannewu/BiospecimenMetrics_Connect_SAS/blob/main/Consolidated_weekly_Report_03152023.Rmd
#she creates this plot around line 2374

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

#creating variables like race, sex, age from data
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
              "d_130371375_d_266600170_d_731498909", "surveybio_complete", "surveybld_complete",
              "biocol_type", "state_d_934298480", "race", bio.col,modules)
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################
#data subsetting by verified status#
#subsetting only the verified respondents who are not duplicates and are not being actively recruited
#d_512820379 = recruitment type, where 486306141 indicates an individual is being actively recruited, we dont want active recruits
#d_821247024 = match verification after creation of user profile, where 922622075 indicates a duplicate recruit
veri_resp <- d2[which(d2$d_512820379 != 486306141 | d2$d_821247024 != 922622075 ),var.list] %>%
  mutate(recruit_year= year(ymd_hms(d_471593703)),
         recruit_month = month(ymd_hms(d_471593703)),
         verified_year= year(ymd_hms(d_914594314)),
         verified_month = month(ymd_hms(d_914594314)),
         elgible.time = ymd_hms(d_130371375_d_266600170_d_787567527),
         law.time = ymd_hms(d_264644252),
         sas.time = ymd_hms(d_770257102),
         mre.time = ymd_hms(d_832139544),
         boh.time = ymd_hms(d_517311251),
         bldcol.time = case_when(d_878865966 == 353358909 ~ pmin(as.POSIXct(d_173836415_d_266600170_d_561681068),as.POSIXct(d_173836415_d_266600170_d_982213346),as.POSIXct(d_173836415_d_266600170_d_398645039),as.POSIXct(d_173836415_d_266600170_d_822274939),na.rm=TRUE)),
         Module.complete = ifelse(d_100767870==353358909,"Complete","NotComplete") #blood time
  )
#verified respondent times
veri_resp$verified_time <- ymd_hms(veri_resp$d_914594314)
veri_resp$recru_time <- as_date(ymd_hms(veri_resp$d_471593703))
veri_resp$recrstart.date <- as_date(min(veri_resp$recru_time,na.rm=TRUE))
veri_resp$verified.week <- ceiling(as.numeric(difftime(as_date(veri_resp$verified_time),as_date(veri_resp$recrstart.date),units="days"))/7)
veri_resp$verified.week.date <- (veri_resp$recrstart.date) + dweeks(veri_resp$verified.week)
#all of participants w clinical blood collections should have their blood receiving time: d_173836415_d_266600170_d_398645039
veri_resp$blood_collection_time <- case_when(veri_resp$d_878865966 == 353358909 ~ pmin(as.POSIXct(veri_resp$d_173836415_d_266600170_d_561681068),
                                                                                       as.POSIXct(veri_resp$d_173836415_d_266600170_d_982213346),
                                                                                       as.POSIXct(veri_resp$d_173836415_d_266600170_d_398645039),
                                                                                       as.POSIXct(veri_resp$d_173836415_d_266600170_d_822274939),na.rm=TRUE))
veri_resp$blood_collection_time <- as_date(veri_resp$blood_collection_time)
veri_resp$module.tm <- do.call(pmax, c(veri_resp[,c("sas.time","mre.time","law.time","boh.time")],na.rm=TRUE))
veri_resp <- veri_resp%>%
  mutate(module.time  = as_datetime(ifelse(d_100767870 ==353358909,module.tm,ifelse( d_100767870 ==104430631,NA,NA)))) #module completion time
veri_resp$module.time <- as_date(veri_resp$module.time)
veri_resp$blood_and_survey_completion_date <- pmax(veri_resp$module.time, veri_resp$blood_collection_time, na.rm = TRUE)
#age and race
veri_resp$age_bucket <- factor(veri_resp$state_d_934298480)
veri_resp$verified.week.factor <- factor(veri_resp$verified.week.date)






#recruitment site, time and age subsets
#d_821247024 = indicator of match verification, where 197316935 is "verified" 
#d_827220437 = recruitment site
recruit.site <- veri_resp[which(veri_resp$d_821247024 == 197316935 & veri_resp$d_827220437 >0),] %>% 
  group_by(d_827220437) %>%
  dplyr::summarise(Verified=n())

#aggregation of total recruits by week, we do not need to specify the site
recruit.time <- veri_resp[which(veri_resp$d_821247024 == 197316935 & veri_resp$d_827220437 >0),] %>% 
  group_by(recru_time) %>%
  dplyr::summarise(Verified=n())
colnames(recruit.time) <- c("date", "Verified")

#aggregation of total recruits by week and age, we do not need to specify the site
t = data.frame(veri_resp[which(veri_resp$d_821247024 == 197316935 & veri_resp$d_827220437 >0),])
recruit.time.age <- aggregate(t$age_bucket,
                              by = list(t$recrstart.date, t$age_bucket),
                              FUN = length)

#race
t = data.frame(veri_resp[which(veri_resp$d_821247024 == 197316935 & veri_resp$d_827220437 >0),])
recruit.time.race <- aggregate(t$race,
                              by = list(t$recrstart.date, t$race),
                              FUN = length)


#################################


#blood sample by site, week and age
#aggregation of total blood sample completion by site
#d_878865966 = baseline blood sample collected, 353358909 = yes 
#d_821247024 = indicator of match verification after creation of user profile, 197316935 = verified
#d_827220437 = site
blood.site <- veri_resp[which(veri_resp$d_878865966==353358909 & veri_resp$d_821247024 == 197316935 & veri_resp$d_827220437 >0),] %>%
  select(d_827220437,d_821247024, d_878865966) %>%
  group_by(d_827220437) %>%
  dplyr::summarise(Blood=n())

blood.time <- veri_resp[which(veri_resp$d_878865966==353358909 & veri_resp$d_821247024 == 197316935 & veri_resp$d_827220437 >0),] %>%
  select(d_827220437,d_821247024, d_878865966, blood_collection_time) %>%
  group_by(blood_collection_time) %>%
  dplyr::summarise(Blood=n())
colnames(blood.time) <- c("date", "Blood")


#aggregation of total recruits by week and age, we do not need to specify the site
t = data.frame(veri_resp[which(veri_resp$d_878865966==353358909 & veri_resp$d_821247024 == 197316935 & veri_resp$d_827220437 >0),])
blood.time.age <- aggregate(t$age_bucket,
                              by = list(t$blood_collection_time, t$age_bucket),
                              FUN = length)
#aggregation of total recruits by week and race, we do not need to specify the site
t = data.frame(veri_resp[which(veri_resp$d_878865966==353358909 & veri_resp$d_821247024 == 197316935 & veri_resp$d_827220437 >0),])
blood.time.race <- aggregate(t$race,
                            by = list(t$blood_collection_time, t$race),
                            FUN = length)



#################################



#aggregate of survey completion by site
survey.site <- filter(veri_resp,d_100767870==353358909 & d_821247024 == 197316935 &d_827220437 >0) %>% 
  group_by(d_827220437) %>%
  dplyr::summarize(Surveys=n())
#aggregate of survey completion by time
survey.time <- filter(veri_resp,d_100767870==353358909 & d_821247024 == 197316935 &d_827220437 >0) %>% 
  group_by(module.time) %>%
  dplyr::summarize(Surveys=n())
colnames(survey.time) <- c("date", "Surveys")

#aggregation of survey by week and age, we do not need to specify the site
t = data.frame(filter(veri_resp,d_100767870==353358909 & d_821247024 == 197316935 &d_827220437 >0))
survey.time.age <- aggregate(t$age_bucket,
                            by = list(t$module.time, t$age_bucket),
                            FUN = length)
#aggregation of survey by week and age, we do not need to specify the site
t = data.frame(filter(veri_resp,d_100767870==353358909 & d_821247024 == 197316935 &d_827220437 >0))
survey.time.race <- aggregate(t$race,
                             by = list(t$module.time, t$race),
                             FUN = length)

#################################
#################################

#aggregate of surveybld_complete completion by site
complete.site <- filter(veri_resp,surveybld_complete=="Yes" & d_821247024 == 197316935 &d_827220437 >0) %>%
  group_by(d_827220437) %>%
  dplyr::summarize(`Survey + Blood`=n())
#aggregate of surveybld_complete by verified week
complete.time <- filter(veri_resp,surveybld_complete=="Yes" & d_821247024 == 197316935 &d_827220437 >0) %>%
  group_by(blood_and_survey_completion_date) %>%
  dplyr::summarize(`Survey + Blood`=n())
colnames(complete.time) <- c("date", "Survey + Blood")

#aggregation of survery bld completion by week and age, we do not need to specify the site
t = data.frame(filter(veri_resp,surveybld_complete=="Yes" & d_821247024 == 197316935 &d_827220437 >0))
complete.time.age <- aggregate(t$age_bucket,
                                by = list(t$blood_and_survey_completion_date, t$age_bucket),
                                FUN = length)
#aggregation of survery bld completion by week and age, we do not need to specify the site
t = data.frame(filter(veri_resp,surveybld_complete=="Yes" & d_821247024 == 197316935 &d_827220437 >0))
complete.time.race <- aggregate(t$race,
                               by = list(t$blood_and_survey_completion_date, t$race),
                               FUN = length)






recruit.site.time <- list(recruit.time,blood.time,survey.time,complete.time) %>% reduce(full_join, by="date")
recruit.site.time.age <- list(recruit.time.age,blood.time.age,survey.time.age,complete.time.age) %>% reduce(full_join, by=c("Group.1", "Group.2"))
recruit.time.race <- list(recruit.time.race, blood.time.race, survey.time.race, complete.time.race) %>% reduce(full_join, by=c("Group.1", "Group.2"))
colnames(recruit.site.time.age) <- c("verified.week.date", "age", "Verified", "Blood", "Surveys", "Survey + Blood")
colnames(recruit.time.race) <- c("verified.week.date", "race", "Verified", "Blood", "Surveys", "Survey + Blood")




#summing all the expected values by site, that will be the expected value for each week
recruit.site.time <- recruit.site.time %>% mutate(Expected = (1900+1300+790+1900+1600+2247+760+1015+2500),
                                                  `Blood Only` = Blood - `Survey + Blood`,
                                                  `Survey Only` = Surveys - `Survey + Blood`,
                                                  `Verified, no Activities` = Verified-(Surveys + Blood - `Survey + Blood`),
                                                   Expected.toGo = Expected - Verified)

recruit.site.time.age <- recruit.site.time.age %>% mutate(Expected = (1900+1300+790+1900+1600+2247+760+1015+2500),
                                                       `Blood Only` = Blood - `Survey + Blood`,
                                                  `Survey Only` = Surveys - `Survey + Blood`,
                                                  `Verified, no Activities` = Verified-(Surveys + Blood - `Survey + Blood`),
                                                  Expected.toGo = Expected - Verified)

recruit.time.race <- recruit.time.race %>% mutate(Expected = (1900+1300+790+1900+1600+2247+760+1015+2500),
                                                          `Blood Only` = Blood - `Survey + Blood`,
                                                          `Survey Only` = Surveys - `Survey + Blood`,
                                                          `Verified, no Activities` = Verified-(Surveys + Blood - `Survey + Blood`),
                                                          Expected.toGo = Expected - Verified)




recruit.site.time$Expected.toGo <- as.integer(recruit.site.time$Expected.toGo)
recruit.site.time.age$Expected.toGo <- as.integer(recruit.site.time.age$Expected.toGo)
recruit.time.race$Expected.toGo <- as.integer(recruit.time.race$Expected.toGo)


recruit.site.time$Expected <- as.integer(recruit.site.time$Expected)
recruit.site.time.age$Expected <- as.integer(recruit.site.time.age$Expected)
recruit.time.race$Expected <- as.integer(recruit.time.race$Expected)


#convert wide to long
verifed_expected_n <- melt(recruit.site.time[,c("Expected.toGo","Verified, no Activities","date","Survey Only","Blood Only","Survey + Blood")],
                           id.vars="date", 
                           measure.vars=c("Expected.toGo","Verified, no Activities","Survey Only","Blood Only","Survey + Blood"),
                           variable.name="Expected Verified",
                           value.name="Expected_Verified_n")
verifed_expected_age_n <- melt(recruit.site.time.age[,c("Expected.toGo","Verified, no Activities","verified.week.date","Survey Only","Blood Only","Survey + Blood", "age")],
                          id.vars=c("verified.week.date","age"), 
                           measure.vars=c("Expected.toGo","Verified, no Activities","Survey Only","Blood Only","Survey + Blood"),
                          variable.name="Expected Verified",
                           value.name="Expected_Verified_n")
verifed_expected_race_n <- melt(recruit.time.race[,c("Expected.toGo","Verified, no Activities","verified.week.date","Survey Only","Blood Only","Survey + Blood", "race")],
                               id.vars=c("verified.week.date","race"), 
                               measure.vars=c("Expected.toGo","Verified, no Activities","Survey Only","Blood Only","Survey + Blood"),
                               variable.name="Expected Verified",
                               value.name="Expected_Verified_n")


verifed_expected_n <- verifed_expected_n  %>% group_by(date) %>%
  arrange(date,fct_rev(`Expected Verified`))%>%
  mutate(cum.count=cumsum(Expected_Verified_n),label.count=cumsum(Expected_Verified_n)-0.1*Expected_Verified_n)

verifed_expected_age_n <- verifed_expected_age_n  %>% group_by(verified.week.date, age) %>%
  arrange(verified.week.date,fct_rev(`Expected Verified`))%>%
  mutate(cum.count=cumsum(Expected_Verified_n),label.count=cumsum(Expected_Verified_n)-0.1*Expected_Verified_n)

verifed_expected_race_n <- verifed_expected_race_n  %>% group_by(verified.week.date, race) %>%
  arrange(verified.week.date,fct_rev(`Expected Verified`))%>%
  mutate(cum.count=cumsum(Expected_Verified_n),label.count=cumsum(Expected_Verified_n)-0.1*Expected_Verified_n)


verifed_expected_n$`Expected Verified` <- gsub(".toGo", "",verifed_expected_n$`Expected Verified`)
verifed_expected_age_n$`Expected Verified` <- gsub(".toGo", "",verifed_expected_age_n$`Expected Verified`)
verifed_expected_race_n$`Expected Verified` <- gsub(".toGo", "",verifed_expected_race_n$`Expected Verified`)

verifed_expected_n$`Expected Verified` <- factor(verifed_expected_n$`Expected Verified`, 
                                                 levels=c("Survey + Blood","Blood Only","Survey Only","Verified, no Activities","Expected"))
verifed_expected_age_n$`Expected Verified` <- factor(verifed_expected_age_n$`Expected Verified`, 
                                                 levels=c("Survey + Blood","Blood Only","Survey Only","Verified, no Activities","Expected"))
verifed_expected_race_n$`Expected Verified` <- factor(verifed_expected_race_n$`Expected Verified`, 
                                                     levels=c("Survey + Blood","Blood Only","Survey Only","Verified, no Activities","Expected"))

verifed_expected_n <- verifed_expected_n  %>% group_by(date) %>% arrange(date,`Expected Verified`)%>% 
  mutate(cum.count=cumsum(Expected_Verified_n),                                                                     
         label.count=cumsum(Expected_Verified_n) *1.02)

verifed_expected_age_n <- verifed_expected_age_n  %>% group_by(verified.week.date, age) %>% arrange(verified.week.date,`Expected Verified`)%>% 
  mutate(cum.count=cumsum(Expected_Verified_n),                                                                     
         label.count=cumsum(Expected_Verified_n) *1.02)

verifed_expected_race_n <- verifed_expected_race_n  %>% group_by(verified.week.date, race) %>% arrange(verified.week.date,`Expected Verified`)%>% 
  mutate(cum.count=cumsum(Expected_Verified_n),                                                                     
         label.count=cumsum(Expected_Verified_n) *1.02)



verifed_expected_n1 <- verifed_expected_n %>% 
  mutate(cum.Expected =ifelse(`Expected Verified` !="Expected", cum.count, NA ),
         verified.n = ifelse(`Expected Verified` !="Expected",Expected_Verified_n, cumsum(Expected_Verified_n)))
#verifed_expected_n1_age <- verifed_expected_age_n %>% 
#  mutate(cum.Expected =ifelse(`Expected Verified` !="Expected", cum.count, NA ),
#         verified.n = ifelse(`Expected Verified` !="Expected",Expected_Verified_n, cumsum(Expected_Verified_n)))


currentDate <- Sys.Date()
set.seed(42)

verifed_expected_n1$type <- verifed_expected_n1$`Expected Verified`
filtered <- subset(verifed_expected_n1, verifed_expected_n1$type!= "Expected")
# Visualization
ggplot(filtered, aes(x = date, y = verified.n)) + 
  geom_line(aes(color = type, linetype = type))+
 # scale_y_continuous(name="Individuals, N", breaks=c(100,200,300,400,500,600) ) +
  labs( title=str_wrap(paste("Newly Validated Connect Participants by Validation Date",currentDate, sep=" "),  60),
        x = "Activity Completion Week Date")+
  theme(panel.background = element_blank(),
        legend.title = element_text(size=8),
        legend.position = "bottom",
        axis.line = element_line(linewidth = 0.2),
        axis.text.x = element_text(hjust = 0.5,size = 8, face = "bold"),  plot.title = element_text(hjust = 0.5,size = 12, face = "bold"))



#######################################
#verified participants by AGE
#line plot of ages since 2021
'
124276120 = 40-45
450985724 = 46-50
363147933 = 51-55
636706443 = 56-60
771230670 = 61-65
'
age_buckets = subset(verifed_expected_age_n,verifed_expected_age_n$`Expected Verified` != 'Expected')
age_buckets = aggregate(age_buckets$Expected_Verified_n,
                        by = list(age_buckets$verified.week.date, age_buckets$age),
                        FUN = sum)
colnames(age_buckets) <- c("date", "age", "participants")
# Visualization
ggplot(na.omit(age_buckets), aes(x = date, y = participants)) + 
  geom_line(aes(color = age, linetype = age))+
  scale_y_continuous(name="Individuals, N", breaks=c(25,50,75,100) ) +
  labs( title=str_wrap(paste("Newly Validated Connect Participants by Age",currentDate, sep=" "),  60),
        x = "Validation Week Date")+
  theme(panel.background = element_blank(),
        legend.title = element_text(size=8),
        legend.position = "bottom",
        axis.line = element_line(linewidth = 0.2),
        axis.text.x = element_text(hjust = 0.5,size = 8, face = "bold"),  plot.title = element_text(hjust = 0.5,size = 12, face = "bold"))


#this week's age breakdown
#bar plot
this_week <- subset(age_buckets, age_buckets$date >= Sys.Date())
ggplot(na.omit(this_week), aes(x=age, y = participants, fill = age)) +
  geom_bar(stat = "identity")+
  theme_minimal()+
  labs(title = paste("Age Distribution of Newly Verified Participants, \nweek of", Sys.Date()))+
  scale_x_discrete(labels = c('40-45', '46-50', '51-55', '56-60', '61-65'))+
  scale_fill_discrete(labels = c('40-45', '46-50', '51-55', '56-60', '61-65'))




#race histogram
race_buckets = subset(verifed_expected_race_n,verifed_expected_race_n$`Expected Verified` != 'Expected')
race_buckets = aggregate(race_buckets$Expected_Verified_n,
                        by = list(race_buckets$verified.week.date, race_buckets$race),
                        FUN = sum)
colnames(race_buckets) <- c("date", "race", "participants")


#this week's race breakdown
this_week <- na.omit(subset(race_buckets, race_buckets$date >= Sys.Date()))
ggplot(this_week, aes(x=race, y = participants, fill = race)) +
  geom_bar(stat = "identity")+
  theme_minimal()+
  labs(title = paste("Race Distribution of Newly Verified Participants, \nweek of", Sys.Date()))













####################################################
#histogram by age, of current week data age breakdown
Age groups (histogram):
  Age: state_d_934298480, birth date, profile, Question 1 (M1), age to be categorical one same as one to; #PII variables, BQ2 only show the derived variable as age
participants_JP: D_371067537, D_205553981,
Module1: D_479353866,  D_150344905
age_calculated= round(time_length(difftime(module1$D_205553981, module1$DOB), "years"), digits=3)


######################################################
pie chart
Race groups:
  Working data: Module1_v1, module1_v2 for both b. and c
Race, M1; race <-  c("Connect_ID", "D_384191091_D_384191091_D_583826374", "D_384191091_D_384191091_D_636411467", "D_384191091_D_384191091_D_458435048",
                     "D_384191091_D_384191091_D_706998638", "D_384191091_D_384191091_D_973565052", "D_384191091_D_384191091_D_586825330",
                     "D_384191091_D_384191091_D_412790539", "D_384191091_D_384191091_D_807835037", "D_384191091_D_747350323", "D_384191091_D_384191091_D_746038746")















  
