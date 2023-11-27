#recreating jing plot from here: https://github.com/jeannewu/BiospecimenMetrics_Connect_SAS/blob/main/Connect_invites_activities_plot_08292023.Rmd
#outputplot <- "~/Documents/Connect_projects/Biospecimen_Feb2022/Mia_requests/ConnectPlots/"
activities_plot<- function(selected_hospital = NA){
  #pull data
  {modules <- c("d_100767870", "d_949302066", "d_536735468", "d_663265240", "d_976570371", "d_517311251", "d_832139544", "d_264644252", "d_770257102")
  bio.col <- c("d_684635302", "d_878865966", "d_167958071", "d_173836415_d_266600170_d_915179629", "d_173836415_d_266600170_d_718172863",
               "d_173836415_d_266600170_d_592099155", "d_173836415_d_266600170_d_561681068", "d_173836415_d_266600170_d_847159717",
               "d_173836415_d_266600170_d_448660695", "d_173836415_d_266600170_d_139245758", "d_173836415_d_266600170_d_541311218",
               "d_173836415_d_266600170_d_224596428", "d_173836415_d_266600170_d_740582332", "d_173836415_d_266600170_d_982213346",
               "d_173836415_d_266600170_d_398645039", "d_173836415_d_266600170_d_822274939")
  clc.bldtm <- c("d_173836415_d_266600170_d_769615780", "d_173836415_d_266600170_d_822274939", "d_173836415_d_266600170_d_398645039",
                 "d_173836415_d_266600170_d_982213346", "d_173836415_d_266600170_d_740582332")
  clc.urinetm <- c("d_173836415_d_266600170_d_139245758", "d_173836415_d_266600170_d_224596428", "d_173836415_d_266600170_d_541311218",
                   "d_173836415_d_266600170_d_939818935", "d_173836415_d_266600170_d_740582332")
  # age <- c("state_d_934298480", "d_914594314")
  # race <- c("d_821247024", "d_914594314",  "d_827220437","d_512820379","d_949302066" , "d_517311251")
  
  var_list_activity_plot <- c("token", "Connect_ID", "d_821247024", "d_914594314", "d_512820379", "state_d_158291096", "d_471593703", "d_827220437",
                              "d_130371375_d_266600170_d_787567527", "d_130371375_d_266600170_d_731498909","state_d_934298480","d_827220437", bio.col, modules)
  var_list_activity_plot <- var_list_activity_plot[!duplicated(var_list_activity_plot)]
  
  # Define the variables from the second query
  activity_plot_project <- 'nih-nci-dceg-connect-prod-6d04'
  activity_plot_dataset <- 'FlatConnect'
  activity_plot_table <- 'participants_JP'
  
  activity_plot_variables = paste(var_list_activity_plot, collapse = ", ")
  }
  source("/Users/sansalerj/Desktop/local_app/get_gcp_data.R", local = TRUE)
  #this reactive function will ensure that data from plot 1 is pulled and plotted
  #before it is overwritten for plot2
  data <- get_gcp_data(activity_plot_variables, activity_plot_dataset, activity_plot_table, activity_plot_project)
  
  
print(paste0("current selected_hospital variable value: ", selected_hospital))
options(knitr.table.format = "latex")
currentDate <- Sys.Date()-2
data <- expss::apply_labels(data,d_827220437 = "Site",#RcrtES_Site_v1r0
                            d_827220437 = c("HealthPartners"= 531629870,
                                            "Henry Ford Health System"=548392715,
                                            "Kaiser Permanente Colorado" = 125001209,
                                            "Kaiser Permanente Georgia" = 327912200,
                                            "Kaiser Permanente Hawaii" = 300267574,
                                            "Kaiser Permanente Northwest" = 452412599,
                                            "Marshfield Clinic Health System" = 303349821,
                                            "Sanford Health" = 657167265, 
                                            "University of Chicago Medicine" = 809703864,
                                            "National Cancer Institute" = 517700004,
                                            "National Cancer Institute" = 13,"Other" = 181769837))
print(paste0("activity plot data dimensions, beginning of file", dim(data)))
#filter by hospital if specified
hospital_list <- data.frame(hospital_name = c("HealthPartners", "Henry Ford Health System", "Kaiser Permanente Colorado",
                                              "Kaiser Permanente Georgia", "Kaiser Permanente Hawaii", "Kaiser Permanente Northwest",
                                              "Marshfield Clinic Health System","Sanford Health","University of Chicago Medicine",
                                              "National Cancer Institute", "National Cancer Institute", "other"), hospital_cid = c("531629870","548392715","125001209",
                                                                                                                                   "327912200","300267574", "452412599",
                                                                                                                                   "303349821", "657167265", "809703864",
                                                                                                                                   "517700004", "13", "181769837"))
if(is.na(selected_hospital) == FALSE){
  data <- data[data$d_827220437 == selected_hospital,]
  hospital_label <- hospital_list[hospital_list$hospital_cid==selected_hospital, 1]
}else{
  hospital_label = ""
}




# numbers_only <- function(x) !grepl("\\D", x) #\\D means "not digits"
# ##this function is T/F identifying any observations with numbers in them:
# #!grepl("\\D", 3) = TRUE
# 
# cnames <- names(data)
# for (i in 1: length(cnames)){
#    varname <- cnames[i]
#    var<-pull(data,varname) #selects that data column
#    data[,cnames[i]] <- ifelse(numbers_only(var), as.numeric(as.character(var)), var) #converting variables to numeric if applicable
#  }

#first round of filtering
#keeping participants who are not actively being recruited
#d_512820379 = recruitment type, where 180583933 == not active
#d_821247024 = match verification where 197316935 = cannot be verified
data <- data[which(data$d_512820379 != 180583933 | data$d_821247024 == 197316935),] #0 removeal
print(paste0("after removing not active recruits, ", dim(data)))
#d_821247024 = match verification where 922622075 = duplicate
#d_512820379 = recruitment type, where 486306141 = active
data <- data[which(data$d_821247024 != 922622075 | data$d_512820379 !=486306141),] #248
print(paste0("after removing duplicates, ", dim(data)))





#defining total recruitment, active/passive recruitment
#defining verified participants
#in order to calculate the number of participants by activity,
#we have to count the cumulative verified participants, then
#separate by activity. 
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#d_512820379 = recruitment type, where 486306141 = active
#d_821247024 = match verification where 922622075 = duplicate
print(paste0("activity plot data dimensions, beginning of file", dim(data)))
veri_resp <- data[which(data$d_512820379 != 486306141 | data$d_821247024 != 922622075 ),] %>%
                                  mutate(recru_time = ymd_hms(d_471593703),
                                  verified_time = ymd_hms(d_914594314),
                                  elgible.time = ymd_hms(d_130371375_d_266600170_d_787567527))
    
veri_resp$recrstart.date <-  as_date(min(ymd_hms(veri_resp$d_471593703),na.rm=TRUE))

#naming sites
if(is.na(selected_hospital)){
veri_resp <- apply_labels(veri_resp,d_827220437 = "Site",#RcrtES_Site_v1r0
                          d_827220437 = c("HealthPartners"= 531629870, "Henry Ford Health System"=548392715, 
                                          "Kaiser Permanente Colorado" = 125001209, "Kaiser Permanente Georgia" = 327912200,
                                          "Kaiser Permanente Hawaii" = 300267574,"Kaiser Permanente Northwest" = 452412599, 
                                          "Marshfield Clinic Health System" = 303349821,"Sanford Health" = 657167265, 
                                          "University of Chicago Medicine" = 809703864, "National Cancer Institute" = 517700004,
                                          "National Cancer Institute" = 13,"Other" = 181769837))
veri_resp$site <- factor(veri_resp$d_827220437,
                         levels=c("HealthPartners", "Henry Ford Health System","Marshfield Clinic Health System",
                                  "Sanford Health", "University of Chicago Medicine","Kaiser Permanente Colorado",
                                  "Kaiser Permanente Georgia","Kaiser Permanente Hawaii","Kaiser Permanente Northwest",
                                  "National Cancer Institute","Other"))
veri_resp$site <- droplevels(veri_resp$site)}
#recruitment start date, creating a variable that starts at the beginning of the recruitment
#where the first week of recruitment for the entire CC study is 1
recrstart.date <- as_date(min(ymd_hms(veri_resp$recru_time),na.rm=TRUE))
as_date(recrstart.date-days(5))
#"2021-07-18" #Sunday the censoring starting day
#1 #Sunday
#calculating when recruitment of an individual started and when they were recruited
veri_resp$recruit.week <- ceiling(as.numeric(difftime(as_date(veri_resp$recru_time), as_date(veri_resp$recrstart.date-days(5)),units="days"))/7) #to lock at Monday as the start of week
veri_resp$recruit.week.date <- veri_resp$recrstart.date + days(2) + dweeks(veri_resp$recruit.week-1) #to present the date of the starting date in the following week
summary(as_date(veri_resp$recruit.week.date))

#time between an individual being recruited and being verified
veri_resp$verified.week <- ceiling(as.numeric(difftime(as_date(ymd_hms(veri_resp$d_914594314)),as_date(veri_resp$recrstart.date-days(5)),units="days"))/7)
veri_resp$verified.week.date <- (veri_resp$recrstart.date) + days(2)+ dweeks(veri_resp$verified.week-1)



#create the time censoring variables by week
veri_resp <- veri_resp[which(veri_resp$recruit.week.date < currentDate),]
#as Michelle and Amelia want to set up the censor week as Monday to Sunday (the Sunday would be the last day of the week, and the report is done every Monday),
#the data on the week of the checking day would be excluded from the analytic dataset 
#number of recruited participants by week:
#d_512820379 = recruitment type, and 486306141 == active
recruit.wk.active <- veri_resp[which(veri_resp$d_512820379==486306141),] %>% 
  group_by(recruit.week,recruit.week.date) %>%
  dplyr::summarize(active_recruits=n(),
                   recruitdate_max=max(recru_time,rm.na=T))

#number of active verified participants by week
#d_512820379 = recruitment type, and 486306141 == active
#d_821247024 = verified
verified.wk.active <- veri_resp[which(veri_resp$d_821247024 == 197316935 & veri_resp$d_512820379==486306141 & veri_resp$verified.week.date < currentDate),] %>% 
  group_by(verified.week,verified.week.date) %>%
  dplyr::summarize(active_verifieds=n(),
                   verified_activetime.max=max(verified_time,rm.na=T))

#number of total verified participants by week
verified.wk.total <- veri_resp[which(veri_resp$d_821247024 == 197316935 & veri_resp$verified.week.date < currentDate),] %>% 
  group_by(verified.week,verified.week.date) %>%
  dplyr::summarize(total_verifieds=n(),
                   verified_time.max=max(verified_time,rm.na=T))

#merge number of total verified + active verified participants
verified.wk.all <- merge(verified.wk.active,verified.wk.total, by.x = c("verified.week","verified.week.date"),by.y=c("verified.week","verified.week.date"),all.y=TRUE,all.x=TRUE)
verified.wk.all$verified.week.date[is.na(verified.wk.all$verified.week.date)]<- recrstart.date + days(3)+ dweeks(verified.wk.all$verified.week-1)

#merge number of recruited participants with number of verified participants
recrui_wk_verified <- merge(recruit.wk.active,verified.wk.all, by.x=c("recruit.week","recruit.week.date"),by.y=c("verified.week","verified.week.date"),all.x=TRUE,all.y=TRUE)
recrui_wk_verified <- recrui_wk_verified %>% 
  mutate_at(c("active_recruits","active_verifieds","total_verifieds"),~replace_na(., 0))

#moving these 2 columns to the front of the dataset
recrui_wk_verified <- recrui_wk_verified %>% arrange(recruit.week,recruit.week.date)

#passive recruits are the passive verified
#new passive recruitment = total verified - active verified
recrui_wk_verified$passive_verifieds <- recrui_wk_verified$total_verifieds-recrui_wk_verified$active_verifieds 
#new active recruits = cumulative summation of active recruits by WEEK
recrui_wk_verified$active_recruits.cum <- cumsum(recrui_wk_verified$active_recruits)
recrui_wk_verified$passive_verifieds.cum <- cumsum(recrui_wk_verified$passive_verifieds)
recrui_wk_verified$active_verifieds.cum <- cumsum(recrui_wk_verified$active_verifieds)

#total recruits = weekly active recruits + passive verified recruits
recrui_wk_verified$total_recruits <- recrui_wk_verified$active_recruits + recrui_wk_verified$passive_verifieds 

#cumulative total recruits = cumulative summation(active recruits + passive verified recruits)
recrui_wk_verified$total_recruits.cum <- cumsum(recrui_wk_verified$active_recruits + recrui_wk_verified$passive_verifieds) 

#cumulative total verified = cumulative sum(total_verified)
recrui_wk_verified$total_verified.cum <- cumsum(recrui_wk_verified$total_verifieds)

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

#Now, we are going to only focus on verified participant activities
#we need to aggregate these individuals by activity,
#we still need the recruitment/verified info in order to understand the full population
#using the verified respondant dataset used above, veri_resp
#this time we must create variables which identify individuals activities (which surveys they have taken)
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
veri_act <- filter(veri_resp,d_821247024==197316935) %>% 
  mutate(law.time = ymd_hms(d_264644252),
         sas.time = ymd_hms(d_770257102),
         mre.time = ymd_hms(d_832139544),
         boh.time = ymd_hms(d_517311251),
         bldcol.time = case_when(d_878865966 == 353358909 ~ pmin(as.POSIXct(d_173836415_d_266600170_d_561681068),as.POSIXct(d_173836415_d_266600170_d_982213346),as.POSIXct(d_173836415_d_266600170_d_398645039),as.POSIXct(d_173836415_d_266600170_d_822274939),na.rm=TRUE)),
         urine.time = case_when(d_167958071 == 353358909 ~ pmin(as.POSIXct(d_173836415_d_266600170_d_847159717),as.POSIXct(d_173836415_d_266600170_d_139245758),as.POSIXct(d_173836415_d_266600170_d_541311218),as.POSIXct(d_173836415_d_266600170_d_224596428),na.rm=TRUE)),
         mw.time = ymd_hms(d_173836415_d_266600170_d_448660695),
         biospeDonation = ifelse(d_878865966 %in% c(104430631,NA)  & d_167958071 %in% c(104430631,NA) & d_684635302 %in% c(104430631,NA),"No Sample Donations",
                                 ifelse(d_878865966 == 353358909 & d_167958071 == 353358909 & d_684635302 == 353358909,"Completed All 3 Sample Donations", 
                                        "Completed Some but Not All 3 Sample Donations")),
         biospeComplete = case_when(biospeDonation =="Completed All 3 Sample Donations" | 
                                      (d_173836415_d_266600170_d_592099155==664882224 & d_878865966 == 353358909 & d_167958071 == 353358909)  ~ 1,
                                    biospeDonation == "No Sample Donations" | ( biospeDonation == "Completed Some but Not All 3 Sample Donations" & 
                                                                                  d_173836415_d_266600170_d_592099155 %in% c(534621077,NA)) | 
                                      d_173836415_d_266600170_d_592099155==664882224 & d_878865966 %in% c(104430631, NA) | d_167958071 %in% c(104430631, NA) ~ 0),
         Module.complete = ifelse(d_100767870==353358909,"Complete","NotComplete")
  )

#module.tm = the maximum date of survey completion
veri_act$module.tm <- do.call(pmax, c(veri_act[,c("sas.time","mre.time","law.time","boh.time")],na.rm=TRUE))
#d_100767870==353358909, where d_100767870 is completion of all surveys, 353358909 means yes
veri_act <- veri_act%>% 
  mutate(module.time  = as_datetime(ifelse(d_100767870 ==353358909,module.tm,ifelse( d_100767870 ==104430631,NA,NA))),
         bioany.time = pmin(as.POSIXct(bldcol.time),as.POSIXct(urine.time),as.POSIXct(mw.time), na.rm = TRUE), #the earliest time of any collection
         biocol.time = pmax(as.POSIXct(bldcol.time),as.POSIXct(urine.time),as.POSIXct(mw.time), na.rm = TRUE)) ##the most recent time of any collection

#calculating the week between the start of the study (week 1), and the participant completing an activity
#7-23-2021 is week 1, this is the first recruitment completion reported
veri_act <- veri_act%>% mutate(
  module.week = ifelse(!is.na(module.time),ceiling(as.numeric(difftime(as_date(module.time)+days(5),as_date(recrstart.date),units="days"))/7),NA),
  anybio.week = ifelse(!is.na(bioany.time),ceiling(as.numeric(difftime(as_date(bioany.time+days(5)),as_date(recrstart.date), units="days"))/7),NA),
  biocol.week = ifelse(!is.na(biocol.time),ceiling(as.numeric(difftime(as_date(biocol.time+days(5)),as_date(recrstart.date), units="days"))/7),NA),
  bldcol.week = ifelse(!is.na(bldcol.time),ceiling(as.numeric(difftime(as_date(bldcol.time+days(5)),as_date(recrstart.date), units="days"))/7),NA))

#allacts1= all surveys are completed and blood has been collected
veri_act <- veri_act%>% mutate(allacts1 = case_when(d_100767870 == 353358909 & d_878865966 == 353358909 ~ 353358909, #surveys are completed and blood drawn
                                                    d_100767870 == 104430631  | d_878865966 %in% c(0,NA) ~ 104430631 ))

#time1 is the maximum time that a participant has reported, max(module.time, bldcol.time)
#then converting this time into the running week counter
veri_act$allacts.time1 <- apply(veri_act[,c("module.time","bldcol.time")],1,max)
veri_act$allacts.week1 <- ceiling(as.numeric(difftime(as_date(as.POSIXct(veri_act$allacts.time1)+days(5)),
                                                      as_date(recrstart.date), units="days"))/7)

#blood and surveys completed, aggregated by the activity completed date
allacts.wk.total <- filter(veri_act,allacts1==353358909) %>% arrange(allacts.week1,verified.week) %>% 
  group_by(allacts.week1)%>%
  dplyr::summarize(total_allacts=n(),
                   allacts.time.max=max(allacts.time1,rm.na=T)) #blood + surveys

#aggregating the number of individuals who have completed all the surveys
module.wk.total <- filter(veri_act,d_100767870==353358909) %>% arrange(module.week,verified.week) %>% 
  group_by(module.week)%>%
  dplyr::summarize(total_module=n(),
                   module.time.max=max(as_date(module.time),rm.na=T))

#individuals who have completed the blood draw
bldcol.wk.total <- filter(veri_act,d_878865966 == 353358909) %>% arrange(bldcol.week,verified.week) %>% 
  group_by(bldcol.week)%>%
  dplyr::summarize(total_blood=n(),
                   bldcol.time.max=max(as_date(bldcol.time),rm.na=T))

#merge together all 3 aggregated tables
dt.merged1 <- merge(module.wk.total,bldcol.wk.total,by.x="module.week",by.y="bldcol.week",all.x=TRUE,all.y=TRUE)
dt.merged1 <- merge(dt.merged1,allacts.wk.total,by.x="module.week",by.y="allacts.week1",all.x=TRUE,all.y=TRUE)  
recr_svybld_wk <-  merge(recrui_wk_verified,dt.merged1, by.x="recruit.week",by.y="module.week",all.x=TRUE,all.y=TRUE)


recr.type.vars <- c("active_recruits","active_verifieds","passive_verifieds","total_recruits","total_verifieds","total_module","total_blood","total_allacts")
recr_svybld_wk <- recr_svybld_wk %>% mutate_at(recr.type.vars,~replace_na(., 0)) %>%  arrange(recruit.week)%>% 
  mutate(active_recruits.cum =cumsum(active_recruits),
         total_verified.cum = cumsum(total_verifieds),
         active_verifieds.cum = cumsum(active_verifieds),
         passive_verifieds.cum = cumsum(passive_verifieds),
         total_recruits = active_recruits + passive_verifieds,
         total_recruits.cum = cumsum(active_recruits + passive_verifieds),
         total_module.cum=cumsum(total_module),
         total_blood.cum=cumsum(total_blood),
         total_bldsvy.cum=cumsum(total_allacts),
         total_anyact = total_blood + total_module - total_allacts,
         total_anyact.cum=cumsum(total_anyact),
         total_veri_noact = total_verifieds - total_blood - total_module + total_allacts,
         total_veri_noact.cum=cumsum(total_veri_noact))

#reshape the data from wide to long
veri_svybld_wk <- melt(recr_svybld_wk[,c("recruit.week.date","total_anyact.cum","total_verified.cum", "total_blood.cum", "total_module.cum", "total_bldsvy.cum","total_veri_noact.cum")], 
                       measure.vars=c("total_verified.cum", "total_anyact.cum","total_blood.cum", "total_module.cum", "total_bldsvy.cum","total_veri_noact.cum"), variable.name="Verified_type",
                       value.name="Verified_Activities_n") 

veri_svybld_wk <- veri_svybld_wk %>% mutate(Verified_type=case_when(veri_svybld_wk[,2] =="total_verified.cum" ~ "Verified", 
                                                                    veri_svybld_wk[,2] =="total_anyact.cum" ~ "Surveys or Blood",
                                                                    veri_svybld_wk[,2] =="total_blood.cum" ~ "Blood", 
                                                                    veri_svybld_wk[,2] =="total_module.cum" ~ "Surveys", 
                                                                    veri_svybld_wk[,2] =="total_bldsvy.cum" ~ "Blood + Surveys",
                                                                    veri_svybld_wk[,2] =="total_veri_noact.cum" ~ "Verified, no Activities")) %>% arrange(veri_svybld_wk[,2])

#convert the verified.activites variable to a factor variable
veri_svybld_wk$verified.activities <- factor(veri_svybld_wk$Verified_type, 
                                             levels=c("Verified, no Activities","Blood + Surveys","Blood","Surveys","Surveys or Blood","Verified"))


# Extract unique monthly dates from data
unique_monthly_dates <- unique(as.Date(format(veri_svybld_wk$recruit.week.date, "%Y-%m-01")))

#plotly plot
Fig_all.plotly <- plot_ly() %>%
  add_lines(data = veri_svybld_wk, x = ~as.Date(recruit.week.date), color = ~veri_svybld_wk$verified.activities,
            y = ~veri_svybld_wk[,3]) %>%
  layout(
    title = list(text = paste0("Cumulative Number of Participants by Study Activities \n as of ", currentDate, " hospital filter: ", hospital_label),
                 font = list(size = 10)),
    xaxis = list(title = "Date", tickvals = unique_monthly_dates, ticktext = format(unique_monthly_dates, "%y-%m-%d"), showline = TRUE),
    yaxis = list(title = "Number of Participants", showline = TRUE),
    legend = list(x = 0, y = 1, traceorder = "normal", font = list(family = "sans-serif", size = 12, color = "black")),
    showlegend = TRUE
  ) %>%
  colorbar(title = "Baseline Study Activities", colors = c("lightblue", "purple", "red", "blue", "green", "#BEBADA"),
           tickvals = c(1, 2, 3, 4, 5, 6), ticktext = c("Verified, no Activities", "Surveys + Blood", "Blood", "Surveys", "Surveys or Blood", "Verified"))

# Print the plotly plot
Fig_all.plotly
}


