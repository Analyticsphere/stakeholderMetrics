#recreating jing plot from here: https://github.com/jeannewu/BiospecimenMetrics_Connect_SAS/blob/main/Connect_invites_activities_plot_08292023.Rmd
#outputplot <- "~/Documents/Connect_projects/Biospecimen_Feb2022/Mia_requests/ConnectPlots/"
activities_plot<- function(){
options(knitr.table.format = "latex")
currentDate <- Sys.Date()-2


modules <- c("d_100767870","d_949302066","d_536735468","d_663265240","d_976570371","d_517311251","d_832139544","d_264644252","d_770257102")
bio.col <- c("d_684635302","d_878865966", "d_167958071", "d_173836415_d_266600170_d_915179629", "d_173836415_d_266600170_d_718172863", "d_173836415_d_266600170_d_592099155", "d_173836415_d_266600170_d_561681068", "d_173836415_d_266600170_d_847159717", "d_173836415_d_266600170_d_448660695", "d_173836415_d_266600170_d_139245758", "d_173836415_d_266600170_d_541311218", "d_173836415_d_266600170_d_224596428", "d_173836415_d_266600170_d_740582332", "d_173836415_d_266600170_d_982213346", "d_173836415_d_266600170_d_398645039", "d_173836415_d_266600170_d_822274939")
###clinical collection time:
clc.bldtm <- c("d_173836415_d_266600170_d_769615780","d_173836415_d_266600170_d_822274939","d_173836415_d_266600170_d_398645039","d_173836415_d_266600170_d_982213346","d_173836415_d_266600170_d_740582332")
clc.urinetm <- c("d_173836415_d_266600170_d_139245758","d_173836415_d_266600170_d_224596428","d_173836415_d_266600170_d_541311218","d_173836415_d_266600170_d_939818935","d_173836415_d_266600170_d_740582332")
var.list <- c("token","Connect_ID","d_821247024","d_914594314","d_512820379","state_d_158291096","d_471593703","d_827220437",
              "d_130371375_d_266600170_d_787567527","d_130371375_d_266600170_d_731498909",bio.col,modules)

select <- paste(var.list,collapse=",")

project <- "nih-nci-dceg-connect-prod-6d04"
billing <- "nih-nci-dceg-connect-prod-6d04" ##project and billing should be consistent

#d_512820379 !=  '486306141', where d_512820379 = recruitment type and 486306141 = active
#d_821247024 != '922622075', where d_821247024 = indicator of match verification after creation of user profile,
#and 922622075 = duplicate
query <-  eval(parse(text=paste("bq_project_query(project, query=\"SELECT", select,"FROM  `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants_JP` Where d_512820379 != '180583933' and (d_512820379 !=  '486306141' or d_821247024 != '922622075') \")",sep=" ")))
data <- bq_table_download(query, bigint="integer64",n_max = Inf, page_size = 10000)

numbers_only <- function(x) !grepl("\\D", x) #\\D means "not digits"
##this function is T/F identifying any observations with numbers in them:
#!grepl("\\D", 3) = TRUE
###convert the numeric

cnames <- names(data)
# #  to check variables in recr_noinact_wl1
for (i in 1: length(cnames)){
  varname <- cnames[i]
  var<-pull(data,varname) #selects that data column
  data[,cnames[i]] <- ifelse(numbers_only(var), as.numeric(as.character(var)), var) #converting variables to numeric if applicable
}

#d_512820379 = recruitment type, where 180583933 == not active
#d_821247024 = match verification where 197316935 = cannot be verified
data <- data[which(data$d_512820379 != 180583933 | data$d_821247024 == 197316935),] #0 removeal
#d_821247024 = match verification where 922622075 = duplicate
#d_512820379 = recruitment type, where 486306141 = active
data <- data[which (data$d_821247024 != 922622075 | data$d_512820379 !=486306141),] #248
#d_512820379 = recruitment type, where 486306141 = active
#d_821247024 = match verification where 922622075 = duplicate
veri_resp <- data[which(data$d_512820379 != 486306141 | data$d_821247024 != 922622075 ),var.list] %>%
  mutate(recru_time = ymd_hms(d_471593703),
         verified_time = ymd_hms(d_914594314),
         elgible.time = ymd_hms(d_130371375_d_266600170_d_787567527))
veri_resp$recrstart.date <-  as_date(min(ymd_hms(veri_resp$d_471593703),na.rm=TRUE))


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
veri_resp$site <- droplevels(veri_resp$site)
recrstart.date <- as_date(min(ymd_hms(veri_resp$recru_time),na.rm=TRUE))
#wday(unique(veri_resp$recrstart.date), week_start = 1)
as_date(recrstart.date-days(5))
#[1] "2021-07-18" #Sunday the censoring starting day
#[1] 1 #Sunday
#calculating when recruitment of an individual started and when they were recruited
veri_resp$recruit.week <- ceiling(as.numeric(difftime(as_date(veri_resp$recru_time), as_date(veri_resp$recrstart.date-days(5)),units="days"))/7) #to lock at Monday as the start of week
veri_resp$recruit.week.date <- veri_resp$recrstart.date + days(2) + dweeks(veri_resp$recruit.week-1) #to present the date of the starting date in the following week
summary(as_date(veri_resp$recruit.week.date))
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2021-07-25" "2022-08-14" "2022-11-06" "2022-10-25" "2023-01-15" "2023-03-26" 

#time between an individual being recruited and being verified
veri_resp$verified.week <- ceiling(as.numeric(difftime(as_date(ymd_hms(veri_resp$d_914594314)),as_date(veri_resp$recrstart.date-days(5)),units="days"))/7)
veri_resp$verified.week.date <- (veri_resp$recrstart.date) + days(2)+ dweeks(veri_resp$verified.week-1)

min(as_date(ymd_hms(veri_resp$verified_time)),na.rm=TRUE)
# [1] "2021-07-27"
unique(as_date(min(veri_resp$verified.week.date,na.rm=TRUE)))
#[1] "2021-08-01"
unique(as_date(max(veri_resp$verified.week.date,na.rm=TRUE)))
#[1] "2023-03-26"
#7 Sunday
###to create the time censoring variables by week;
veri_resp <- veri_resp[which(veri_resp$recruit.week.date < currentDate),]
#as Michelle and Amelia want to set up the censor week as Monday to Sunday (the Sunday would be the last day of the week, and the report is done every Monday), the data on the week of the checking day would be excluded from the analytical dataset 
#verified  over time:
#d_512820379 = recruitment type, and 486306141 == active
recruit.wk.active <- veri_resp[which(veri_resp$d_512820379==486306141),] %>% 
  group_by(recruit.week,recruit.week.date) %>%
  dplyr::summarize(active_recruits=n(),
                   recruitdate_max=max(recru_time,rm.na=T))
#d_512820379 = recruitment type, and 486306141 == active
#d_821247024 = verified
verified.wk.active <- veri_resp[which(veri_resp$d_821247024 == 197316935 & veri_resp$d_512820379==486306141 & veri_resp$verified.week.date < currentDate),] %>% 
  group_by(verified.week,verified.week.date) %>%
  dplyr::summarize(active_verifieds=n(),
                   verified_activetime.max=max(verified_time,rm.na=T))

verified.wk.total <- veri_resp[which(veri_resp$d_821247024 == 197316935 & veri_resp$verified.week.date < currentDate),] %>% 
  group_by(verified.week,verified.week.date) %>%
  dplyr::summarize(total_verifieds=n(),
                   verified_time.max=max(verified_time,rm.na=T))

verified.wk.all <- merge(verified.wk.active,verified.wk.total, by.x = c("verified.week","verified.week.date"),by.y=c("verified.week","verified.week.date"),all.y=TRUE,all.x=TRUE)
verified.wk.all$verified.week.date[is.na(verified.wk.all$verified.week.date)]<- recrstart.date + days(3)+ dweeks(verified.wk.all$verified.week-1)
recrui_wk_verified <- merge(recruit.wk.active,verified.wk.all, by.x=c("recruit.week","recruit.week.date"),by.y=c("verified.week","verified.week.date"),all.x=TRUE,all.y=TRUE)
recrui_wk_verified <- recrui_wk_verified %>% 
  mutate_at(c("active_recruits","active_verifieds","total_verifieds"),~replace_na(., 0))

recrui_wk_verified <- recrui_wk_verified %>% arrange(recruit.week,recruit.week.date)

# passive recruits are the passive verified
recrui_wk_verified$passive_verifieds <- recrui_wk_verified$total_verifieds-recrui_wk_verified$active_verifieds #new passive recruitment
recrui_wk_verified$active_recruits.cum <- cumsum(recrui_wk_verified$active_recruits)
recrui_wk_verified$passive_verifieds.cum <- cumsum(recrui_wk_verified$passive_verifieds)
recrui_wk_verified$active_verifieds.cum <- cumsum(recrui_wk_verified$active_verifieds)

recrui_wk_verified$total_recruits <- recrui_wk_verified$active_recruits + recrui_wk_verified$passive_verifieds ##the total recruits till that week

recrui_wk_verified$total_recruits.cum <- cumsum(recrui_wk_verified$active_recruits + recrui_wk_verified$passive_verifieds) ##the total recruits till that week

recrui_wk_verified$total_verified.cum <- cumsum(recrui_wk_verified$total_verifieds)

#new response rate:
recrui_wk_verified$new.active.response.rate <- ifelse(recrui_wk_verified$active_recruits >0, 100*(recrui_wk_verified$active_verifieds/recrui_wk_verified$active_recruits), 0)
recrui_wk_verified$new.passive.response.rate <- ifelse(recrui_wk_verified$active_recruits!=0, 100*(recrui_wk_verified$passive_verifieds/recrui_wk_verified$active_recruits),NA)
recrui_wk_verified$new.total.response.rate <- ifelse(recrui_wk_verified$active_recruits>0, 100*(recrui_wk_verified$total_verifieds/recrui_wk_verified$active_recruits), 0)

#cumalative rate
recrui_wk_verified$cum.active.response.rate <- 100*(cumsum(recrui_wk_verified$active_verifieds)/cumsum(recrui_wk_verified$active_recruits))
recrui_wk_verified$cum.passive.response.rate <- 100*(cumsum(recrui_wk_verified$passive_verifieds)/cumsum(recrui_wk_verified$active_recruits))
recrui_wk_verified$cum.total.response.rate <- 100*(cumsum(recrui_wk_verified$total_verifieds)/cumsum(recrui_wk_verified$active_recruits))


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

veri_act$module.tm <- do.call(pmax, c(veri_act[,c("sas.time","mre.time","law.time","boh.time")],na.rm=TRUE))
veri_act <- veri_act%>% 
  mutate(module.time  = as_datetime(ifelse(d_100767870 ==353358909,module.tm,ifelse( d_100767870 ==104430631,NA,NA))),
         bioany.time = pmin(as.POSIXct(bldcol.time),as.POSIXct(urine.time),as.POSIXct(mw.time), na.rm = TRUE), #the earliest time of any collection
         biocol.time = pmax(as.POSIXct(bldcol.time),as.POSIXct(urine.time),as.POSIXct(mw.time), na.rm = TRUE)) ##the most recent time of any collection

veri_act <- veri_act%>% mutate(
  module.week = ifelse(!is.na(module.time),ceiling(as.numeric(difftime(as_date(module.time)+days(5),as_date(recrstart.date),units="days"))/7),NA),
  anybio.week = ifelse(!is.na(bioany.time),ceiling(as.numeric(difftime(as_date(bioany.time+days(5)),as_date(recrstart.date), units="days"))/7),NA),
  biocol.week = ifelse(!is.na(biocol.time),ceiling(as.numeric(difftime(as_date(biocol.time+days(5)),as_date(recrstart.date), units="days"))/7),NA),
  bldcol.week = ifelse(!is.na(bldcol.time),ceiling(as.numeric(difftime(as_date(bldcol.time+days(5)),as_date(recrstart.date), units="days"))/7),NA))

##to recreate the new variable of th competions based on the updated concept of completions as the completions 
###of all four baseline modules and blood collections as Yes.
veri_act <- veri_act%>% mutate(allacts0 = case_when(d_100767870 == 353358909 & biospeComplete==1 ~ 353358909,
                                                    d_100767870 == 104430631  | biospeComplete %in% c(0,NA) ~ 104430631 ),
                               allacts1 = case_when(d_100767870 == 353358909 & d_878865966 == 353358909 ~ 353358909,
                                                    d_100767870 == 104430631  | d_878865966 %in% c(0,NA) ~ 104430631 ))

veri_act$allacts.time1 <- apply(veri_act[,c("module.time","bldcol.time")],1,max)
veri_act$allacts.week1 <- ceiling(as.numeric(difftime(as_date(as.POSIXct(veri_act$allacts.time1)+days(5)),
                                                      as_date(recrstart.date), units="days"))/7)

allacts.wk.total <- filter(veri_act,allacts1==353358909) %>% arrange(allacts.week1,verified.week) %>% 
  group_by(allacts.week1)%>%
  dplyr::summarize(total_allacts=n(),
                   allacts.time.max=max(allacts.time1,rm.na=T)) #blood + surveys

module.wk.total <- filter(veri_act,d_100767870==353358909) %>% arrange(module.week,verified.week) %>% 
  group_by(module.week)%>%
  dplyr::summarize(total_module=n(),
                   module.time.max=max(as_date(module.time),rm.na=T))

bldcol.wk.total <- filter(veri_act,d_878865966 == 353358909) %>% arrange(bldcol.week,verified.week) %>% 
  group_by(bldcol.week)%>%
  dplyr::summarize(total_blood=n(),
                   bldcol.time.max=max(as_date(bldcol.time),rm.na=T))

dt.merged1 <- merge(module.wk.total,bldcol.wk.total,by.x="module.week",by.y="bldcol.week",all.x=TRUE,all.y=TRUE)
dt.merged1 <- merge(dt.merged1,allacts.wk.total,by.x="module.week",by.y="allacts.week1",all.x=TRUE,all.y=TRUE)  
#veri_wk_svybld <- merge(verified.wk.total, dt.merged1,by.x="verified.week",by="module.week", all.x=TRUE,all.y=TRUE)

recr_svybld_wk <-  merge(recrui_wk_verified,dt.merged1, by.x="recruit.week",by.y="module.week",all.x=TRUE,all.y=TRUE)

vars <- c("active_recruits","active_verifieds","passive_verifieds","total_recruits","total_verifieds","total_module","total_blood","total_allacts")
recr_svybld_wk <- recr_svybld_wk %>% mutate_at(vars,~replace_na(., 0)) %>%  arrange(recruit.week)%>% 
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

veri_svybld_wk <- melt(recr_svybld_wk[,c("recruit.week.date","total_anyact.cum","total_verified.cum", "total_blood.cum", "total_module.cum", "total_bldsvy.cum","total_veri_noact.cum")],#id.vars="Site", 
                       measure.vars=c("total_verified.cum", "total_anyact.cum","total_blood.cum", "total_module.cum", "total_bldsvy.cum","total_veri_noact.cum"), variable.name="Verified_type",
                       value.name="Verified_Activities_n") %>% 
  mutate(verified.activities=case_when(Verified_type =="total_verified.cum" ~ "Verified", 
                                       Verified_type =="total_anyact.cum" ~ "Surveys or Blood",
                                       Verified_type =="total_blood.cum" ~ "Blood", 
                                       Verified_type =="total_module.cum" ~ "Surveys", 
                                       Verified_type =="total_bldsvy.cum" ~ "Blood + Surveys",
                                       Verified_type =="total_veri_noact.cum" ~ "Verified, no Activities")) %>%
  arrange(verified.activities)

veri_svybld_wk$verified.activities <- factor(veri_svybld_wk$verified.activities, 
                                             levels=c("Verified, no Activities","Blood + Surveys","Blood","Surveys","Surveys or Blood","Verified"))
max_y <- max(veri_svybld_wk$Verified_Activities_n)
breaks <- seq(1000,1000*round(max_y/1000,0),1000)


# Extract unique monthly dates from data
unique_monthly_dates <- unique(as.Date(format(veri_svybld_wk$recruit.week.date, "%Y-%m-01")))

Fig_all.plotly <- plot_ly() %>%
  add_lines(data = veri_svybld_wk, x = ~as.Date(recruit.week.date), color = ~verified.activities,
            y = ~Verified_Activities_n) %>%
  layout(
    title = paste("Cumulative Number of Participants by Study Activities \nthroughout", currentDate, "Overall", sep = " "),
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


