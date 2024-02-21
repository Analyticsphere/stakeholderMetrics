clean_data <- function(data = data, type = "verified"){
  if(type == "verified"){
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
  
  data <- data %>%
    mutate(
      sex = case_when(
        state_d_706256705 == "536341288" | state_d_435027713 == "536341288" ~ "Female",
        state_d_706256705 == "654207589" | state_d_435027713 == "654207589" ~ "Male",
        state_d_706256705 == "830573274" ~ "Intersex or Other",
        state_d_706256705 %in% c("178420302", NA) | state_d_435027713 %in% c("178420302", NA) ~ "Unknown"
      ),
      biocol_type = case_when(
        d_878865966 == "353358909" & d_167958071 == "353358909" & d_684635302 == "353358909" ~ "All 3 Sample Donations",
        d_878865966 == "353358909" & d_167958071 == "353358909" & d_684635302 == "104430631" ~ "Blood & Urine",
        d_878865966 == "353358909" & d_167958071 == "104430631" & d_684635302 == "353358909" ~ "Blood & Mouthwash",
        d_878865966 == "104430631" & d_167958071 == "353358909" & d_684635302 == "353358909" ~ "Mouthwash & Urine",
        d_878865966 == "353358909" & d_167958071 == "104430631" & d_684635302 == "104430631" ~ "Blood Only",
        d_878865966 == "104430631" & d_167958071 == "353358909" & d_684635302 == "104430631" ~ "Urine Only",
        d_878865966 == "104430631" & d_167958071 == "104430631" & d_684635302 == "353358909" ~ "Mouthwash Only",
        d_878865966 == "104430631" & d_167958071 == "104430631" & d_684635302 == "104430631" ~ "No Samples"
      ),
      Msrv_complt = case_when(
        d_100767870 == "353358909" ~ "All 4 Survey Sections",
        d_100767870 == "104430631" & d_949302066 == "231311385" & d_536735468 != "231311385" & d_976570371 != "231311385" & d_663265240 != "231311385" ~ "BOH only",
        d_100767870 == "104430631" & d_949302066 == "231311385" & d_536735468 == "231311385" & d_976570371 != "231311385" & d_663265240 != "231311385" ~ "BOH and MRE",
        d_100767870 == "104430631" & d_949302066 == "231311385" & d_536735468 != "231311385" & d_976570371 == "231311385" & d_663265240 != "231311385" ~ "BOH and SAS",
        d_100767870 == "104430631" & d_949302066 == "231311385" & d_536735468 != "231311385" & d_976570371 != "231311385" & d_663265240 == "231311385" ~ "BOH and LAW",
        d_100767870 == "104430631" & d_949302066 == "231311385" & d_536735468 == "231311385" & d_976570371 == "231311385" & d_663265240 != "231311385" ~ "BOH, MRE, and SAS",
        d_100767870 == "104430631" & d_949302066 == "231311385" & d_536735468 == "231311385" & d_976570371 != "231311385" & d_663265240 == "231311385" ~ "BOH, MRE, and LAW",
        d_100767870 == "104430631" & d_949302066 == "231311385" & d_536735468 != "231311385" & d_976570371 == "231311385" & d_663265240 == "231311385" ~ "BOH, SAS, and LAW",
        d_100767870 == "104430631" & d_949302066 != "231311385" & d_536735468 != "231311385" & d_976570371 != "231311385" & d_663265240 != "231311385" ~ "No Survey Sections"),
      income = case_when(
        income == "374508062" ~ "Less than $10,000/year",
        income == "976555124" ~ "$10,000-$24,999/year",
        income == "745561936" ~ "$25,000-$34,999/year",
        income == "209571450" ~ "$35,000-$49,999/year",
        income == "212249150" ~ "$50,000-$74,999/year",
        income == "777814771" ~ "$75,000-$99,999/year",
        income == "922395188" ~ "$100,000-$149,999/year",
        income == "913602274" ~ "$150,000-$199,999/year",
        income == "742032816" ~ "$200,000 or more/year",
        income == "178420302" ~ "Unavailable/Unknown",
        income == "746038746" ~ "Declined",
        TRUE ~ "Unknown"  # Default case if none of the above conditions are met
      ))
}else{
data <- expss::apply_labels(data,#RcrtES_Site_v1r0
                                   site = c("HealthPartners"= 531629870,
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
  data <- data %>%
    mutate(
      sex = case_when(
        sex == "536341288" | sex == "536341288" ~ "Female",
        sex == "654207589" | sex == "654207589" ~ "Male",
        sex == "830573274" ~ "Intersex or Other",
        sex %in% c("178420302", NA) | sex %in% c("178420302", NA) ~ "Unknown"
      ))
}
}
