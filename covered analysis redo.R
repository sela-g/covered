rm(list = ls())
library(lme4)
library(MASS)
library(multcomp)
library(Gmisc)
library(RColorBrewer)
library(lubridate)
library(Hmisc)
library(broman)
library(dplyr)
library(ggplot2)
library(mgcv)
library(effects)
library(here)
library(tidyverse)
library(margins)
library(sjPlot)
library(rcompanion)
library(ggpubr)
library(ggeffects)
library(survey)
library(knitr)
library(lmerTest)
library(emmeans)
library(quantreg)
library(zoo)
library(sjstats)
library(kableExtra)
library(geepack)
library(viridis)
library(chron)
library(ghibli)
library(psych)
library(ltm)
library(readxl)

### Load data
setwd("\\\\PHSAhome2.phsabc.ehcnet.ca/sela.grays/covered")
getwd()


data <- read.csv("MainSurveyDatabaseCa-VaccineAttitudesIF_DATA_2024-01-12_1718.csv",header = TRUE)

## look at data

length(unique(data$record_id))
describeFactors(data$bl16_country_res[which(data$withdraw_answer == 1)])

data <- data[-which(data$withdraw_answer == 1), ]

EDDs <- data$bl1a_delivery_date

ids.can.EDD <- data$record_id[which(data$bl16_country_res == 1 & data$bl1a_delivery_date != "" &
                                      data$bl1a_delivery_date != "" &
                                      data$vaccine_attitudes_survey_timestamp != "" &
                                      data$bl1a_delivery_date > data$vaccine_attitudes_survey_timestamp
)]

ids.can.EDD.vacc <- data$record_id[which(data$bl16_country_res == 1 & data$bl1a_delivery_date != "" &
                                           data$bl1a_delivery_date != "" &
                                           data$vaccine_attitudes_survey_timestamp != "" &
                                           data$bl1a_delivery_date > data$vaccine_attitudes_survey_timestamp &
                                           data$do3_date != "" & data$bl1a_delivery_date > data$do3_date
)]


ids.can.EDD.unvacc <- data$record_id[which(data$bl16_country_res == 1 & data$bl1a_delivery_date != "" &
                                           data$bl1a_delivery_date != "" &
                                           data$vaccine_attitudes_survey_timestamp != "" &
                                           data$bl1a_delivery_date > data$vaccine_attitudes_survey_timestamp &
                                           data$do3_date != "" & data$bl1a_delivery_date > data$do3_date
)]
vacc.dates <-  data$do3_date


data_bl <- data[which(data$redcap_event_name == "baseline_arm_1" & data$record_id %in% ids.can.EDD), ]
data_2mo <- data[which(data$redcap_event_name == "followup_2_months_arm_1" & data$record_id %in% ids.can.EDD), ]
data_4mo <- data[which(data$redcap_event_name == "followup_4_months_arm_1" & data$record_id %in% ids.can.EDD), ]
data_6mo <- data[which(data$redcap_event_name == "followup_6_months_arm_1" & data$record_id %in% ids.can.EDD), ]
data_8mo <- data[which(data$redcap_event_name == "followup_8_months_arm_1" & data$record_id %in% ids.can.EDD), ]
data_10mo <- data[which(data$redcap_event_name == "followup_10_months_arm_1" & data$record_id %in% ids.can.EDD), ]
data_14mo <- data[which(data$redcap_event_name == "followup_14_months_arm_1" & data$record_id %in% ids.can.EDD), ]

# followup_10_months_arm_1 "3,522"
# followup_14_months_arm_1 "411"   
# followup_2_months_arm_1  "4,992"
# followup_4_months_arm_1  "4,725"
# followup_6_months_arm_1  "4,399"
# followup_8_months_arm_1  "3,975"

## clear out empty rows and columns
### some columns are entirely "" rather than NA
data_bl <- data_bl %>% 
  mutate(across(where(is.factor), ~as.character(.x))) %>% 
  mutate(across(where(is.character), ~ifelse(.x == "", NA_character_, .)))

data_2mo <- data_2mo %>% 
  mutate(across(where(is.factor), ~as.character(.x))) %>% 
  mutate(across(where(is.character), ~ifelse(.x == "", NA_character_, .)))

data_4mo <- data_4mo %>% 
  mutate(across(where(is.factor), ~as.character(.x))) %>% 
  mutate(across(where(is.character), ~ifelse(.x == "", NA_character_, .)))

data_6mo <- data_6mo %>% 
  mutate(across(where(is.factor), ~as.character(.x))) %>% 
  mutate(across(where(is.character), ~ifelse(.x == "", NA_character_, .)))

data_8mo <- data_8mo %>% 
  mutate(across(where(is.factor), ~as.character(.x))) %>% 
  mutate(across(where(is.character), ~ifelse(.x == "", NA_character_, .)))

data_10mo <- data_10mo %>% 
  mutate(across(where(is.factor), ~as.character(.x))) %>% 
  mutate(across(where(is.character), ~ifelse(.x == "", NA_character_, .)))

data_14mo <- data_14mo %>% 
  mutate(across(where(is.factor), ~as.character(.x))) %>% 
  mutate(across(where(is.character), ~ifelse(.x == "", NA_character_, .)))

# data_bl <- janitor::remove_empty(data_bl)
# data_2mo <- janitor::remove_empty(data_2mo)
# data_4mo <- janitor::remove_empty(data_4mo)
# data_6mo <- janitor::remove_empty(data_6mo)
# data_8mo <- janitor::remove_empty(data_8mo)
# data_10mo <- janitor::remove_empty(data_10mo)
# data_14mo <- janitor::remove_empty(data_14mo)

# find variable names in common
colnames(data_bl) <- paste0(colnames(data_bl), "_BL")
colnames(data_2mo) <- paste0(colnames(data_2mo), "_2mo")
colnames(data_4mo) <- paste0(colnames(data_4mo), "_4mo")
colnames(data_6mo) <- paste0(colnames(data_6mo), "_6mo")
colnames(data_8mo) <- paste0(colnames(data_8mo), "_8mo")
colnames(data_10mo) <- paste0(colnames(data_10mo), "_10mo")
colnames(data_14mo) <- paste0(colnames(data_14mo), "_14mo")

data_bl$record_id <- data_bl$record_id_BL
data_2mo$record_id <- data_2mo$record_id_2mo
data_4mo$record_id <- data_4mo$record_id_4mo
data_6mo$record_id <- data_6mo$record_id_6mo
data_8mo$record_id <- data_8mo$record_id_8mo
data_10mo$record_id <- data_10mo$record_id_10mo
data_14mo$record_id <- data_14mo$record_id_14mo

data_use <- left_join(data_bl, data_2mo, by = "record_id")
data_use <- left_join(data_use, data_4mo, by = "record_id")
data_use <- left_join(data_use, data_6mo, by = "record_id")
data_use <- left_join(data_use, data_8mo, by = "record_id")
data_use <- left_join(data_use, data_10mo, by = "record_id")
data_use <- left_join(data_use, data_14mo, by = "record_id")

data_use <- data_use %>%
  mutate(
    dose1_T = case_when(
      bl4_dose1_BL == 0 & bl1_currently_preg_BL == 1 ~ "No vaccine pregnant",
      bl4_dose1_BL == 0 & bl1_currently_preg_BL == 0 ~ "No vaccine NOT pregnant",
      do2_timing_BL == 2 | do2_timing2___2_BL == 1 ~ "D1 during preg",
      do2_timing_BL == 1 | do2_timing2___1_BL == 1 ~ "D1 before preg",
      do2_timing_BL == 3 | do2_timing2___3_BL == 1 ~ "D1 post-partum/BF",
      do2_timing_BL == 4 | do2_timing2___4_BL == 1 ~ "D1 post-partum/Not BF"
    )
  )

data <- data_use

#VACCINATED IN PREG
data[which(data$record_id %in% ids.can.EDD.vacc),]

#UNVACCINATED IN PREG 
data[-which(data$record_id %in% ids.can.EDD.vacc),]

#preg_outcomes <- rowSums(!is.na(data[, c("pregnancy_outcomes_timestamp")]))
data_use$num_preg_outcomes <- rowSums(!is.na(data_use[, c("pregnancy_outcomes_timestamp_BL", "pregnancy_outcomes_timestamp_2mo", "pregnancy_outcomes_timestamp_4mo", "pregnancy_outcomes_timestamp_6mo", "pregnancy_outcomes_timestamp_8mo", "pregnancy_outcomes_timestamp_10mo", "pregnancy_outcomes_timestamp_14mo")]))

## only 1 or 0, this might help a lot

# write a for loop to figure out which of the survey times contains the pregnancy outcome information

data_use$which_time_preg_out <- c()

for(i in 1:nrow(data_use)){
  xx <- data_use[i, c("pregnancy_outcomes_timestamp_BL", "pregnancy_outcomes_timestamp_2mo", "pregnancy_outcomes_timestamp_4mo", "pregnancy_outcomes_timestamp_6mo", "pregnancy_outcomes_timestamp_8mo", "pregnancy_outcomes_timestamp_10mo", "pregnancy_outcomes_timestamp_14mo")]
  # print(i)
  # print(which(!is.na(xx)))
  
  if(all(is.na(xx))) {
    data_use$which_time_preg_out[i] = "No preg outcome"
  }
  
  else if(length(which(!is.na(xx))) > 1){
    data_use$which_time_preg_out[i] = which(!is.na(xx))[1]
  }
  
  else {
    data_use$which_time_preg_out[i] = which(!is.na(xx))
  }
  
}



###### exclusions
# 1. exclude everyone who does not have a pregnancy outcomes survey filled in
data_ex1 <- data_use[-which(data_use$which_time_preg_out == "No preg outcome"), ] # 5688 to 5190
#describeFactors(data_ex1$include_BL)
# D1&2 in preg           "1,967 (37.9%)"
# exclude                "514 (9.9%)"
# exclude not preg       "57 (1.1%)"
# include D1 in preg     "864 (16.6%)"
# include D1 pp          "1,625 (31.3%)"
# include D1&2&3 in preg "16 (0.3%)"
# include no vax BL      "75 (1.4%)"
# Missing                "72 (1.4%)"

# 2. exclude the ones that should be excluded from using BL information (eg vaxx before preg or not pregnant)
data_ex2 <- data_ex1[-which(data_ex1$include_BL %in% c("exclude", "exclude not preg") | is.na(data_ex1$include_BL)), ] # 4547

data_ex2$all_dose1_info <- paste(data_ex2$dose1_T, data_ex2$dose1_T_2mo,
                                 data_ex2$dose1_T_4mo, data_ex2$dose1_T_6mo,
                                 data_ex2$dose1_T_8mo, data_ex2$dose1_T_10mo,
                                 data_ex2$dose1_T_14mo, sep = "_")

data_ex2$all_dose1_dates <- paste(data_ex2$do3_date_BL, data_ex2$do3_date_2mo,
                                  data_ex2$do3_date_4mo, data_ex2$do3_date_6mo,
                                  data_ex2$do3_date_8mo, data_ex2$do3_date_10mo,
                                  data_ex2$do3_date_14mo, sep = "_")

data_ex2$all_dose1_dates <- gsub("NA_", "", data_ex2$all_dose1_dates)
data_ex2$all_dose1_dates <- gsub("__", "", data_ex2$all_dose1_dates)
data_ex2$all_dose1_dates <- gsub("_", NA_character_, data_ex2$all_dose1_dates)

data_ex2$all_baby_dob <- paste(data_ex2$i1_dob1_BL, data_ex2$i1_dob1_2mo,
                               data_ex2$i1_dob1_4mo, data_ex2$i1_dob1_6mo,
                               data_ex2$i1_dob1_8mo, data_ex2$i1_dob1_10mo,
                               data_ex2$i1_dob1_14mo, sep = "_")

data_ex2$all_baby_dob <- gsub("NA_", "", data_ex2$all_baby_dob)
data_ex2$all_baby_dob <- gsub("_NA", "", data_ex2$all_baby_dob)
data_ex2$all_baby_dob <- gsub("NA", NA_character_, data_ex2$all_baby_dob)


data_ex2$all_dose2_info <- paste(data_ex2$dose2_T, data_ex2$dose2_T_2mo,
                                 data_ex2$dose2_T_4mo, data_ex2$dose2_T_6mo,
                                 data_ex2$dose2_T_8mo, data_ex2$dose2_T_10mo,
                                 data_ex2$dose2_T_14mo, sep = "_")

data_ex2$all_dose2_dates <- paste(data_ex2$dt3_date_BL, data_ex2$dt3_date_2mo,
                                  data_ex2$dt3_date_4mo, data_ex2$dt3_date_6mo,
                                  data_ex2$dt3_date_8mo, data_ex2$dt3_date_10mo,
                                  data_ex2$dt3_date_14mo, sep = "_")

data_ex2$all_dose2_dates <- gsub("NA_", "", data_ex2$all_dose2_dates)
data_ex2$all_dose2_dates <- gsub("__", "", data_ex2$all_dose2_dates)
data_ex2$all_dose2_dates <- gsub("_", NA_character_, data_ex2$all_dose2_dates)

data_ex2$all_dose3_info <- paste(data_ex2$dose3_T, data_ex2$dose3_T_2mo,
                                 data_ex2$dose3_T_4mo, data_ex2$dose3_T_6mo,
                                 data_ex2$dose3_T_8mo, data_ex2$dose3_T_10mo,
                                 data_ex2$dose3_T_14mo, sep = "_")

data_ex2$all_dose3_dates <- paste(data_ex2$dt3_date_vb_BL, data_ex2$dt3_date_vb_2mo,
                                  data_ex2$dt3_date_vb_4mo, data_ex2$dt3_date_vb_6mo,
                                  data_ex2$dt3_date_vb_8mo, data_ex2$dt3_date_vb_10mo,
                                  data_ex2$dt3_date_vb_14mo, sep = "_")

data_ex2$all_dose3_dates <- gsub("NA_", "", data_ex2$all_dose3_dates)
data_ex2$all_dose3_dates <- gsub("_NA", "", data_ex2$all_dose3_dates)
data_ex2$all_dose3_dates <- gsub("NA", NA_character_, data_ex2$all_dose3_dates)
data_ex2$all_dose3_dates <- gsub("_.*", "", data_ex2$all_dose3_dates)

data_ex2$all_dose1_dates <- as.Date(data_ex2$all_dose1_dates)
data_ex2$all_dose2_dates <- as.Date(data_ex2$all_dose2_dates)
data_ex2$all_dose3_dates <- as.Date(data_ex2$all_dose3_dates)
data_ex2$all_baby_dob <- as.Date(data_ex2$all_baby_dob)

data_ex2$all_dose2_dates[which(data_ex2$all_dose2_dates == "NA")] <- NA

### calculate GA where possible and cross-check with timing
data_ex2$bl1a_delivery_date_BL <- as.Date(data_ex2$bl1a_delivery_date_BL)
data_ex2$dose1_GA <- as.numeric((280 - (data_ex2$bl1a_delivery_date_BL - data_ex2$all_dose1_dates))/7)
data_ex2$dose2_GA <- as.numeric((280 - (data_ex2$bl1a_delivery_date_BL - data_ex2$all_dose2_dates))/7)
data_ex2$dose3_GA <- as.numeric((280 - (data_ex2$bl1a_delivery_date_BL - data_ex2$all_dose3_dates))/7)

data_ex2$GA_del <- as.numeric((280 - (data_ex2$bl1a_delivery_date_BL - data_ex2$all_baby_dob))/7)


######### OTHER VARIABLES ############
## age
data$bl14_dob_month
data$bl14_dob_year

data$dob <- paste(data$bl14_dob_year, data$bl14_dob_month, "01", sep = "-")
data$dob <- as.Date(data$dob, format = "%Y-%m-%d")
data$bl_completed_date <- as.Date(data$bl_completed_date, format = "%Y-%m-%d")
# 
summary(data$dob)

data$bl14_dob_month_BL[data$bl14_dob_year_BL == "1992.06"& !is.na(data$bl14_dob_year_BL)] <- 06
data$bl14_dob_year_BL[data$bl14_dob_year_BL == "1992.06"& !is.na(data$bl14_dob_year_BL)] <- 1992

data$bl14_dob_month_BL[!is.na(data$bl14_dob_year_BL) & is.na(data$bl14_dob_month_BL)] <- 01
data$dob[which(data$dob == "0026-03-01")] <- NA
data$dob[data$bl14_dob_year_BL >= 2020 & !is.na(data$bl14_dob_year_BL)] <- NA
data$dob <- as.Date(data$dob)


data$m_age <- data$bl_completed_date_BL - data$dob
data$m_age <- as.numeric(data$m_age)/365.25
summary(data$m_age)
data <- data %>% 
  mutate(
    age_cat = case_when(
      m_age <25 ~ "20-24",
      m_age <30 ~ "25-29",
      m_age <35 ~ "30-34",
      m_age <40 ~ "35-39",
      m_age <45 ~ "40-44",
      m_age >=45 ~ "45+"
    )
  )
describeFactors(data$age_cat)


## single parent
data$bl18_parent_alone_BL
describeFactors(data$bl18_parent_alone_BL)
data <- data %>%
  mutate(
    single = case_when(
      bl18_parent_alone_BL == 1 ~ "Parent alone",
      bl18_parent_alone_BL == 2 ~ "With another person",
      bl18_parent_alone_BL == 997 ~ "Prefer not to answer"
    )
  )
describeFactors(data$single)

## Obstetric History 

summary(data$bl37_timespreg_BL)
summary(data$bl37_timespreg_unknown___999_BL)
describeFactors(data$bl37_timespreg_unknown___999_BL)
#note that two people selected "unknown" for "How many times have you been pregnant?"
mean(data$bl37_timespreg_BL, na.rm= TRUE)
sd(data$bl37_timespreg_BL, na.rm=TRUE)
range(data$bl37_timespreg_BL, na.rm=TRUE)

length(which(data$bl37_timespreg_BL == "0"))

## now to make gravidity a categorical variable and analyze it that way

data <- data %>% 
  mutate(
    gravid_cat = case_when(
      bl37_timespreg_BL < 2 ~ "Primigravida",
      bl37_timespreg_BL >= 2 ~ "Multigravidas",
    )
  )

describeFactors(data$gravid_cat)
data$gravid_cat <- factor(data$gravid_cat, levels = c("Multigravidas", "Primigravida"))


#and for live births: 

summary(data$bl38_livebirths_BL)
summary(data$bl38_livebirths_unknown___999_BL)
describeFactors(data$bl38_livebirths_unknown___999_BL)

mean(data$bl38_livebirths_BL, na.rm=TRUE)
sd(data$bl38_livebirths_BL, na.rm=TRUE)
range(data$bl38_livebirths_BL, na.rm=TRUE)

# now as a categorical variable 
library(dplyr)

data <- data %>% 
  mutate(
    livebirth_cat = case_when(
      bl38_livebirths_BL <1 ~ "No Live Births",
      bl38_livebirths_BL == 1 ~ "one previous live birth",
      bl38_livebirths_BL >=2 ~ "At least two previous live births",
      TRUE ~ as.character(bl38_livebirths_BL)
    )
  )

describeFactors(data$livebirth_cat)

## ethnicity
data$bl19_ethnicity___1_BL
data <- data %>% 
  mutate(
    eth = case_when(
      bl19_ethnicity___2_BL == 1 ~ "African/Caribbean/Black",
      bl19_ethnicity___3_BL == 1 ~ "Hispanic/Latino/Latina",
      bl19_ethnicity___4_BL == 1 ~ "East Asian",
      bl19_ethnicity___5_BL == 1 ~ "South Asian",
      bl19_ethnicity___6_BL == 1 ~ "South East Asian",
      bl19_ethnicity___7_BL == 1 ~ "West Central Asian/Middle Eastern",
      bl19_ethnicity___8_BL == 1 ~ "Indigenous originating from North America",
      bl19_ethnicity___9_BL == 1 ~ "Indigneous originating from outside North America",
      bl19_ethnicity___10_BL == 1 ~ "Mixed",
      bl19_ethnicity___998_BL == 1 ~ "Other",
      bl19_ethnicity___997_BL == 1 ~ "Prefer not to answer",
      bl19_ethnicity___999_BL == 1 ~ "Unknown",
      bl19_ethnicity___1_BL == 1 ~ "White/Caucasian"
    )
  )

describeFactors(data$eth)


## gender
describeFactors(data$bl20_gender_BL)
describeFactors(data$bl20_gender_other_BL)

data <- data %>% 
  mutate(
    gender = case_when(
      bl20_gender_BL == 1 ~ "Woman",
      !is.na(bl20_gender_other_BL) ~ "Nonbinary/Genderqueer",
      bl20_gender_BL == 3 ~ "Nonbinary/Genderqueer",
      bl20_gender_BL == 998 ~ "Nonbinary/Genderqueer",
      TRUE ~ NA_character_
    )
  )
describeFactors(data$gender)
data$gender <- factor(data$gender, levels = c("Woman", "Nonbinary/Genderqueer"))

## education
data$bl123_education_BL
describeFactors(data$bl23_education_BL)
data$bl23_education_BL <- factor(data$bl23_education_BL, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 997, 999), labels = c("Less than high school graduation", "High school graduation", "Trade certificate/Vocational", "Apprenticeship", "Non-University certificate or diploma", "Community college/CEGEP", "Bachelor's degree", "Graduate degree", NA, NA))

## combine
data$bl23_education_BL <- factor(data$bl23_education_BL)
levels(data$bl23_education_BL)[c(1, 2)] <- "High school or less"
levels(data$bl23_education_BL)[c(2, 3, 4, 5)] <- "Trade/Vocational/Apprenticeship/Certificate/College/CEGEP"

## employment
describeFactors(data$bl22_employ_BL)
data$bl22_employ_BL <- factor(data$bl22_employ_BL, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 997, 998), labels = c("Employed", "Enrolled in College/University", "Volunteering", "Homemaker", "On leave", "Retired", "Unemployed", "Unable to work", "Prefer not to answer", "Other"))

# Lump unemployed, unable to work, and retired
# Lump other, prefer not to answer, unknown, and homemaker

data <- data %>% 
  mutate(
    employ_cat = case_when(
      bl22_employ_BL %in% c("Unemployed", "Unable to work", "Retired") ~ "Not working",
      bl22_employ_BL %in% c("Prefer not to answer", "Other", "Homemaker", "Volunteering") ~ "Other",
      is.na(bl22_employ_BL) ~ "Other",
      TRUE ~ as.character(bl22_employ_BL)
    )
  )

describeFactors(data$employ_cat)

htmlTable::htmlTable(output_data, align = "rrrr",
                     rgroup = rgroup, n.rgroup = n.rgroup, 
                     css.rgroup.sep = "", 
                     cgroup = cgroup,
                     n.cgroup = n.cgroup,
                     rowlabel = "", 
                     caption = "", 
                     ctable = TRUE) 

# Arianne's analysis
# PHO
# 
table(data$v31_pho_approve_BL) 
# recode to -2 to 2
data$v31_pho_approve_BL <- data$v31_pho_approve_BL-3

table(data$v32_pho_important_BL)
# then multiply by v32_pho_important
data$SNI.PHO <- data$v31_pho_approve_BL * data$v32_pho_important_BL
summary(data$SNI.PHO)

describeFactors(data$v31_pho_approve_BL)

# partner
table(data$v33_partner_approve_BL)
data$v33_partner_approve_BL <- replace(data$v33_partner_approve_BL, which(data$v33_partner_approve_BL == 999), NA)
# recode to -2 to 2
data$v33_partner_approve_BL <- data$v33_partner_approve_BL-3

# then multiply by v34_partner_important

table(data$v34_partner_important_BL)

data$v34_partner_important_BL <- replace(data$v34_partner_important_BL, which(data$v34_partner_important_BL == 999), NA)

data$SNI.PART <- data$v33_partner_approve_BL * data$v34_partner_important_BL
summary(data$SNI.PART)


# Friends/fam
table(data$v35_fam_approve_BL)
# recode to -2 to 2
data$v35_fam_approve_BL <- data$v35_fam_approve_BL-3

# then multiply by v36_fam_important
table(data$v36_fam_important_BL)

data$SNI.FRI <- data$v35_fam_approve_BL * data$v36_fam_important_BL
summary(data$SNI.FRI)


# prenatal care provider
table(data$v37_prenatal_approve_BL)

data$v37_prenatal_approve_BL <- replace(data$v37_prenatal_approve_BL, which(data$v37_prenatal_approve_BL == 999), NA)
# recode to -2 to 2
data$v37_prenatal_approve_BL <- data$v37_prenatal_approve_BL-3

# then multiply by v38_prenatal_important
table(data$v38_prenatal_important_BL)

data$v38_prenatal_important_BL <- replace(data$v38_prenatal_important_BL, which(data$v38_prenatal_important_BL == 999), NA)

data$SNI.PRE <- data$v37_prenatal_approve_BL * data$v38_prenatal_important_BL
summary(data$SNI.PRE)


sn.ca <- alpha(data[, c("SNI.PHO", "SNI.PART", "SNI.FRI", "SNI.PRE")])
sn.ca
#          lower alpha upper
# Feldt     0.581   0.603  0.625

data <- data %>% 
  rowwise() %>% 
  mutate(SNI.TOT = sum(SNI.PHO, SNI.PART, SNI.FRI, SNI.PRE))

summary(data$SNI.TOT)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -34.00   21.00   30.00   26.95   35.00   40.00     192

data.isn <- data %>% 
  ungroup() %>% 
  summarise(across(c(v31_pho_approve_BL:v38_prenatal_important_BL, SNI.PHO, SNI.PART, SNI.FRI, SNI.PRE, SNI.TOT) , list(mean = mean, sd = sd), na.rm = TRUE))

(data.isn2 <- t(data.isn))




# WHO scale
# new vaccine more risk, concerned about adverse effects 8 and 9 = risks 

describeFactors(data$v16_risks_BL)
describeFactors(data$v17_effects_BL)

# get the average scores


data <- data %>% 
  rowwise() %>% 
  mutate(VHS_risk = sum(c(v16_risks_BL, v17_effects_BL)))

summary(data$VHS_risk)


VHS_risk <- alpha(data[, c("v16_risks_BL", "v17_effects_BL")])
VHS_risk
#          lower alpha upper
# Feldt    0.626 0.651 0.673

data.who <- data %>% 
  ungroup() %>% 
  summarise(across(c(v16_risks_BL, v17_effects_BL, VHS_risk), list(mean = mean, sd = sd), na.rm = TRUE))

(data.who <- t(data.who))
data.who





getT1Stat <- function(varname, digits=0, useNA = "ifany"){
  getDescriptionStatsBy(data[, varname], 
                        data$likely, 
                        add_total_col=TRUE,
                        show_all_values=TRUE, 
                        statistics = TRUE,
                        hrzl_prop = TRUE,
                        html=TRUE, 
                        useNA = useNA,
                        digits=digits,
                        header_count = TRUE)
}


getT1Stat <- function(varname, digits=0, useNA = "ifany"){
  getDescriptionStatsBy(data[, varname], 
                        data$likely, 
                        add_total_col=TRUE,
                        show_all_values=TRUE, 
                        statistics = TRUE,
                        html=TRUE, 
                        useNA = useNA,
                        digits=digits,
                        header_count = TRUE)
}

data$likely <- relevel(factor(data$likely), ref = "Yes")

table_data <- list()

data <- as.data.frame(data)

library(dplyr)
library(Gmisc)
library(Hmisc)
library(htmlTable)


# Get the basic stats

table_data[["Age"]] <- getT1Stat("age_cat", 1,)
table_data[["Single"]] <- getT1Stat("single", 1)
table_data[["Gravidity"]] <- getT1Stat("gravid_cat", 1)
table_data[["Live Births"]] <- getT1Stat("livebirth_cat", 1)

table_data[["Ethnicitiy Caucasian"]] <- getT1Stat("bl19_ethnicity___1_BL")
table_data[["Ethnicitiy African/Black"]] <- getT1Stat("bl19_ethnicity___2_BL")
table_data[["Ethnicitiy Hispanic"]] <- getT1Stat("bl19_ethnicity___3_BL")
table_data[["Ethnicitiy East Asian"]] <- getT1Stat("bl19_ethnicity___4_BL")
table_data[["Ethnicitiy South Asian"]] <- getT1Stat("bl19_ethnicity___5_BL")
table_data[["Ethnicitiy South East Asian"]] <- getT1Stat("bl19_ethnicity___6_BL")
table_data[["Ethnicitiy Middle Eastern"]] <- getT1Stat("bl19_ethnicity___7_BL")
table_data[["Ethnicitiy Indigenous NA"]] <- getT1Stat("bl19_ethnicity___8_BL")
table_data[["Ethnicitiy Indigenous outside NA"]] <- getT1Stat("bl19_ethnicity___9_BL")
table_data[["Ethnicitiy Mixed"]] <- getT1Stat("bl19_ethnicity___10_BL")
table_data[["Ethnicitiy Prefer not to answer"]] <- getT1Stat("bl19_ethnicity___997_BL")
table_data[["Ethnicitiy Other"]] <- getT1Stat("bl19_ethnicity___998_BL")
table_data[["ethnicity unknown"]] <- getT1Stat("bl19_ethnicity___999_BL")
table_data[["Ethnicity Other_new"]] <- getT1Stat("other_new")


table_data[["Ethnicity"]] <- getT1Stat("eth2", 1)
table_data[["Gender"]] <- getT1Stat("gender", 1)
table_data[["Country of Birth"]] <- getT1Stat("bl15_country_born_BL", 1)
table_data[["Province of residence"]] <- getT1Stat("prov_cat", 1)
table_data[["Education"]] <- getT1Stat("bl23_education_BL", 1)
table_data[["Employment"]] <- getT1Stat("employ_cat", 1)

table_data[["When you are not pregnant/lactating, do you receive all recommended vaccines?"]] <- getT1Stat("v2_notpreg_vaccines_BL", 1)
table_data[["When you are pregnant/lactating, do you receive all recommended vaccines?"]] <- getT1Stat("v3_notpreg_vaccines_BL",1)
table_data[["Thinking back to before COVID-19, how much did you value vaccines? New"]] <- getT1Stat("v4_before_value_BL", 1)
table_data[["Today, how much do you value vaccines? New"]] <- getT1Stat("v5_today_value_BL", 1)
table_data[["How you received the influenza vaccine in the past 5 years?"]] <- getT1Stat("v6_flu_BL", 1)

table_data[["Where do you get your healthcare information Physician?"]] <- getT1Stat("v7_healthinfo___1_BL", 1)
table_data[["Where do you get your healthcare information prenatal care provider?"]] <- getT1Stat("v7_healthinfo___2_BL", 1)
table_data[["Where do you get your healthcare information other HCP?"]] <- getT1Stat("v7_healthinfo___3_BL", 1)
table_data[["Where do you get your healthcare information FamFri?"]] <- getT1Stat("v7_healthinfo___4_BL", 1)
table_data[["Where do you get your healthcare information social media?"]] <- getT1Stat("v7_healthinfo___5_BL", 1)
table_data[["Where do you get your healthcare information the news?"]] <- getT1Stat("v7_healthinfo___6_BL", 1)
table_data[["Where do you get your healthcare information google?"]] <- getT1Stat("v7_healthinfo___7_BL", 1)
table_data[["Where do you get your healthcare information government?"]] <- getT1Stat("v7_healthinfo___8_BL", 1)

table_data[["The health information I receive from my physician is... (higher value = more reliable"]] <- getT1Stat("famphys", 1)
table_data[["The health information I receive from my prenatal care provider is..."]]<- getT1Stat("prenatal", 1)
table_data[["The health information I receive from my other healthcare care provider is..."]]<- getT1Stat("other.hcp", 1)
table_data[["The health information I receive from my family and friends is..."]]<- getT1Stat("famfri", 1)
table_data[["The health information I receive from social media is......"]]<- getT1Stat("social", 1)
table_data[["The health information I receive from traditional news is..."]]<- getT1Stat("news", 1)
table_data[["The health information I receive from google search is ..."]]<- getT1Stat("google", 1)
table_data[["The health information I receive from the government is..."]]<- getT1Stat("gov", 1)


table_data[["WHO scale: Vaccine Risks"]] <- getT1Stat("VHS_risk", 1)
table_data[["Attitudes to COVID-19 Vaccine"]] <- getT1Stat("ATT", 1)
table_data[["Perceived Behavioural Control"]] <- getT1Stat("PBC", 1)
table_data[["Direct Social Norms: Most people who are important to me would think that I should receive the COVID-19 vaccine while pregnant/lactating"]] <- getT1Stat("v29_important_BL", 1)
table_data[["Direct Social Norms: I feel under social pressure to receive a COVID-19 vaccine while pregnant/lactating"]] <- getT1Stat("v30_socialpressure_BL", 1)
table_data[["Indirect Social Norms: Provincial Health Officer"]] <- getT1Stat("SNI.PHO", 1)
table_data[["Indirect Social Norms: Partner"]] <- getT1Stat("SNI.PART", 1)
table_data[["Indirect Social Norms: Friends & Family"]] <- getT1Stat("SNI.FRI", 1)
table_data[["Indirect Social Norms: Prenatal care provider"]] <- getT1Stat("SNI.PRE", 1)
table_data[["Indirect Social Norms: Total"]] <- getT1Stat("SNI.TOT", 1)
summary(table_data)


# Now merge everything into a matrix
# and create the rgroup & n.rgroup variabels
rgroup <- c()
n.rgroup <- c()
output_data <- NULL
for (varlabel in names(table_data)) {
  output_data <- rbind(output_data, 
                       table_data[[varlabel]])
  rgroup <- c(rgroup, 
              varlabel)
  n.rgroup <- c(n.rgroup, 
                nrow(table_data[[varlabel]]))
}

# Add a column spanner for the columns
cgroup <- c("", "Likely to receive COVID-19 vaccine")
n.cgroup <- c(1, 3)
colnames(output_data) <- gsub("[ ]*Likely to receive COVID-19 vaccine", "", colnames(output_data))

htmlTable::htmlTable(output_data, align = "rrrr",
                     rgroup = rgroup, n.rgroup = n.rgroup, 
                     css.rgroup.sep = "", 
                     cgroup = cgroup,
                     n.cgroup = n.cgroup,
                     rowlabel = "", 
                     caption = "", 
                     ctable = TRUE)  %>% htmltools::html_print()



# ## Methods and Results (From Arianne)
# 
# Likelihood of receiving vaccine was assessed using the question: As COVID-19 vaccines have been recommended for pregnant and lactating women and people, how likely are you to receive it?
#   
#   Answers of 1-3 (very unlikely to neutral) were considered as not likely Answers of 4-6 (somewhat likely, very likely, and already received) were considered as likely
# 
# There were 3,418 participants who completed the intention question and are included in the summaries below. However, the breakdown of answers indicate that the vast majority had already received at least one dose (option 6) at the time of the survey:
#   
#   `r kbl(table(data$v1_dose1_likely_BL, useNA = "ifany"))`
# 
# <br>
#   
#   Of the demographic variables, education was significantly associated with intention to vaccinate, with those with graduate degrees being less likely to not intend to get the vaccine (8%) compared to Bachelor's degree and less (\~11%). Or more likely to intend/have received the vaccine (92% vs \~89%). All of the vaccine theory of planned behaviour variables, except perceived behavioural control, were significantly associated with intention to vaccinate. Of note, perceived behavioural control was only calculated for those who had not yet received a vaccine and so the sample size was much smaller.
# 

htmlTable::htmlTable(output_data, align = "rrrr",
                     rgroup = rgroup, n.rgroup = n.rgroup, 
                     css.rgroup.sep = "", 
                     cgroup = cgroup,
                     n.cgroup = n.cgroup,
                     rowlabel = "", 
                     caption = "", 
                     ctable = TRUE) 




mod1 <- glm(likely ~ bl23_education_BL, data = data, family = binomial)
summary(mod1)
drop1(mod1, test = "Chi")

edu.ph <- emmeans(mod1, specs = "bl23_education_BL", contr = "pairwise")
summary(edu.ph)

mod2 <- glm(likely ~ VHS_risk, data = data, family = binomial)
summary(mod2)
drop1(mod2, test = "Chi")

mod3 <- glm(likely ~ ATT, data = data, family = binomial)
summary(mod3)
drop1(mod3, test = "Chi")

mod4 <- glm(likely ~ v29_important_BL, data = data, family = binomial)
summary(mod4)
drop1(mod4, test = "Chi")

mod5 <- glm(likely ~ v30_socialpressure_BL, data = data, family = binomial)
summary(mod5)
drop1(mod5, test = "Chi")

mod6 <- glm(likely ~ SNI.PHO, data = data, family = binomial)
summary(mod6)
drop1(mod6, test = "Chi")

mod7 <- glm(likely ~ SNI.PART, data = data, family = binomial)
summary(mod7)
drop1(mod7, test = "Chi")

mod8 <- glm(likely ~ SNI.FRI, data = data, family = binomial)
summary(mod8)
drop1(mod8, test = "Chi")

mod9 <- glm(likely ~ SNI.PRE, data = data, family = binomial)
summary(mod9)
drop1(mod9, test = "Chi")

mod10 <- glm(likely~ employ_cat, data=data, family=binomial)
summary(mod10)
drop1(mod10, test="Chi")

mod11 <- glm(likely~SNI.TOT, data = data, family=binomial)
summary(mod11)
drop1(mod11, test="Chi")

mod12 <- glm(likely~v2_notpreg_vaccines_BL, data=data, family = binomial)
summary(mod12)
drop1(mod12, test="Chi")

mod13 <- glm(likely~v3_notpreg_vaccines_BL, data = data, family = binomial)
summary(mod13)
drop1(mod13, test= "Chi")

mod14 <- glm(likely~v4_before_value_BL, data = data, family = binomial)
summary(mod14)
drop1(mod14, test = "Chi")

mod15 <- glm(likely ~ v5_today_value_BL, data = data, family = binomial)
summary(mod15)
drop1(mod15, test = "Chi")

mod16 <- glm(likely~v6_flu_BL, data = data, family = binomial)
summary(mod16)
drop1(mod16, test= "Chi")

mod17 <- glm(likely~famphys, data = data, family = binomial)
summary(mod17)

mod18 <- glm(likely~prenatal, data = data, family = binomial)
summary(mod18)

mod19 <- glm(likely~other.hcp, data = data, family = binomial)
summary(mod19)

mod20 <- glm(likely~famfri, data = data, family = binomial)
summary(mod20)


mod21 <- glm(likely~social, data = data, family = binomial)
summary(mod21)

mod22 <- glm(likely~news, data = data, family = binomial)
summary(mod22)

mod23 <- glm(likely~gov, data = data, family = binomial)
summary(mod23)

mod24 <- glm(likely~ gravid_cat, data = data, family = binomial)
summary(mod24)

mod_all <- glm(likely ~ bl23_education_BL + employ_cat + VHS_risk + ATT + v29_important_BL + v30_socialpressure_BL + SNI.PHO + SNI.PART + SNI.FRI + SNI.PRE, data = data, family = binomial)

mod_all_IF <- glm(likely ~ bl23_education_BL + employ_cat + VHS_risk + ATT + v29_important_BL + v30_socialpressure_BL + SNI.PHO + SNI.PART + SNI.FRI + SNI.PRE + v2_notpreg_vaccines_BL + v3_notpreg_vaccines_BL + v4_before_value_BL +v5_today_value_BL + v6_flu_BL , data = data, family = binomial)

summary(mod_all)


# <br>
#   
#   I ran logistic regressions of all of the singificant bivariable relationships and included all into a multivariable model. For education, post-hoc pairwise Tukey's tests confirm that the only pairwise significant difference was between graduate degree and bachelor's degree (adjusted p = 0.025), likely due to smaller sample size in the other education categories.


### check for liz to see how many were pregnant at time of filling out the covered survey:

dim(data)
dim(data_use)

data_check <- left_join(data,data_use, by = "record_id_BL")

dim(data_check)

data_check %>% group_by(bl1_currently_preg_BL) %>% summarise(n=n())

