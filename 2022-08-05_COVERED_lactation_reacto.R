## ----setup, include=FALSE------------------------------------------------------------------------------
rm(list = ls())
knitr::opts_chunk$set(echo = FALSE, include = TRUE)
`%notin%` <- Negate(`%in%`)

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



theme_set(
  theme(text = element_text(size = 14))
)



## ----data----------------------------------------------------------------------------------------------
setwd("/Users/selagrays/dev/covered")
data <- read.csv("MainSurveyDatabaseCa-VaccineAttitudesIF_DATA_2024-02-05_1739.csv",header = TRUE)

## ----data cleaning and inclusion, include = F----------------------------------------------------------
# exclude the non living in Canada
describeFactors(data$redcap_event_name)
#describeFactors(data$covered_econsent_complete)
describeFactors(data$bl16_country_res)
describeFactors(data$bl16_country_res[which(data$redcap_event_name == "baseline_arm_1")])

describeFactors(data$bl16_country_res[which(data$withdraw_answer == 1)])

data <- data[-which(data$withdraw_answer == 1), ]

ids.can <- data$record_id[which(data$bl16_country_res == 1)]
ids.can.preg <- data$record_id[which(data$bl16_country_res == 1 & data$vaccine_attitudes_survey_timestamp != "" &
                                       data$i1_dob1 != "" &
                                       #data$bl1_currently_preg == "yes" &
                                       data$i1_dob1 > data$vaccine_attitudes_survey_timestamp
)]

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

vacc.dates <-  data$do3_date

length(unique(data$record_id))

data_bl <- data[which(data$redcap_event_name == "baseline_arm_1" & data$record_id %in% ids.can.EDD), ]
data_2mo <- data[which(data$redcap_event_name == "followup_2_months_arm_1" & data$record_id %in% ids.can.EDD), ]
data_4mo <- data[which(data$redcap_event_name == "followup_4_months_arm_1" & data$record_id %in% ids.can.EDD), ]
data_6mo <- data[which(data$redcap_event_name == "followup_6_months_arm_1" & data$record_id %in% ids.can.EDD), ]
data_8mo <- data[which(data$redcap_event_name == "followup_8_months_arm_1" & data$record_id %in% ids.can.EDD), ]
data_10mo <- data[which(data$redcap_event_name == "followup_10_months_arm_1" & data$record_id %in% ids.can.EDD), ]
data_14mo <- data[which(data$redcap_event_name == "followup_14_months_arm_1" & data$record_id %in% ids.can.EDD), ]

# followup_10_months_arm_1 "3,050 (10.5%)"
# followup_14_months_arm_1 "413 (1.4%)"   
# followup_2_months_arm_1  "4,993 (17.2%)"
# followup_4_months_arm_1  "4,505 (15.6%)"
# followup_6_months_arm_1  "4,196 (14.5%)"
# followup_8_months_arm_1  "3,839 (13.3%)"

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

data_bl <- janitor::remove_empty(data_bl)
data_2mo <- janitor::remove_empty(data_2mo)
data_4mo <- janitor::remove_empty(data_4mo)
data_6mo <- janitor::remove_empty(data_6mo)
data_8mo <- janitor::remove_empty(data_8mo)
data_10mo <- janitor::remove_empty(data_10mo)
data_14mo <- janitor::remove_empty(data_14mo)

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

data <- data_use



## ----starting again with vaccine timing----------------------------------------------------------------
# 
# data_use <- data_use %>% 
#   mutate(
#     dose1_T = case_when(
#       bl4_dose1_BL == 0 & bl1_currently_preg_BL == 1 ~ "No vaccine pregnant",
#       bl4_dose1_BL == 0 & bl1_currently_preg_BL == 0 ~ "No vaccine NOT pregnant",
#       do2_timing_BL == 2 | do2_timing2___2_BL == 1 ~ "D1 during preg",
#       do2_timing_BL == 1 | do2_timing2___1_BL == 1 ~ "D1 before preg",
#       do2_timing_BL == 3 | do2_timing2___3_BL == 1 ~ "D1 post-partum/BF",
#       do2_timing_BL == 4 | do2_timing2___4_BL == 1 ~ "D1 post-partum/Not BF"
#     )
#   )
# 
# describeFactors(data_use$dose1_T)
# # # View(data_use[which(data_use$dose1_T == "D1 before preg"), c("record_id", "dose1_preg_ga",  "bl4_dose1", "bl1_currently_preg", "bl5_dose1_timing", "do2_timing", "bl5_dose1_timing2___1", "do2_timing2___1", "bl5_dose1_timing2___2", "do2_timing2___2", "bl5_dose1_timing2___3", "do2_timing2___3", "bl5_dose1_timing2___4", "do2_timing2___4", "do2_timing_other", "bl5_dose1_timing_other")])
# 
# data_use <- data_use %>% 
#    mutate(
#     dose2_T = case_when(
#       dose1_T == "No vaccine NOT pregnant" ~ "No vaccine NOT pregnant",
#       dose1_T == "No vaccine pregnant" ~ "No vaccine pregnant",
#       # bl8_dose2 == 0 & bl1_currently_preg == 1 ~ "No vaccine pregnant",
#       # bl8_dose2 == 0 & bl1_currently_preg == 0 ~ "No vaccine NOT pregnant",
#       bl8_dose2_BL == 0 ~ "No D2",
#       dt2_timing_BL == 2 | (dt2_timing2___2_BL == 1) ~ "D2 during preg",
#       dt2_timing_BL == 1 | (dt2_timing2___1_BL == 1) ~ "D2 before preg",
#       dt2_timing_BL == 3 | dt2_timing2___3_BL == 1 ~ "D2 post-partum/BF",
#       dt2_timing_BL == 4 | dt2_timing2___4_BL == 1 ~ "D2 post-partum/Not BF"
#     )
#   )
# 
# describeFactors(data_use$dose2_T)
# 
# tab_xtab(data_use$dose1_T, data_use$dose2_T)
# 
# # if we just stick to the baseline data, how many had pregnancy outcomes?
# ## pick out the clear ones
# data_use <- data_use %>% 
#   mutate(
#     include = case_when(
#       dose1_T == "D1 during preg" & dose2_T == "D2 during preg" ~ "include",
#       dose1_T == "D1 during preg" & dose2_T == "D2 post-partum/BF" ~ "include",
#       dose1_T == "D1 during preg" & dose2_T == "D2 post-partum/Not BF" ~ "include",
#       dose1_T == "D1 during preg" & dose2_T == "No D2" ~ "include",
#       dose1_T == "D1 post-partum/BF" & dose2_T == "D2 post-partum/BF" ~ "include",
#       dose1_T == "D1 post-partum/BF" & dose2_T == "D2 post-partum/Not BF" ~ "include",
#       dose1_T == "D1 post-partum/BF" & dose2_T == "No D2" ~ "include",
#       dose1_T == "D1 post-partum/Not BF" & dose2_T == "D2 post-partum/Not BF" ~ "include",
#       dose1_T == "D1 post-partum/Not BF" & dose2_T == "D2 post-partum/BF" ~ "include",
#       dose1_T == "D1 post-partum/Not BF" & dose2_T == "No D2" ~ "include",
#       dose1_T == "No vaccine NOT pregnant" | dose1_T == "No vaccine pregnant" ~ "include",
#       TRUE ~ "exclude"
#     )
#   )
# 
# describeFactors(data_use$include)
# # exclude "953 (16.8%)"  
# # include "4,735 (83.2%)"
# 
# describeFactors(data_use$i1_dob1_BL)
# data_use$BABY_HAS_DOB_BL <- ifelse(is.na(data_use$i1_dob1_BL), "No", "Yes")
# 
# tab_xtab(data_use$BABY_HAS_DOB_BL, data_use$include)
# 
# tab_xtab(data_use$dose1_T[which(data_use$BABY_HAS_DOB_BL == "Yes")], data_use$dose2_T[which(data_use$BABY_HAS_DOB_BL == "Yes")])
# 
# 
# data_use <- data_use %>% 
#    mutate(
#     dose3_T = case_when(
#       dose1_T == "No vaccine NOT pregnant" ~ "No vaccine NOT pregnant",
#       dose1_T == "No vaccine pregnant" ~ "No vaccine pregnant",
#       bl8_dose2_BL == 0 ~ "No D2 or D3",
#       dt2_timing2_vb___2_BL == 1 ~ "D3 during preg",
#       dt2_timing2_vb___1_BL == 1 ~ "D3 before preg",
#       dt2_timing2_vb___3_BL == 1 ~ "D3 post-partum/BF",
#       dt2_timing2_vb___4_BL == 1 ~ "D3 post-partum/Not BF"
#     )
#   )
# 
# describeFactors(data_use$dose3_T)
# 
# tab_xtab(data_use$dose1_T, data_use$dose3_T)
# tab_xtab(data_use$dose1_T, data_use$dose2_T)
# 
# #########
# #### DOSE SPECIFIC SURVEYS WERE ONLY ASKED ONCE SO I THINK WE CAN COMBINE THEM ACROSS TIME POINTS FOR EACH PERSON
# # IF DOSE 1 WAS BEFORE PREG AT BASELINE THEN EXCLUDE THEM
# # IF D1 DURING PREG AT BASELINE THEN Exclude THEM
# # We want just first and subsequent doses during lactation, and not doses prior
data <- data %>%
  mutate(include_BL = case_when(
    #    dose1_T == "D1 before preg" ~ "exclude",
    data_use$record_id %in% ids.can.EDD.vacc ~ "include D1&2&3 in preg",
    data_use$record_id %in% ids.can.EDD.vacc ~ "D1&2 in preg",
    data_use$record_id %in% ids.can.EDD.vacc ~ "include D1 in preg",
    #    grepl("post", dose1_T) & dose2_T %in% c("D2 before preg", "D2 during preg") & dose3_T %in% c("D3 before preg", "D3 during preg") ~ "exclude",
    #    grepl("post", dose1_T) & dose2_T %in% c("D2 before preg", "D2 during preg") ~ "exclude",
    data_use$record_id %in% ids.can.EDD.vacc ~ "include D1 pp",
    #    dose1_T == "No vaccine NOT pregnant" ~ "exclude not preg",
    data_use$record_id %notin% ids.can.EDD.vacc  ~ "include no vax BL"
  ))
describeFactors(data$include_BL)
# # exclude           "3,767 (66.2%)"
# # exclude not preg  "59 (1.0%)"    
# # include D1 pp     "1,645 (28.9%)"
# # include no vax BL "89 (1.6%)"    
# # Missing           "128 (2.3%)" 
# 
# ### then we need to look at self-reported timing for subsequent follow up and doses
# 
# data_use <- data_use %>% 
#   mutate(
#     dose1_T_2mo = case_when(
#       do2_timing2___2_2mo == 1 ~ "D1 during preg",
#       do2_timing2___1_2mo == 1 ~ "D1 before preg",
#       do2_timing2___3_2mo == 1 ~ "D1 post-partum/BF",
#       do2_timing2___4_2mo == 1 ~ "D1 post-partum/Not BF",
#       TRUE ~ NA_character_
#     )
#   )
# 
# describeFactors(data_use$dose1_T_2mo)
# tab_xtab(data_use$include_BL, data_use$dose1_T_2mo)
# 
# data_use <- data_use %>% 
#    mutate(
#     dose2_T_2mo = case_when(
#       dt2_timing2___2_2mo == 1 ~ "D2 during preg",
#       dt2_timing2___1_2mo == 1 ~ "D2 before preg",
#       dt2_timing2___3_2mo == 1 ~ "D2 post-partum/BF",
#       dt2_timing2___4_2mo == 1 ~ "D2 post-partum/Not BF",
#       TRUE ~ NA_character_
#     )
#   )
# 
# describeFactors(data_use$dose2_T_2mo)
# tab_xtab(data_use$include_BL, data_use$dose2_T_2mo)
# 
# ## nothing surprising here
# data_use <- data_use %>% 
#    mutate(
#     dose3_T_2mo = case_when(
#       dt2_timing2_vb___2_2mo == 1 ~ "D3 during preg",
#       dt2_timing2_vb___1_2mo == 1 ~ "D3 before preg",
#       dt2_timing2_vb___3_2mo == 1 ~ "D3 post-partum/BF",
#       dt2_timing2_vb___4_2mo == 1 ~ "D3 post-partum/Not BF",
#       TRUE ~ NA_character_
#     )
#   )
# 
# describeFactors(data_use$dose3_T_2mo)
# tab_xtab(data_use$include_BL, data_use$dose3_T_2mo) # 8 will need to be excluded as they indicated D3 before preg with D1 in pp
# 
# 
# 
# 
###### Thinking about this in a different way. Can we determine when someone has entered data into the pregnancy outcomes survey?
# View(data_use[, c("record_id", "pregnancy_outcomes_timestamp_BL", "pregnancy_outcomes_timestamp_2mo", "pregnancy_outcomes_timestamp_4mo", "pregnancy_outcomes_timestamp_6mo", "pregnancy_outcomes_timestamp_8mo", "pregnancy_outcomes_timestamp_10mo", "pregnancy_outcomes_timestamp_14mo")])

data$num_preg_outcomes <- rowSums(!is.na(data[, c("demographic_health_pregnancy_survey_timestamp_BL", "pregnancy_outcomes_timestamp_2mo",
                                                  "pregnancy_outcomes_timestamp_4mo", "pregnancy_outcomes_timestamp_6mo",
                                                  "pregnancy_outcomes_timestamp_8mo", "pregnancy_outcomes_timestamp_10mo",
                                                  "pregnancy_outcomes_timestamp_14mo")]))
## only 1 or 0, this might help a lot

# write a for loop to figure out which of the survey times contains the pregnancy outcome information

data$which_time_preg_out <- c()

for(i in 1:nrow(data)){
  xx <- data[i, c("demographic_health_pregnancy_survey_timestamp_BL", "pregnancy_outcomes_timestamp_2mo",
                  "pregnancy_outcomes_timestamp_4mo", "pregnancy_outcomes_timestamp_6mo",
                  "pregnancy_outcomes_timestamp_8mo", "pregnancy_outcomes_timestamp_10mo",
                  "pregnancy_outcomes_timestamp_14mo")]
  print(i)
  # print(which(!is.na(xx)))
  
  if(all(is.na(xx))) {
    data$which_time_preg_out[i] = "No preg outcome"
  }
  
  else if(length(which(!is.na(xx))) > 1){
    data$which_time_preg_out[i] = which(!is.na(xx))[1]
  }
  
  else {
    data$which_time_preg_out[i] = which(!is.na(xx))
  }

}

# View(data_use[, c("record_id", "pregnancy_outcomes_timestamp_BL", "pregnancy_outcomes_timestamp_2mo", "pregnancy_outcomes_timestamp_4mo", "pregnancy_outcomes_timestamp_6mo", "pregnancy_outcomes_timestamp_8mo", "pregnancy_outcomes_timestamp_10mo", "pregnancy_outcomes_timestamp_14mo", "which_time_preg_out")])

describeFactors(data$which_time_preg_out)
# 1               "2,865 (50.4%)"
# 2               "860 (15.1%)"  
# 3               "706 (12.4%)"  
# 4               "536 (9.4%)"   
# 5               "199 (3.5%)"   
# 6               "22 (0.4%)"    
# 7               "2 (0.0%)"     
# No preg outcome "498 (8.8%)" 

### this variable lets us choose which of the survey times need to be considered for deciding on the vaccination status and timing in pregnancy

tab_xtab(data$which_time_preg_out, data$include_BL)


#### then I think we can paste all the results for doses together?
# data <- data %>% 
#   mutate(
#     dose1_T_4mo = case_when(
#       do2_timing2___2_4mo == 1 ~ "D1 during preg",
#       do2_timing2___1_4mo == 1 ~ "D1 before preg",
#       do2_timing2___3_4mo == 1 ~ "D1 post-partum/BF",
#       do2_timing2___4_4mo == 1 ~ "D1 post-partum/Not BF",
#       TRUE ~ NA_character_
#     )
#   )
# 
# describeFactors(data_use$dose1_T_4mo)
# tab_xtab(data_use$include_BL, data_use$dose1_T_4mo)
# 
# data_use <- data_use %>% 
#    mutate(
#     dose2_T_4mo = case_when(
#       dt2_timing2___2_4mo == 1 ~ "D2 during preg",
#       dt2_timing2___1_4mo == 1 ~ "D2 before preg",
#       dt2_timing2___3_4mo == 1 ~ "D2 post-partum/BF",
#       dt2_timing2___4_4mo == 1 ~ "D2 post-partum/Not BF",
#       TRUE ~ NA_character_
#     )
#   )
# 
# describeFactors(data_use$dose2_T_4mo)
# tab_xtab(data_use$include_BL, data_use$dose2_T_4mo)
# 
# data_use <- data_use %>% 
#    mutate(
#     dose3_T_4mo = case_when(
#       dt2_timing2_vb___2_4mo == 1 ~ "D3 during preg",
#       dt2_timing2_vb___1_4mo == 1 ~ "D3 before preg",
#       dt2_timing2_vb___3_4mo == 1 ~ "D3 post-partum/BF",
#       dt2_timing2_vb___4_4mo == 1 ~ "D3 post-partum/Not BF",
#       TRUE ~ NA_character_
#     )
#   )
# 
# describeFactors(data_use$dose3_T_4mo)
# tab_xtab(data_use$include_BL, data_use$dose3_T_4mo)
# 
# ### 6 mo
# data_use <- data_use %>% 
#   mutate(
#     dose1_T_6mo = case_when(
#       do2_timing2___2_6mo == 1 ~ "D1 during preg",
#       do2_timing2___1_6mo == 1 ~ "D1 before preg",
#       do2_timing2___3_6mo == 1 ~ "D1 post-partum/BF",
#       do2_timing2___4_6mo == 1 ~ "D1 post-partum/Not BF",
#       TRUE ~ NA_character_
#     )
#   )
# 
# describeFactors(data_use$dose1_T_6mo)
# # tab_xtab(data_use$include_BL, data_use$dose1_T_6mo)
# 
# data_use <- data_use %>% 
#    mutate(
#     dose2_T_6mo = case_when(
#       dt2_timing2___2_6mo == 1 ~ "D2 during preg",
#       dt2_timing2___1_6mo == 1 ~ "D2 before preg",
#       dt2_timing2___3_6mo == 1 ~ "D2 post-partum/BF",
#       dt2_timing2___4_6mo == 1 ~ "D2 post-partum/Not BF",
#       TRUE ~ NA_character_
#     )
#   )
# 
# describeFactors(data_use$dose2_T_6mo)
# # tab_xtab(data_use$include_BL, data_use$dose2_T_6mo)
# 
# data_use <- data_use %>% 
#    mutate(
#     dose3_T_6mo = case_when(
#       dt2_timing2_vb___2_6mo == 1 ~ "D3 during preg",
#       dt2_timing2_vb___1_6mo == 1 ~ "D3 before preg",
#       dt2_timing2_vb___3_6mo == 1 ~ "D3 post-partum/BF",
#       dt2_timing2_vb___4_6mo == 1 ~ "D3 post-partum/Not BF",
#       TRUE ~ NA_character_
#     )
#   )
# 
# describeFactors(data_use$dose3_T_6mo)
# # tab_xtab(data_use$include_BL, data_use$dose3_T_6mo)
# 
# 
# ### 8 mo
# data_use <- data_use %>% 
#   mutate(
#     dose1_T_8mo = case_when(
#       do2_timing2___2_8mo == 1 ~ "D1 during preg",
#       do2_timing2___1_8mo == 1 ~ "D1 before preg",
#       do2_timing2___3_8mo == 1 ~ "D1 post-partum/BF",
#       do2_timing2___4_8mo == 1 ~ "D1 post-partum/Not BF",
#       TRUE ~ NA_character_
#     )
#   )
# 
# describeFactors(data_use$dose1_T_8mo)
# # tab_xtab(data_use$include_BL, data_use$dose1_T_8mo)
# 
# data_use <- data_use %>% 
#    mutate(
#     dose2_T_8mo = case_when(
#       dt2_timing2___2_8mo == 1 ~ "D2 during preg",
#       dt2_timing2___1_8mo == 1 ~ "D2 before preg",
#       dt2_timing2___3_8mo == 1 ~ "D2 post-partum/BF",
#       dt2_timing2___4_8mo == 1 ~ "D2 post-partum/Not BF",
#       TRUE ~ NA_character_
#     )
#   )
# 
# describeFactors(data_use$dose2_T_8mo)
# # tab_xtab(data_use$include_BL, data_use$dose2_T_8mo)
# 
# data_use <- data_use %>% 
#    mutate(
#     dose3_T_8mo = case_when(
#       dt2_timing2_vb___2_8mo == 1 ~ "D3 during preg",
#       dt2_timing2_vb___1_8mo == 1 ~ "D3 before preg",
#       dt2_timing2_vb___3_8mo == 1 ~ "D3 post-partum/BF",
#       dt2_timing2_vb___4_8mo == 1 ~ "D3 post-partum/Not BF",
#       TRUE ~ NA_character_
#     )
#   )
# 
# describeFactors(data_use$dose3_T_8mo)
# # tab_xtab(data_use$include_BL, data_use$dose3_T_8mo)
# 
# 
# 
# ### 10 mo
# data_use <- data_use %>% 
#   mutate(
#     dose1_T_10mo = case_when(
#       do2_timing2___2_10mo == 1 ~ "D1 during preg",
#       do2_timing2___1_10mo == 1 ~ "D1 before preg",
#       do2_timing2___3_10mo == 1 ~ "D1 post-partum/BF",
#       do2_timing2___4_10mo == 1 ~ "D1 post-partum/Not BF",
#       TRUE ~ NA_character_
#     )
#   )
# 
# describeFactors(data_use$dose1_T_10mo)
# # tab_xtab(data_use$include_BL, data_use$dose1_T_10mo)
# 
# data_use <- data_use %>% 
#    mutate(
#     dose2_T_10mo = case_when(
#       dt2_timing2___2_10mo == 1 ~ "D2 during preg",
#       dt2_timing2___1_10mo == 1 ~ "D2 before preg",
#       dt2_timing2___3_10mo == 1 ~ "D2 post-partum/BF",
#       dt2_timing2___4_10mo == 1 ~ "D2 post-partum/Not BF",
#       TRUE ~ NA_character_
#     )
#   )
# 
# describeFactors(data_use$dose2_T_10mo)
# # tab_xtab(data_use$include_BL, data_use$dose2_T_10mo)
# 
# data_use <- data_use %>% 
#    mutate(
#     dose3_T_10mo = case_when(
#       dt2_timing2_vb___2_10mo == 1 ~ "D3 during preg",
#       dt2_timing2_vb___1_10mo == 1 ~ "D3 before preg",
#       dt2_timing2_vb___3_10mo == 1 ~ "D3 post-partum/BF",
#       dt2_timing2_vb___4_10mo == 1 ~ "D3 post-partum/Not BF",
#       TRUE ~ NA_character_
#     )
#   )
# 
# describeFactors(data_use$dose3_T_10mo)
# # tab_xtab(data_use$include_BL, data_use$dose3_T_10mo)
# 
# 
# ### 14 mo
# data_use <- data_use %>% 
#   mutate(
#     dose1_T_14mo = case_when(
#       do2_timing2___2_14mo == 1 ~ "D1 during preg",
#       do2_timing2___1_14mo == 1 ~ "D1 before preg",
#       do2_timing2___3_14mo == 1 ~ "D1 post-partum/BF",
#       do2_timing2___4_14mo == 1 ~ "D1 post-partum/Not BF",
#       TRUE ~ NA_character_
#     )
#   )
# 
# describeFactors(data_use$dose1_T_14mo)
# # tab_xtab(data_use$include_BL, data_use$dose1_T_14mo)
# 
# data_use <- data_use %>% 
#    mutate(
#     dose2_T_14mo = case_when(
#       dt2_timing2___2_14mo == 1 ~ "D2 during preg",
#       dt2_timing2___1_14mo == 1 ~ "D2 before preg",
#       dt2_timing2___3_14mo == 1 ~ "D2 post-partum/BF",
#       dt2_timing2___4_14mo == 1 ~ "D2 post-partum/Not BF",
#       TRUE ~ NA_character_
#     )
#   )
# 
# describeFactors(data_use$dose2_T_14mo)
# # tab_xtab(data_use$include_BL, data_use$dose2_T_14mo)
# 
# data_use <- data_use %>% 
#    mutate(
#     dose3_T_14mo = case_when(
#       dt2_timing2_vb___2_14mo == 1 ~ "D3 during preg",
#       dt2_timing2_vb___1_14mo == 1 ~ "D3 before preg",
#       dt2_timing2_vb___3_14mo == 1 ~ "D3 post-partum/BF",
#       dt2_timing2_vb___4_14mo == 1 ~ "D3 post-partum/Not BF",
#       TRUE ~ NA_character_
#     )
#   )
# 
# describeFactors(data_use$dose3_T_14mo)
# # tab_xtab(data_use$include_BL, data_use$dose3_T_14mo)

###### exclusions
# 1. exclude everyone who does not have a pregnancy outcomes survey filled in
# data_ex1 <- data_use[-which(data_use$which_time_preg_out == "No preg outcome"), ] # 5688 to 5190
# describeFactors(data_ex1$include_BL)

data_ex1 <- data[which(data$which_time_preg_out == 1), ] # 5688 to 5190
describeFactors(data_ex1$include_BL)
# exclude           "3,361 (64.8%)"
# exclude not preg  "57 (1.1%)"    
# include D1 pp     "1,625 (31.3%)"
# include no vax BL "75 (1.4%)"    
# Missing           "72 (1.4%)"

# 2. exclude the ones that should be excluded from using BL information (eg vaxx before/during preg or not pregnant)
data_ex2 <- data_ex1[which(data_ex1$include_BL %in% c("include D1&2&3 in preg", "include no vax BL")),] # 4547

# data_ex2 <- data_ex1[-which(data_ex1$include_BL %in% c("exclude", "exclude not preg") | is.na(data_ex1$include_BL)), ] # 1700
# 
# data_ex2$all_dose1_info <- paste(data_ex2$dose1_T, data_ex2$dose1_T_2mo,
#                                  data_ex2$dose1_T_4mo, data_ex2$dose1_T_6mo,
#                                  data_ex2$dose1_T_8mo, data_ex2$dose1_T_10mo,
#                                  data_ex2$dose1_T_14mo, sep = "_")
# 
# data_ex2$all_dose1_dates <- paste(data_ex2$do3_date_BL, data_ex2$do3_date_2mo,
#                                  data_ex2$do3_date_4mo, data_ex2$do3_date_6mo,
#                                  data_ex2$do3_date_8mo, data_ex2$do3_date_10mo,
#                                  data_ex2$do3_date_14mo, sep = "_")
# 
# data_ex2$all_dose1_dates <- gsub("NA_", "", data_ex2$all_dose1_dates)
# data_ex2$all_dose1_dates <- gsub("__", "", data_ex2$all_dose1_dates)
# data_ex2$all_dose1_dates <- gsub("_", NA_character_, data_ex2$all_dose1_dates)
# 
data_ex2$all_baby_dob <- paste(data_ex2$i1_dob1_BL, data_ex2$i1_dob1_2mo,
                                 data_ex2$i1_dob1_4mo, data_ex2$i1_dob1_6mo,
                                 data_ex2$i1_dob1_8mo, data_ex2$i1_dob1_10mo,
                                 data_ex2$i1_dob1_14mo, sep = "_")

data_ex2$all_baby_dob <- gsub("NA_", "", data_ex2$all_baby_dob)
data_ex2$all_baby_dob <- gsub("_NA", "", data_ex2$all_baby_dob)
data_ex2$all_baby_dob <- gsub("NA", NA_character_, data_ex2$all_baby_dob)
#   
# 
# data_ex2$all_dose2_info <- paste(data_ex2$dose2_T, data_ex2$dose2_T_2mo,
#                                  data_ex2$dose2_T_4mo, data_ex2$dose2_T_6mo,
#                                  data_ex2$dose2_T_8mo, data_ex2$dose2_T_10mo,
#                                  data_ex2$dose2_T_14mo, sep = "_")
# 
# data_ex2$all_dose2_dates <- paste(data_ex2$dt3_date_BL, data_ex2$dt3_date_2mo,
#                                  data_ex2$dt3_date_4mo, data_ex2$dt3_date_6mo,
#                                  data_ex2$dt3_date_8mo, data_ex2$dt3_date_10mo,
#                                  data_ex2$dt3_date_14mo, sep = "_")
# 
# data_ex2$all_dose2_dates <- gsub("NA_", "", data_ex2$all_dose2_dates)
# data_ex2$all_dose2_dates <- gsub("__", "", data_ex2$all_dose2_dates)
# data_ex2$all_dose2_dates <- gsub("_", NA_character_, data_ex2$all_dose2_dates)
# 
# data_ex2$all_dose3_info <- paste(data_ex2$dose3_T, data_ex2$dose3_T_2mo,
#                                  data_ex2$dose3_T_4mo, data_ex2$dose3_T_6mo,
#                                  data_ex2$dose3_T_8mo, data_ex2$dose3_T_10mo,
#                                  data_ex2$dose3_T_14mo, sep = "_")
# 
# data_ex2$all_dose3_dates <- paste(data_ex2$dt3_date_vb_BL, data_ex2$dt3_date_vb_2mo,
#                                  data_ex2$dt3_date_vb_4mo, data_ex2$dt3_date_vb_6mo,
#                                  data_ex2$dt3_date_vb_8mo, data_ex2$dt3_date_vb_10mo,
#                                  data_ex2$dt3_date_vb_14mo, sep = "_")
# 
# data_ex2$all_dose3_dates <- gsub("NA_", "", data_ex2$all_dose3_dates)
# data_ex2$all_dose3_dates <- gsub("_NA", "", data_ex2$all_dose3_dates)
# data_ex2$all_dose3_dates <- gsub("NA", NA_character_, data_ex2$all_dose3_dates)
# data_ex2$all_dose3_dates <- gsub("_.*", "", data_ex2$all_dose3_dates)
# 
# data_ex2$all_dose1_dates <- as.Date(data_ex2$all_dose1_dates)
# data_ex2$all_dose2_dates <- as.Date(data_ex2$all_dose2_dates)
# data_ex2$all_dose3_dates <- as.Date(data_ex2$all_dose3_dates)
data_ex2$all_baby_dob <- as.Date(data_ex2$all_baby_dob, format = "_%Y-%m-%d")

# 
# ### calculate GA where possible and cross-check with timing
# data_ex2$bl1a_delivery_date_BL <- as.Date(data_ex2$bl1a_delivery_date_BL)
# data_ex2$dose1_GA <- as.numeric((280 - (data_ex2$bl1a_delivery_date_BL - data_ex2$all_dose1_dates))/7)
# data_ex2$dose2_GA <- as.numeric((280 - (data_ex2$bl1a_delivery_date_BL - data_ex2$all_dose2_dates))/7)
# data_ex2$dose3_GA <- as.numeric((280 - (data_ex2$bl1a_delivery_date_BL - data_ex2$all_dose3_dates))/7)
# 
# data_ex2$GA_del <- as.numeric((280 - (data_ex2$bl1a_delivery_date_BL - data_ex2$all_baby_dob))/7)
# 
# 
# #### now we need to sort out vaccine timing 
# data_ex2 <- data_ex2 %>% 
#   mutate(
#     vaccine_timing = case_when(
#       grepl("post", all_dose1_info) & grepl("post", all_dose2_info) & grepl("post", all_dose3_info) ~ "D123 pp",
#       grepl("during", all_dose1_info) & grepl("post", all_dose2_info) & grepl("post", all_dose3_info) ~ "D1 preg, D23 pp",
#       grepl("during", all_dose1_info) & grepl("during", all_dose2_info) & grepl("post", all_dose3_info) ~ "D12 in preg, D3 pp",
#       grepl("during", all_dose1_info) & grepl("during", all_dose2_info) & grepl("during", all_dose3_info) ~ "D123 in preg",
#       grepl("during", all_dose1_info) & grepl("post", all_dose2_info) ~ "D1 in preg, D2 pp, no D3",
#       grepl("during", all_dose1_info) & grepl("during", all_dose2_info) ~ "D12 in preg, no D3",
#       include_BL == "include D1 in preg" ~ "D1 in preg, no D23",
#       include_BL == "include D1 pp" ~ "D1 pp, no D23",
#       TRUE ~ include_BL
#     )
#   )
# 
# describeFactors(data_ex2$vaccine_timing)
# D1 in preg, D2 pp, no D3 "3 (0.2%)"     
# D1 pp, no D23            "623 (36.6%)"  
# D1 preg, D23 pp          "1 (0.1%)"     
# D12 in preg, D3 pp       "10 (0.6%)"    
# D12 in preg, no D3       "19 (1.1%)"    
# D123 pp                  "1,006 (59.2%)"
# include no vax BL        "38 (2.2%)" 


## fix some of the timings based on calculated GA
# data_ex2 <- data_ex2 %>% 
#   mutate(
#     vaccine_timing_fix = case_when(
#       vaccine_timing == "D123 in preg" & dose2_GA > GA_del & dose3_GA > GA_del ~ "D1 preg, D23 pp",
#       vaccine_timing == "D123 in preg" & dose2_GA <= GA_del & dose3_GA > GA_del ~ "D12 in preg, D3 pp",
#       vaccine_timing == "D123 in preg" & dose2_GA <= GA_del & dose3_GA <= GA_del ~ "D123 in preg",
#       vaccine_timing == "D123 in preg" & dose2_GA > GA_del & is.na(all_dose3_dates) ~ "D1 in preg, D2 pp, no D3",
#       vaccine_timing == "D123 in preg" & dose2_GA <= GA_del & is.na(all_dose3_dates) ~ "D12 in preg, no D3",
#       TRUE ~ vaccine_timing
#     )
#   )
# 
# describeFactors(data_ex2$vaccine_timing)
# describeFactors(data_ex2$vaccine_timing_fix)
# 
# ## remove the ones with Baby DOB before March 2020
#describeFactors(data_ex2$bl3_after_march_BL)
data_ex2 <- data_ex2[-which(data_ex2$all_baby_dob < "2020-03-01"), ] # from 1700 to 1693



#### make this the final designation
# data_ex2 <- data_ex2 %>% 
#   mutate(
#     vacc_in_ppp = case_when(
#       grepl("preg", vaccine_timing_fix) ~ "exclude",
#       grepl("pp", vaccine_timing_fix) ~ "pp vaccine",
#       grepl("vax", vaccine_timing_fix) ~ "no vaccine"
#     )
#   )
# 
# describeFactors(data_ex2$vacc_in_ppp)
# 
# 
# data_ex2 <- data_ex2[-which(data_ex2$vacc_in_ppp == "exclude"), ] # 1660

data_ex2 <- data_ex2 %>%
  mutate(
    vacc_in_preg = case_when(
      data_ex2$record_id_BL %in% ids.can.EDD.vacc ~ "vaccine in pregnancy",
      data_ex2$record_id_BL %notin% ids.can.EDD.vacc ~ "no vaccine in pregnancy"
    )
  )

describeFactors(data_ex2$vacc_in_preg)


## ----notes---------------------------------------------------------------------------------------------

# COVERED reactogenicity and infection following vaccination during lactation – I hope this wouldn’t be too bad because it’s the exact same as 2/3 parts of the analysis you just did for those vaccinated during pregnancy, just a different group (vaccinated during lactation). 



## ----table---------------------------------------------------------------------------------------------
data <- as.data.frame(data_ex2)

## age
data$bl14_dob_month_BL
data$bl14_dob_year_BL

data$dob <- paste(data$bl14_dob_year_BL, data$bl14_dob_month_BL, "01", sep = "-")
data$dob <- as.Date(data$dob, format = "%Y-%m-%d")
data$bl_completed_date_BL <- as.Date(as.character(data$bl_completed_date_BL), format = "%Y-%m-%d")
# 
# describe(data$dob)

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
      bl19_ethnicity___1_BL == 1 ~ "White/Caucasian"
    )
  )

describeFactors(data$eth)
data <- data %>% 
  mutate(
    eth2 = case_when(
      eth %in% c("Indigenous originating from North America", "Indigneous originating from outside North America", "Mixed", "Other") ~ "Other",
      eth == "Prefer not to answer" ~ NA_character_,
      TRUE ~ eth
    )
  )
describeFactors(data$eth2)
data$eth2 <- factor(data$eth2, levels = c("White/Caucasian", "East Asian", "South Asian", "South East Asian", "Hispanic/Latino/Latina", "African/Caribbean/Black", "West Central Asian/Middle Eastern", "Other"))

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

## Country of birth
describeFactors(data$bl15_country_born_BL)
data$bl15_country_born_BL <- factor(data$bl15_country_born_BL, levels = c(1, 2, 3, 4, 998), labels = c("Canada", "Mexico", "United Kingdom", "United States of America", "Other"))

## province of residence
describeFactors(data$bl16a_province_res_BL)
data$bl16a_province_res_BL <- factor(data$bl16a_province_res_BL, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), labels = c("AB", "BC", "MB", "NB", "NL", "NT", "NS", "NU", "ON", "PE", "QC", "SK", "YK"))

## education
describeFactors(data$bl23_education_BL)
data$bl23_education_BL <- factor(data$bl23_education_BL, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 997, 999), labels = c("Less than high school graduation", "High school graduation", "Trade certificate/Vocational", "Apprenticeship", "Non-University certificate or diploma", "Community college/CEGEP", "Bachelor's degree", "Graduate degree", NA, NA))
## combine
data$bl23_education_BL <- factor(data$bl23_education_BL)
levels(data$bl23_education_BL)[c(1, 2)] <- "High school or less"
levels(data$bl23_education_BL)[c(2, 3, 4, 5)] <- "Trade/Vocational/Apprenticeship/Certificate/College/CEGEP"

## height
describeFactors(data$bl25_height_inches_BL)
#between 4.1 and 6.1 are feet. Multiply by 12 to get inches:
data$bl25_height_inches2 <- sapply(data$bl25_height_inches_BL, function(x) ifelse((x >= 4.1 & x <= 6.1) %in% TRUE, 12*x, x))
describeFactors(data$bl25_height_inches2)
#INCH TO CM: 1 in = 2.54 cm
data$bl25_height_inches_to_cm <- sapply(data$bl25_height_inches2, function(x) ifelse((x < 125) %in% TRUE, x * 2.54, x))
describeFactors(data$bl25_height_inches_to_cm)

describeFactors(data$bl25_height_cm_BL)
#1.57-1.7 are m instead of cm -> multiply by 100
data$bl25_height_cm2 <- sapply(data$bl25_height_cm_BL, function(x) ifelse((x >= 1.57 & x <= 1.7) %in% TRUE, 100*x, x))
#5.4 and 5.6 are feet -> multiply by 30.48
data$bl25_height_cm3 <- sapply(data$bl25_height_cm2, function(x) ifelse((x >= 5.4 & x <= 5.6) %in% TRUE, 30.48*x, x))
#53-65 are inches -> multiply by 2.54
data$bl25_height_cm4 <- sapply(data$bl25_height_cm3, function(x) ifelse((x < 125) %in% TRUE, x * 2.54, x))
describeFactors(data$bl25_height_cm4)

#combine cm with inches variable
data$bl25_height_comb2 <- mapply(paste, data$bl25_height_cm4, data$bl25_height_inches_to_cm, sep="/")
describeFactors(data$bl25_height_comb2)
data$bl25_height_comb2 <- gsub("/NA","",data$bl25_height_comb2)
data$bl25_height_comb2 <- gsub("NA/","",data$bl25_height_comb2)
describeFactors(data$bl25_height_comb2)

#keep only heights between 125 cm and 193 cm
data$bl25_height_comb <- sapply(data$bl25_height_comb2, function(x) ifelse((x >= 125 & x <= 193) %in% TRUE, x, NA))
describeFactors(data$bl25_height_comb)
data$bl25_height_comb <- as.numeric(data$bl25_height_comb)
# summary(data$bl25_height_comb)


## weight
describeFactors(data$bl26_weight_kg_BL)
describeFactors(data$bl26_weight_lbs_BL)

#Set 1300 to NA
#Set everything below 90 to NA because we don't know if these are typos (10,14), are kg or real
data$bl26_weight_lbs_OLD <- data$bl26_weight_lbs_BL
data$bl26_weight_lbs_BL[data$bl26_weight_lbs==1300 & !is.na(data$bl26_weight_lbs_BL)] <- NA #2551
data$bl26_weight_lbs_BL[data$bl26_weight_lbs < 90 & !is.na(data$bl26_weight_lbs_BL)] <- NA


#LBS to KG: Pound (lbs) / 2.2046 = Result in Kilograms (kg)
data$bl26_weight_lbs_to_kg <- round(data$bl26_weight_lbs_BL/ 2.2046,3)
describeFactors(data$bl26_weight_lbs_to_kg)

data$bl26_weight_comb <- mapply(paste, data$bl26_weight_kg_BL, data$bl26_weight_lbs_to_kg, sep="/")
describeFactors(data$bl26_weight_comb)
data$bl26_weight_comb <- gsub("/NA","",data$bl26_weight_comb)
data$bl26_weight_comb <- gsub("NA/","",data$bl26_weight_comb)
describeFactors(data$bl26_weight_comb)
data$bl26_weight_comb <- as.numeric(data$bl26_weight_comb)
# summary(data$bl26_weight_comb)

#BMI = weight in kilograms divided by height in meters squared
#BMI under 18.5 is underweight (under 16 is increased risk of death)
#BMI between 18.5 and 25 is normal range
#BMI between 25 and 30 is overweight
#BMI over 30 is obese
data$BMI <- 10000 * data$bl26_weight_comb/(data$bl25_height_comb^2)
tail(data$BMI)
# summary(data$BMI)

data <- data %>% 
  mutate(BMI_cat = factor(case_when(
    BMI < 25 ~ "<25",
    BMI < 30 ~ "25-29",
    BMI >= 30 ~ "≥30"), levels = c("<25", "25-29", "≥30")))


## vaccine types
data$all_dose1_type <- paste(data$do1_type_BL, data$do1_type_2mo,
                                 data$do1_type_4mo, data$do1_type_6mo,
                                 data$do1_type_8mo, data$do1_type_10mo,
                                 data$do1_type_14mo, sep = "_")

data$all_dose1_type <- gsub("NA_", "", data$all_dose1_type)
data$all_dose1_type <- gsub("__", "", data$all_dose1_type)
data$all_dose1_type <- gsub("_", NA_character_, data$all_dose1_type)
data$all_dose1_type <- gsub("998", "Combination", data$all_dose1_type)
data$all_dose1_type <- gsub("999", NA_character_, data$all_dose1_type)
data$all_dose1_type <- factor(data$all_dose1_type)
#levels(data$all_dose1_type) <- c("BNT162b2", "mRNA-1273", "ChAdOx1-S", "Ad26.COV2.S", "Combo")
levels(data$all_dose1_type) <- c("Pfizer", "Moderna", "AstraZeneca", "J&J", "Novavax",
                                 "Bivalent Moderna", "Bivalent Pfizer","Combination")

describeFactors(data$all_dose1_type)

data$all_dose2_type <- paste(data$dt1_type_BL, data$dt1_type_2mo,
                                 data$dt1_type_4mo, data$dt1_type_6mo,
                                 data$dt1_type_8mo, data$dt1_type_10mo,
                                 data$dt1_type_14mo, sep = "_")

data$all_dose2_type <- gsub("NA_", "", data$all_dose2_type)
data$all_dose2_type <- gsub("__", "", data$all_dose2_type)
data$all_dose2_type <- gsub("_", NA_character_, data$all_dose2_type)
# data$all_dose2_type <- gsub("998", "Combination", data$all_dose2_type)
data$all_dose2_type <- gsub("999", NA_character_, data$all_dose2_type)
data$all_dose2_type <- factor(data$all_dose2_type)
levels(data$all_dose2_type) <- c("Pfizer", "Moderna", "AstraZeneca", "J&J", "Novavax",
                                 "Bivalent Moderna", "Bivalent Pfizer","Combination")
describeFactors(data$all_dose2_type)

data$all_dose3_type <- paste(data$dt1_type_vb_BL, data$dt1_type_vb_2mo,
                                 data$dt1_type_vb_4mo, data$dt1_type_vb_6mo,
                                 data$dt1_type_vb_8mo, data$dt1_type_vb_10mo,
                                 data$dt1_type_vb_14mo, sep = "_")

data$all_dose3_type <- gsub("NA_", "", data$all_dose3_type)
data$all_dose3_type <- gsub("_", "", data$all_dose3_type)
data$all_dose3_type <- gsub("NA", "", data$all_dose3_type)
data$all_dose3_type <- substr(data$all_dose3_type, start =1 , stop = 1)
data$all_dose3_type <- gsub("9", NA_character_, data$all_dose3_type)
data$all_dose3_type <- factor(data$all_dose3_type)
levels(data$all_dose3_type) <- c(NA, "Pfizer", "Moderna", "AstraZeneca", "J&J", "Novavax",
                                 "Bivalent Moderna", "Bivalent Pfizer","Combination")
describeFactors(data$all_dose3_type)

describeFactors(data$all_baby_dob) 

data$all_baby_dob[which(data$all_baby_dob > "2022-04-01")] <- gsub("2022", "2021", data$all_baby_dob[which(data$all_baby_dob > "2022-04-01")])



getT1Stat <- function(varname, digits=0, useNA = "no"){
  getDescriptionStatsBy(data[, varname], 
                        data$vacc_in_preg, 
                        add_total_col=TRUE,
                        show_all_values=TRUE, 
                        hrzl_prop=FALSE,
                        statistics= F, 
                        html=TRUE, 
                        useNA = useNA,
                        digits=digits,
                        header_count = TRUE)
}

getT1Stat.median <- function(varname, digits=0, useNA = "no"){
  getDescriptionStatsBy(data[, varname], 
                        data$vacc_in_preg, 
                        add_total_col=TRUE,
                        show_all_values=TRUE, 
                        hrzl_prop=FALSE,
                        statistics=F, 
                        html=TRUE, 
                        useNA = useNA,
                        digits=digits,
                        header_count = TRUE,
                        continuous_fn = describeMedian)
}


table_data <- list()
table_data[["Age"]] <- getT1Stat("age_cat", 1)
table_data[["BMI"]] <- getT1Stat("BMI_cat", 1)
table_data[["Ethnicity"]] <- getT1Stat("eth2", 1)
table_data[["Gender"]] <- getT1Stat("gender", 1)
# table_data[["Country of birth"]] <- getT1Stat.median("bl15_country_born", 1)
table_data[["Province of residence"]] <- getT1Stat("bl16a_province_res_BL", 1)
table_data[["Education"]] <- getT1Stat("bl23_education_BL", 1)
#table_data[["Vaccine timing"]] <- getT1Stat("vaccine_timing_fix", 1)
table_data[["Dose 1 vaccine types"]] <- getT1Stat("all_dose1_type", 1)
table_data[["Dose 2 vaccine types"]] <- getT1Stat("all_dose2_type", 1)
table_data[["Dose 3 vaccine types"]] <- getT1Stat("all_dose3_type", 1)



# these last 3 are really confusing because people with only 1 dose in preg have timing in preg for 2nd and 3rd doses. This is because they would have had the other doses pre pregnancy

# Now merge everything into a matrix
# and create the rgroup & n.rgroup variabels
rgroup <- c()
n.rgroup <- c()
output_data <- NULL
for (varlabel in names(table_data)) {
  output_data <- rbind(output_data, table_data[[varlabel]])
  rgroup <- c(rgroup,
              varlabel)
  n.rgroup <- c(n.rgroup,
                nrow(table_data[[varlabel]]))
}

# Add a column spanner for the columns
cgroup <- c("", "Dose timing")
n.cgroup <- c(1, 2)
colnames(output_data) <- gsub("[ ]*Dose timing", "", colnames(output_data))

htmlTable::htmlTable(output_data, align = "rrrr",
          rgroup = rgroup, n.rgroup = n.rgroup,
          css.rgroup.sep = "",
          cgroup = cgroup,
          n.cgroup = n.cgroup,
          rowlabel = "",
          caption = "Table 1. Demographic and clinical history # summary",
          ctable = TRUE)  %>% htmltools::html_print()





## ----plot people over time by vaxx NOT USED------------------------------------------------------------
data$first_date <- as.Date(data$demographic_health_pregnancy_survey_timestamp_BL)
summary(data$first_date)

# recruitment date
ggplot(data, aes(x = first_date, group = vacc_in_preg, fill = vacc_in_preg)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~vacc_in_preg)

# baby DOB 
ggplot(data, aes(x = all_baby_dob, group = vacc_in_preg, fill = vacc_in_preg)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~vacc_in_preg)

## first vaccine dates
data$do3_date_BL <- as.Date(data$do3_date_BL)
ggplot(data, aes(x = do3_date_BL, group = vacc_in_preg, fill = vacc_in_preg)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~vacc_in_preg) +
  scale_x_date(limits = as.Date(c("2020-12-01", "2022-03-01")))

data$do3_date_2mo <- as.Date(data$do3_date_2mo)
ggplot(data, aes(x = do3_date_2mo, group = vacc_in_preg, fill = vacc_in_preg)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~vacc_in_preg) +
  scale_x_date(limits = as.Date(c("2020-12-01", "2022-03-01")))



## ----how many had covid--------------------------------------------------------------------------------
## how many of the completely unvaccinated had covid?

#describeFactors(data$vaccine_timing_fix) # 38 people did not ever record a vaccine

data_unvax <- data[which(data$vacc_in_preg == "no vaccine in pregnancy"), ]

describeFactors(data_unvax$redcap_event_name_2mo)
# followup_2_months_arm_1 "34 (89.5%)"
# Missing                 "4 (10.5%)"

describeFactors(data_unvax$redcap_event_name_4mo)
# followup_4_months_arm_1 "34 (89.5%)"
# Missing                 "4 (10.5%)" 

describeFactors(data_unvax$redcap_event_name_6mo)
# followup_6_months_arm_1 "31 (81.6%)"
# Missing                 "7 (18.4%)"

describeFactors(data_unvax$redcap_event_name_8mo)
# followup_8_months_arm_1 "29 (76.3%)"
# Missing                 "9 (23.7%)"

describeFactors(data_unvax$redcap_event_name_10mo)
# followup_10_months_arm_1 "15 (39.5%)"
# Missing                  "23 (60.5%)"

describeFactors(data_unvax$redcap_event_name_14mo)
# Missing "38 (100.0%)"


data_unvax$all_covid_inf_fu <- paste(data_unvax$fu4_covid_pos_2mo,
                                     data_unvax$fu4_covid_pos_4mo,
                                     data_unvax$fu4_covid_pos_6mo,
                                     data_unvax$fu4_covid_pos_8mo,
                                     data_unvax$fu4_covid_pos_10mo,
                                     data_unvax$fu4_covid_pos_14mo, sep = "_")

data_unvax$all_covid_inf_fu <- gsub("NA_", "", data_unvax$all_covid_inf_fu)
data_unvax$all_covid_inf_fu <- gsub("_NA", "", data_unvax$all_covid_inf_fu)
data_unvax$all_covid_inf_fu <- ifelse(grepl("1", data_unvax$all_covid_inf_fu), "yes", "no")

describeFactors(data_unvax$all_covid_inf_fu)
# no  "20 (52.6%)"
# yes "18 (47.4%)"

## severity
data_unvax$all_covid_severity_fu <- paste(data_unvax$fu5_covid_severity_2mo,
                                     data_unvax$fu5_covid_severity_4mo,
                                     data_unvax$fu5_covid_severity_6mo,
                                     data_unvax$fu5_covid_severity_8mo,
                                     data_unvax$fu5_covid_severity_10mo,
                                     data_unvax$fu5_covid_severity_14mo, sep = "_")

data_unvax$all_covid_severity_fu <- gsub("NA_", "", data_unvax$all_covid_severity_fu)
data_unvax$all_covid_severity_fu <- gsub("_NA", "", data_unvax$all_covid_severity_fu)
describeFactors(data_unvax$all_covid_severity_fu)
# 2     "6 (15.8%)" 
# 2_2   "5 (13.2%)" 
# 2_2_3 "1 (2.6%)"  
# 2_3   "2 (5.3%)"  
# 3     "4 (10.5%)" 
# NA    "20 (52.6%)"
## All mild or moderate - no hosp


### of those vaccinated
data_vaxPP <- data[which(data$vacc_in_preg == "vaccine in pregnancy"), ]
#
data_vaxPP$all_covid_inf_fu <- paste(data_vaxPP$fu4_covid_pos_2mo,
                                     data_vaxPP$fu4_covid_pos_4mo,
                                     data_vaxPP$fu4_covid_pos_6mo,
                                     data_vaxPP$fu4_covid_pos_8mo,
                                     data_vaxPP$fu4_covid_pos_10mo,
                                     data_vaxPP$fu4_covid_pos_14mo, sep = "_")
#
data_vaxPP$all_covid_inf_fu <- gsub("NA_", "", data_vaxPP$all_covid_inf_fu)
data_vaxPP$all_covid_inf_fu <- gsub("_NA", "", data_vaxPP$all_covid_inf_fu)
data_vaxPP$all_covid_inf_fu <- gsub("NA", "", data_vaxPP$all_covid_inf_fu)
data_vaxPP$all_covid_inf_fu <- ifelse(grepl("1", data_vaxPP$all_covid_inf_fu), "yes", "no")
#
describeFactors(data_vaxPP$all_covid_inf_fu)
# # no  "1,068 (65.8%)"
# # yes "554 (34.2%)"

## severity
data_vaxPP$all_covid_severity_fu <- paste(data_vaxPP$fu5_covid_severity_2mo,
                                     data_vaxPP$fu5_covid_severity_4mo,
                                     data_vaxPP$fu5_covid_severity_6mo,
                                     data_vaxPP$fu5_covid_severity_8mo,
                                     data_vaxPP$fu5_covid_severity_10mo,
                                     data_vaxPP$fu5_covid_severity_14mo, sep = "_")

data_vaxPP$all_covid_severity_fu <- gsub("NA_", "", data_vaxPP$all_covid_severity_fu)
data_vaxPP$all_covid_severity_fu <- gsub("_NA", "", data_vaxPP$all_covid_severity_fu)
describeFactors(data_vaxPP$all_covid_severity_fu)
# 1     "13 (0.8%)"
# 1_1   "7 (0.4%)"
# 1_2   "1 (0.1%)"
# 2     "333 (20.5%)"
# 2_1   "1 (0.1%)"
# 2_2   "69 (4.3%)"
# 2_2_1 "1 (0.1%)"
# 2_2_2 "6 (0.4%)"
# 2_3   "4 (0.2%)"
# 3     "102 (6.3%)"
# 3_2   "2 (0.1%)"
# 3_3   "9 (0.6%)"
# 3_3_2 "1 (0.1%)"
# 3_3_3 "3 (0.2%)"
# 4     "1 (0.1%)"
# NA    "1,069 (65.9%)"

##  all asymptomatic, mild, or moderate.




## ----vaccine reactions---------------------------------------------------------------------------------

## redness
# describeFactors(data_vaxPP$do4_redness)
# describeFactors(data_vaxPP$do4_redness_BL)
# describeFactors(data_vaxPP$do4_redness_2mo)
# describeFactors(data_vaxPP$dt4_redness)
# describeFactors(data_vaxPP$dt4_redness_BL)
# describeFactors(data_vaxPP$dt4_redness_2mo)
# describeFactors(data_vaxPP$dt4_redness_vb)
# describeFactors(data_vaxPP$dt4_redness_vb_BL)
# describeFactors(data_vaxPP$dt4_redness_vb_2mo)


data_vaxPP <- data_vaxPP %>%
  mutate(
    dose1_red = case_when(
      do4_redness_BL == 1 | do4_redness_2mo == 1 | do4_redness_4mo | do4_redness_6mo | do4_redness_8mo ~ 1,
      TRUE ~ 0
    ),
    dose2_red = case_when(
      dt4_redness_BL == 1 | dt4_redness_2mo == 1 | dt4_redness_4mo | dt4_redness_6mo | dt4_redness_8mo ~ 1,
      TRUE ~ 0
    ),
    dose3_red = case_when(
      dt4_redness_vb_BL == 1 | dt4_redness_vb_2mo == 1 | dt4_redness_vb_4mo | dt4_redness_vb_6mo | dt4_redness_vb_8mo | dt4_redness_vb_10mo | dt4_redness_vb_14mo == "1" ~ 1,
      TRUE ~ 0)
  )
#
# describeFactors(data_vaxPP$dose1_red)
# describeFactors(data_vaxPP$dose2_red)
# describeFactors(data_vaxPP$dose3_red)
#
## tiredness
data_vaxPP <- data_vaxPP %>%
  mutate(
    dose1_tired = case_when(
      do5_sideffects___1_BL == 1 | do5_sideffects___1_2mo == 1 | do5_sideffects___1_4mo | do5_sideffects___1_6mo | do5_sideffects___1_8mo ~ 1,
      TRUE ~ 0
    ),
    dose2_tired = case_when(
      dt5_sideffects___1_BL == 1 | dt5_sideffects___1_2mo == 1 | dt5_sideffects___1_4mo | dt5_sideffects___1_6mo | dt5_sideffects___1_8mo ~ 1,
      TRUE ~ 0
    ),
    dose3_tired = case_when(
      dt5_sideffects_vb___1_BL == 1 | dt5_sideffects_vb___1_2mo == 1 | dt5_sideffects_vb___1_4mo | dt5_sideffects_vb___1_6mo | dt5_sideffects_vb___1_8mo | dt5_sideffects_vb___1_10mo | dt5_sideffects_vb___1_14mo == "1" ~ 1,
      TRUE ~ 0)
  )


# describeFactors(data_vaxPP$dose1_tired)
# describeFactors(data_vaxPP$dose2_tired)
# describeFactors(data_vaxPP$dose3_tired)
#
## headache
data_vaxPP <- data_vaxPP %>%
  mutate(
    dose1_head = case_when(
      do5_sideffects___2_BL == 1 | do5_sideffects___2_2mo == 1 | do5_sideffects___2_4mo | do5_sideffects___2_6mo | do5_sideffects___2_8mo ~ 1,
      TRUE ~ 0
    ),
    dose2_head = case_when(
      dt5_sideffects___2_BL == 1 | dt5_sideffects___2_2mo == 1 | dt5_sideffects___2_4mo | dt5_sideffects___2_6mo | dt5_sideffects___2_8mo ~ 1,
      TRUE ~ 0
    ),
    dose3_head = case_when(
      dt5_sideffects_vb___2_BL == 1 | dt5_sideffects_vb___2_2mo == 1 | dt5_sideffects_vb___2_4mo | dt5_sideffects_vb___2_6mo | dt5_sideffects_vb___2_8mo | dt5_sideffects_vb___2_10mo | dt5_sideffects_vb___2_14mo == "1" ~ 1,
      TRUE ~ 0)
  )
#
#
# describeFactors(data_vaxPP$dose1_head)
# describeFactors(data_vaxPP$dose2_head)
# describeFactors(data_vaxPP$dose3_head)
#
## muscle pain
data_vaxPP <- data_vaxPP %>%
  mutate(
    dose1_musc = case_when(
      do5_sideffects___3_BL == 1 | do5_sideffects___3_2mo == 1 | do5_sideffects___3_4mo | do5_sideffects___3_6mo | do5_sideffects___3_8mo ~ 1,
      TRUE ~ 0
    ),
    dose2_musc = case_when(
      dt5_sideffects___3_BL == 1 | dt5_sideffects___3_2mo == 1 | dt5_sideffects___3_4mo | dt5_sideffects___3_6mo | dt5_sideffects___3_8mo ~ 1,
      TRUE ~ 0
    ),
    dose3_musc = case_when(
      dt5_sideffects_vb___3_BL == 1 | dt5_sideffects_vb___3_2mo == 1 | dt5_sideffects_vb___3_4mo | dt5_sideffects_vb___3_6mo | dt5_sideffects_vb___3_8mo | dt5_sideffects_vb___3_10mo | dt5_sideffects_vb___3_14mo == "1" ~ 1,
      TRUE ~ 0)
  )

# describeFactors(data_vaxPP$dose1_musc)
# describeFactors(data_vaxPP$dose2_musc)
# describeFactors(data_vaxPP$dose3_musc)
#
## chills
data_vaxPP <- data_vaxPP %>%
  mutate(
    dose1_chill = case_when(
      do5_sideffects___4_BL == 1 | do5_sideffects___4_2mo == 1 | do5_sideffects___4_4mo | do5_sideffects___4_6mo | do5_sideffects___4_8mo ~ 1,
      TRUE ~ 0
    ),
    dose2_chill = case_when(
      dt5_sideffects___4_BL == 1 | dt5_sideffects___4_2mo == 1 | dt5_sideffects___4_4mo | dt5_sideffects___4_6mo | dt5_sideffects___4_8mo ~ 1,
      TRUE ~ 0
    ),
    dose3_chill = case_when(
      dt5_sideffects_vb___4_BL == 1 | dt5_sideffects_vb___4_2mo == 1 | dt5_sideffects_vb___4_4mo | dt5_sideffects_vb___4_6mo | dt5_sideffects_vb___4_8mo | dt5_sideffects_vb___4_10mo | dt5_sideffects_vb___4_14mo == "1" ~ 1,
      TRUE ~ 0)
  )
#
#
#
## fever
data_vaxPP <- data_vaxPP %>%
  mutate(
    dose1_fever = case_when(
      do5_sideffects___5_BL == 1 | do5_sideffects___5_2mo == 1 | do5_sideffects___5_4mo | do5_sideffects___5_6mo | do5_sideffects___5_8mo ~ 1,
      TRUE ~ 0
    ),
    dose2_fever = case_when(
      dt5_sideffects___5_BL == 1 | dt5_sideffects___5_2mo == 1 | dt5_sideffects___5_4mo | dt5_sideffects___5_6mo | dt5_sideffects___5_8mo ~ 1,
      TRUE ~ 0
    ),
    dose3_fever = case_when(
      dt5_sideffects_vb___5_BL == 1 | dt5_sideffects_vb___5_2mo == 1 | dt5_sideffects_vb___5_4mo | dt5_sideffects_vb___5_6mo | dt5_sideffects_vb___5_8mo | dt5_sideffects_vb___5_10mo | dt5_sideffects_vb___5_14mo == "1" ~ 1,
      TRUE ~ 0)
  )

## nausea
data_vaxPP <- data_vaxPP %>%
  mutate(
    dose1_naus = case_when(
      do5_sideffects___6_BL == 1 | do5_sideffects___6_2mo == 1 | do5_sideffects___6_4mo | do5_sideffects___6_6mo | do5_sideffects___6_8mo ~ 1,
      TRUE ~ 0
    ),
    dose2_naus = case_when(
      dt5_sideffects___6_BL == 1 | dt5_sideffects___6_2mo == 1 | dt5_sideffects___6_4mo | dt5_sideffects___6_6mo | dt5_sideffects___6_8mo ~ 1,
      TRUE ~ 0
    ),
    dose3_naus = case_when(
      dt5_sideffects_vb___6_BL == 1 | dt5_sideffects_vb___6_2mo == 1 | dt5_sideffects_vb___6_4mo | dt5_sideffects_vb___6_6mo | dt5_sideffects_vb___6_8mo | dt5_sideffects_vb___6_10mo | dt5_sideffects_vb___6_14mo == "1" ~ 1,
      TRUE ~ 0)
  )






## ----plot vaccine reactions, fig.width = 8-------------------------------------------------------------
## need to ensure correct denominators
react.mat <- matrix(nrow = 21, ncol = 3)
colnames(react.mat) <- c("Effect", "N", "Dose")
react.mat[, 1] <- rep(c("Redness, pain, or swelling at the injection site", "Tiredness", "Headache", "Muscle pain", "Chills", "Fever", "Nausea"), 3)

react.mat[, 3] <- c(rep("Dose 1", 7), rep("Dose 2", 7), rep("Dose 3", 7))

react.mat[1, 2] <- sum(data_vaxPP$dose1_red)/length(which(data_vaxPP$vaccine_timing_fix %in% c("D1 pp, no D23", "D123 pp")))

react.mat[2, 2] <- sum(data_vaxPP$dose1_tired)/length(which(data_vaxPP$vaccine_timing_fix %in% c("D1 pp, no D23", "D123 pp")))

react.mat[3, 2] <- sum(data_vaxPP$dose1_head)/length(which(data_vaxPP$vaccine_timing_fix %in% c("D1 pp, no D23", "D123 pp")))

react.mat[4, 2] <- sum(data_vaxPP$dose1_musc)/length(which(data_vaxPP$vaccine_timing_fix %in% c("D1 pp, no D23", "D123 pp")))

react.mat[5, 2] <- sum(data_vaxPP$dose1_chill)/length(which(data_vaxPP$vaccine_timing_fix %in% c("D1 pp, no D23", "D123 pp")))

react.mat[6, 2] <- sum(data_vaxPP$dose1_fever)/length(which(data_vaxPP$vaccine_timing_fix %in% c("D1 pp, no D23", "D123 pp")))

react.mat[7, 2] <- sum(data_vaxPP$dose1_naus)/length(which(data_vaxPP$vaccine_timing_fix %in% c("D1 pp, no D23", "D123 pp")))


react.mat[8, 2] <- sum(data_vaxPP$dose2_red[which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp"))])/length(which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp")))

react.mat[9, 2] <- sum(data_vaxPP$dose2_tired[which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp"))])/length(which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp")))

react.mat[10, 2] <- sum(data_vaxPP$dose2_head[which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp"))])/length(which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp")))

react.mat[11, 2] <- sum(data_vaxPP$dose2_musc[which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp"))])/length(which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp")))

react.mat[12, 2] <- sum(data_vaxPP$dose2_chill[which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp"))])/length(which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp")))

react.mat[13, 2] <- sum(data_vaxPP$dose2_fever[which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp"))])/length(which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp")))

react.mat[14, 2] <- sum(data_vaxPP$dose2_naus[which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp"))])/length(which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp")))

react.mat[15, 2] <- sum(data_vaxPP$dose3_red[which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp"))])/length(which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp")))

react.mat[16, 2] <- sum(data_vaxPP$dose3_tired[which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp"))])/length(which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp")))

react.mat[17, 2] <- sum(data_vaxPP$dose3_head[which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp"))])/length(which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp")))

react.mat[18, 2] <- sum(data_vaxPP$dose3_musc[which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp"))])/length(which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp")))

react.mat[19, 2] <- sum(data_vaxPP$dose3_chill[which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp"))])/length(which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp")))

react.mat[20, 2] <- sum(data_vaxPP$dose3_fever[which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp"))])/length(which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp")))

react.mat[21, 2] <- sum(data_vaxPP$dose3_naus[which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp"))])/length(which(data_vaxPP$vaccine_timing_fix %in% c("D123 pp")))


react.mat <- as.data.frame(react.mat)
react.mat$Effect <- factor(react.mat$Effect, levels = c("Redness, pain, or swelling at the injection site", "Tiredness", "Headache", "Muscle pain", "Chills", "Fever", "Nausea"), labels = c("Redness,\npain, or swelling\nat the injection site", "Tiredness", "Headache", "Muscle pain", "Chills", "Fever", "Nausea"))

react.mat$N <- as.numeric(react.mat$N)
react.mat$N <- round(react.mat$N*100, 1)

# quartz(width = 10, height = 4)
ggplot(react.mat, aes(x = Effect, y = N, fill = Dose, group = Dose)) +
  geom_col(position = position_dodge(), colour = "black") +
  theme_pubr() +
  scale_fill_manual(values = ghibli_palette("KikiMedium")[c(4, 6, 2)]) +
  theme(panel.grid.major.y = element_line(linetype = 2, colour = "grey"),
        panel.grid.minor.y = element_line(linetype = 3, colour = "grey"),
        text = element_text(size = 13)) +
  xlab("") +
  ylab("% of Participants\nreceiving dose during lactation") +
  guides(fill = guide_legend(title = ""))



## ----comparison of reactions by dose-------------------------------------------------------------------

rx.data <- reshape(data_vaxPP[, c("record_id_BL", "dose1_red", "dose1_tired", "dose1_head", "dose1_musc", "dose1_chill", "dose1_fever", "dose1_naus", "dose2_red", "dose2_tired", "dose2_head", "dose2_musc", "dose2_chill", "dose2_fever", "dose2_naus", "dose3_red", "dose3_tired", "dose3_head", "dose3_musc", "dose3_chill", "dose3_fever", "dose3_naus", "all_dose1_type", "all_dose2_type", "all_dose3_type", "vaccine_timing_fix")],
                   varying = list(
                     red = c("dose1_red", "dose2_red", "dose3_red"),
                     tired = c("dose1_tired", "dose2_tired", "dose3_tired"),
                     head = c("dose1_head", "dose2_head", "dose3_head"),
                     musc = c("dose1_musc", "dose2_musc", "dose3_musc"),
                     chills = c("dose1_chill", "dose2_chill", "dose3_chill"),
                     fever = c("dose1_fever", "dose2_fever", "dose3_fever"),
                     naus = c("dose1_naus", "dose2_naus", "dose3_naus"),
                     type = c("all_dose1_type", "all_dose2_type", "all_dose3_type")
                   ),idvar = "record_id_BL", ids = "record_id_BL", times = c("Dose1", "Dose2", "Dose3"), direction = "long")

tab_xtab(rx.data$dose1_red, rx.data$time, show.col.prc = TRUE)

## redness
red.mod <- glmer(dose1_red ~ time + (1|record_id_BL), data = rx.data, family = binomial)
summary(red.mod)

red.mm <- emmeans(red.mod, ~ time)
contrast(red.mm, method = "pairwise")
 # contrast      estimate     SE  df z.ratio p.value
 # Dose1 - Dose2    0.173 0.0879 Inf   1.971  0.1194
 # Dose1 - Dose3    1.115 0.0904 Inf  12.331  <.0001
 # Dose2 - Dose3    0.941 0.0887 Inf  10.609  <.0001


## tiredness
tired.mod <- glmer(dose1_tired ~ time + (1|record_id_BL), data = rx.data, family = binomial)
summary(tired.mod)

tired.mm <- emmeans(tired.mod, ~ time)
contrast(tired.mm, method = "pairwise")
 # contrast      estimate     SE  df z.ratio p.value
 # Dose1 - Dose2   -1.066 0.0863 Inf -12.351  <.0001
 # Dose1 - Dose3    0.172 0.0815 Inf   2.114  0.0869
 # Dose2 - Dose3    1.238 0.0876 Inf  14.136  <.0001

## headaches
head.mod <- glmer(dose1_head ~ time + (1|record_id_BL), data = rx.data, family = binomial)
summary(head.mod)

head.mm <- emmeans(head.mod, ~ time)
contrast(head.mm, method = "pairwise")
 # contrast      estimate     SE  df z.ratio p.value
 # Dose1 - Dose2 -0.99401 0.0877 Inf -11.328  <.0001
 # Dose1 - Dose3  0.00775 0.0880 Inf   0.088  0.9957
 # Dose2 - Dose3  1.00175 0.0878 Inf  11.405  <.0001


## chills
chill.mod <- glmer(dose1_chill ~ time + (1|record_id_BL), data = rx.data, family = binomial)
summary(chill.mod)

chill.mm <- emmeans(chill.mod, ~ time)
contrast(chill.mm, method = "pairwise")
 # contrast      estimate    SE  df z.ratio p.value
 # Dose1 - Dose2   -1.639 0.117 Inf -14.056  <.0001
 # Dose1 - Dose3   -0.688 0.119 Inf  -5.782  <.0001
 # Dose2 - Dose3    0.951 0.100 Inf   9.490  <.0001

## muscle pain
musc.mod <- glmer(dose1_musc ~ time + (1|record_id_BL), data = rx.data, family = binomial)
summary(musc.mod)

musc.mm <- emmeans(musc.mod, ~ time)
contrast(musc.mm, method = "pairwise")
 # contrast      estimate     SE  df z.ratio p.value
 # Dose1 - Dose2   -0.818 0.0841 Inf  -9.736  <.0001
 # Dose1 - Dose3    0.101 0.0837 Inf   1.212  0.4462
 # Dose2 - Dose3    0.920 0.0849 Inf  10.832  <.0001

## fever
fever.mod <- glmer(dose1_fever ~ time + (1|record_id_BL), data = rx.data, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(fever.mod)

fever.mm <- emmeans(fever.mod, ~ time)
contrast(fever.mm, method = "pairwise")
 # contrast      estimate    SE  df z.ratio p.value
 # Dose1 - Dose2   -1.809 0.162 Inf -11.163  <.0001
 # Dose1 - Dose3   -0.396 0.161 Inf  -2.451  0.0379
 # Dose2 - Dose3    1.413 0.145 Inf   9.753  <.0001


## nausea
naus.mod <- glmer(dose1_naus ~ time + (1|record_id_BL), data = rx.data, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(naus.mod)

naus.mm <- emmeans(naus.mod, ~ time)
contrast(naus.mm, method = "pairwise")
 # contrast      estimate    SE  df z.ratio p.value
 # Dose1 - Dose2  -1.0797 0.145 Inf  -7.422  <.0001
 # Dose1 - Dose3  -0.0496 0.157 Inf  -0.315  0.9468
 # Dose2 - Dose3   1.0301 0.144 Inf   7.172  <.0001


## add annotations to the plot

quartz(width = 10, height = 4)
ggplot(react.mat, aes(x = Effect, y = N, fill = Dose, group = Dose)) +
  geom_col(position = position_dodge(), colour = "black") +
  theme_pubr() +
  scale_fill_manual(values = ghibli_palette("KikiMedium")[c(4, 6, 2)]) +
  theme(panel.grid.major.y = element_line(linetype = 2, colour = "grey"),
        panel.grid.minor.y = element_line(linetype = 3, colour = "grey"),
        text = element_text(size = 13)) +
  xlab("") +
  ylab("% of Participants\nreceiving dose during lactation") +
  guides(fill = guide_legend(title = "")) +
  annotate("text", x = c(0.7, 1, 1.3), y = react.mat[c(1, 8, 15), "N"] + 5, label = c("a", "b", "b")) +
  annotate("text", x = c(1.7, 2, 2.3), y = react.mat[c(2, 9, 16), "N"] + 5, label = c("a", "b", "a")) +
  annotate("text", x = c(2.7, 3, 3.3), y = react.mat[c(3, 10, 17), "N"] + 5, label = c("a", "b", "a")) +
  annotate("text", x = c(3.7, 4, 4.3), y = react.mat[c(4, 11, 18), "N"] + 5, label = c("a", "b", "c")) +
  annotate("text", x = c(4.7, 5, 5.3), y = react.mat[c(5, 12, 19), "N"] + 5, label = c("a", "b", "a")) +
  annotate("text", x = c(5.7, 6, 6.3), y = react.mat[c(6, 13, 20), "N"] + 5, label = c("a", "b", "c")) +
  annotate("text", x = c(6.7, 7, 7.3), y = react.mat[c(7, 14, 21), "N"] + 5, label = c("a", "b", "a"))



## ----supp plot for reactions by type-------------------------------------------------------------------

rx.data2 <- rx.data %>% 
  group_by(time, all_dose1_type) %>% 
  summarise(across(dose1_red:dose1_naus, ~ sum(.x == 1)))

rx.data3 <- rx.data %>% 
  group_by(time, all_dose1_type) %>% 
  summarise(across(dose1_red:dose1_naus, ~ length(.x)))

rx.data4 <- left_join(rx.data2, rx.data3, by = c("time", "all_dose1_type"))
rx.data4$ids <- paste0(rx.data4$time, rx.data4$all_dose1_type)

rx.data5 <- reshape(as.data.frame(rx.data4), varying = list(
  react = c("dose1_red.x", "dose1_tired.x", "dose1_head.x", "dose1_musc.x", "dose1_chill.x", "dose1_fever.x", "dose1_naus.x"),
  totals = c("dose1_red.y", "dose1_tired.y", "dose1_head.y", "dose1_musc.y", "dose1_chill.y", "dose1_fever.y", "dose1_naus.y")), ids = rx.data4$ids, times = c("red", "tired", "head", "musc", "chill", "fever", "naus"), direction = "long")

rx.data5 <- na.omit(rx.data5)

rx.data5$prop <- rx.data5$dose1_red.x/rx.data5$dose1_red.y
rx.data5 <- rx.data5 %>% 
  mutate(
    dose = case_when(
      grepl("Dose1", ids) ~ "Dose 1",
      grepl("Dose2", ids) ~ "Dose 2",
      grepl("Dose3", ids) ~ "Dose 3",
    )
  )

rx.data5$time <- factor(rx.data5$time, levels = c("red", "tired", "head", "musc", "chill", "fever", "naus"), labels = c("Redness, pain, or swelling at the injection site", "Tiredness", "Headache", "Muscle pain", "Chills", "Fever", "Nausea"))

ggplot(rx.data5[-which(rx.data5$all_dose1_type %in% c("Combo", "Ad26.COV2.S", "ChAdOx1-S")), ], aes(x = time, y = prop, group = all_dose1_type, fill = all_dose1_type)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~dose) +
  scale_x_discrete(labels = scales::label_wrap(10))

quartz(width = 12, height = 6)
ggplot(rx.data5[-which(rx.data5$all_dose1_type %in% c("Combo", "Ad26.COV2.S", "ChAdOx1-S")), ], aes(y = time, x = prop, group = all_dose1_type, fill = all_dose1_type)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~dose) +
  scale_y_discrete(labels = scales::label_wrap(20)) +
  scale_x_continuous(labels = scales::percent_format()) +
  theme_pubr() +
  scale_fill_manual(values = ghibli_palette("KikiMedium")[c(4, 6, 2)]) +
  theme(panel.grid.major.y = element_line(linetype = 2, colour = "grey"),
        panel.grid.minor.y = element_line(linetype = 3, colour = "grey"),
        text = element_text(size = 14),
        panel.spacing = unit(2, "lines"),
        strip.text.x = element_text(size = 20)) +
  ylab("") +
  xlab("% of Participants\nreceiving dose in pregnancy") +
  guides(fill = guide_legend(title = "", reverse = TRUE))





## ----problems post vaccine-----------------------------------------------------------------------------
## new/worse problems
# describeFactors(data$do7_problem)
# describeFactors(data$do7_problem_BL) #
# describeFactors(data$do7_problem_2mo)

data_vaxPP <- data_vaxPP %>% 
  mutate(dose1_newprob = case_when(
    do7_problem_BL == 1 | do7_problem_2mo == 1 | do7_problem_4mo == 1 | do7_problem_6mo == 1 | do7_problem_8mo == 1 ~ 1,
    TRUE ~ 0
  ),
  dose2_newprob = case_when(
    dt7_problem_BL == 1 | dt7_problem_2mo == 1 | dt7_problem_4mo == 1 | dt7_problem_6mo == 1 | dt7_problem_8mo == 1 ~ 1,
    TRUE ~ 0
  ),
  dose3_newprob = case_when(
    dt7_problem_vb_BL == 1 | dt7_problem_vb_2mo == 1 | dt7_problem_vb_4mo == 1 | dt7_problem_vb_6mo == 1 | dt7_problem_vb_8mo == 1 | dt7_problem_vb_10mo == 1 | dt7_problem_vb_14mo == 1 ~ 1,
    TRUE ~ 0)
  )

## prevented/stopped normal activity
# describeFactors(data_vaxPP$do8_normalactivi_2mo_BL) #
# describeFactors(data_vaxPP$do8_normalactivi_2mo_2mo) 
# describeFactors(data_vaxPP$do8_normalactivi_2mo)

data_vaxPP <- data_vaxPP %>% 
  mutate(dose1_act = case_when(
    do8_normalactivity_BL == 1 | do8_normalactivity_2mo == 1 | do8_normalactivity_4mo == 1 ~ 1,
    TRUE ~ 0
  ),
  dose2_act = case_when(
    dt8_normalactivity_BL == 1 | dt8_normalactivity_2mo == 1 | dt8_normalactivity_4mo == 1 | dt8_normalactivity_6mo == 1~ 1,
    TRUE ~ 0
  ),
  dose3_act = case_when(
    dt8_normalactivity_vb_BL == 1 | dt8_normalactivity_vb_2mo == 1 | dt8_normalactivity_vb_4mo == 1 | dt8_normalactivity_vb_6mo == 1 | dt8_normalactivity_vb_8mo == 1 | dt8_normalactivity_vb_10mo == 1 | dt8_normalactivity_vb_14mo == 1 ~ 1,
    TRUE ~ 0
  ))

## to see health care provider


data_vaxPP <- data_vaxPP %>% 
  mutate(dose1_hcp = case_when(
    do9_hcp_BL == 1 | do9_hcp_2mo == 1 | do9_hcp_4mo == 1 ~ 1,
    TRUE ~ 0
  ),
  dose2_hcp = case_when(
    dt9_hcp_BL == 1 | dt9_hcp_2mo == 1 | dt9_hcp_4mo == 1 | dt9_hcp_6mo == 1  ~ 1,
    TRUE ~ 0
  ),
  dose3_hcp = case_when(
    dt9_hcp_vb_BL == 1 | dt9_hcp_vb_2mo == 1 | dt9_hcp_vb_4mo == 1 | dt9_hcp_vb_6mo == 1 | dt9_hcp_vb_8mo == 1 | dt9_hcp_vb_10mo == 1 | dt9_hcp_vb_14mo == 1  ~ 1,
    TRUE ~ 0
  ))

## saw HCP
# describeFactors(data_vaxPP$do10_hcp_seen_BL) 
# describeFactors(data_vaxPP$do10_hcp_seen_2mo) #
# describeFactors(data_vaxPP$do9_hcp_2mo)

data_vaxPP <- data_vaxPP %>% 
  mutate(dose1_sawhcp = case_when(
    do10_hcp_seen_BL == 1 | do10_hcp_seen_4mo == 1 ~ 1,
    TRUE ~ 0
  ),
  dose2_sawhcp = case_when(
    dt10_hcp_seen_BL == 1 | dt10_hcp_seen_2mo == 1 | dt10_hcp_seen_6mo == 1 ~ 1,
    TRUE ~ 0
  ),
  dose3_sawhcp = case_when(
    dt10_hcp_seen_vb_BL == 1 | dt10_hcp_seen_vb_2mo == 1 | dt10_hcp_seen_vb_6mo == 1 | dt10_hcp_seen_vb_8mo == 1  | dt10_hcp_seen_vb_10mo == 1 | dt10_hcp_seen_vb_14mo == 1 ~ 1,
    TRUE ~ 0
  ))

## emergency room/urgent care
# describeFactors(data_vaxPP$do11_visit_2mope___3) 
# describeFactors(data_vaxPP$do11_visit_2mope___3_BL) #
# describeFactors(data_vaxPP$do11_visit_2mope___3_2mo)

data_vaxPP <- data_vaxPP %>% 
  mutate(dose1_er = case_when(
    do11_visittype___3_BL == 1 | do11_visittype___3_2mo == 1 | do11_visittype___3_4mo == 1 | do11_visittype___3_6mo == 1 | do11_visittype___3_8mo == 1 | do11_visittype___3_10mo == 1 | do11_visittype___3_14mo == 1 ~ 1,
    TRUE ~ 0
  ),
  dose2_er = case_when(
    dt11_visittype___3_BL == 1 | dt11_visittype___3_2mo == 1 | dt11_visittype___3_4mo == 1 | dt11_visittype___3_6mo == 1 | dt11_visittype___3_8mo == 1 | dt11_visittype___3_10mo == 1 | dt11_visittype___3_14mo == 1  ~ 1,
    TRUE ~ 0
  ),
  dose3_er = case_when(
    dt11_visittype_vb___3_BL == 1 | dt11_visittype_vb___3_2mo == 1 | dt11_visittype_vb___3_4mo == 1 | dt11_visittype_vb___3_6mo == 1 | dt11_visittype_vb___3_8mo == 1 | dt11_visittype_vb___3_10mo == 1 | dt11_visittype_vb___3_14mo == 1  ~ 1,
    TRUE ~ 0
  ))

## hospitalized
# describeFactors(data_vaxPP$do11_visit_2mope___4) 
# describeFactors(data_vaxPP$do11_visit_2mope___4_BL) #
# describeFactors(data_vaxPP$do11_visit_2mope___4_2mo)

data_vaxPP <- data_vaxPP %>% 
  mutate(dose1_hosp = case_when(
    do11_visittype___4_BL == 1 | do11_visittype___4_2mo == 1 | do11_visittype___4_4mo == 1 | do11_visittype___4_6mo == 1 | do11_visittype___4_8mo == 1 | do11_visittype___4_10mo == 1 | do11_visittype___4_14mo == 1 ~ 1,
    TRUE ~ 0
  ),
  dose2_hosp = case_when(
    dt11_visittype___4_BL == 1 | dt11_visittype___4_2mo == 1 | dt11_visittype___4_4mo == 1 | dt11_visittype___4_6mo == 1 | dt11_visittype___4_8mo == 1 | dt11_visittype___4_10mo == 1 | dt11_visittype___4_14mo == 1  ~ 1,
    TRUE ~ 0
  ),
  dose3_hosp = case_when(
    dt11_visittype_vb___4_BL == 1 | dt11_visittype_vb___4_2mo == 1 | dt11_visittype_vb___4_4mo == 1 | dt11_visittype_vb___4_6mo == 1 | dt11_visittype_vb___4_8mo == 1 | dt11_visittype_vb___4_10mo == 1 | dt11_visittype_vb___4_14mo == 1  ~ 1,
    TRUE ~ 0
  ))

## icu
# describeFactors(data_vaxPP$do18_icu) 
# describeFactors(data_vaxPP$dt18_icu_BL) 
# describeFactors(data_vaxPP$dt18_icu_2mo)

data_vaxPP <- data_vaxPP %>% 
  mutate(dose1_icu= case_when(
    do18_icu_BL == 1  ~ 1,
    TRUE ~ 0
  ),
  dose2_icu = case_when(
    dt18_icu_BL == 1 | dt18_icu_2mo == 1 ~ 1,
    TRUE ~ 0
  ),
  dose3_icu = case_when(
    dt18_icu_vb_BL == 1    ~ 1,
    TRUE ~ 0
  ))

## what were the problems described?
describeFactors(data_vaxPP$do12_diagnosis_BL[which(data_vaxPP$vacc_in_preg == "pp vaccine")]) #4
describeFactors(data_vaxPP$do12_diagnosis_4mo[which(data_vaxPP$vacc_in_preg == "pp vaccine")]) # 0


data_vaxPP$dose1_problems <- paste0(data_vaxPP$do13_diagnosis_yes_BL)

data_vaxPP$dose2_problems <- paste0(data_vaxPP$dt13_diagnosis_yes_BL, data_vaxPP$dt13_diagnosis_yes_2mo, data_vaxPP$dt13_diagnosis_yes_6mo, sep = "_")
data_vaxPP$dose2_problems <- gsub("NA", "", data_vaxPP$dose2_problems)
data_vaxPP$dose2_problems <- gsub(" _", "", data_vaxPP$dose2_problems)
data_vaxPP$dose2_problems <- gsub("_", "", data_vaxPP$dose2_problems)

data_vaxPP$dose3_problems <- paste0(data_vaxPP$dt13_diagnosis_yes_vb_BL, data_vaxPP$dt13_diagnosis_yes_vb_2mo, data_vaxPP$dt13_diagnosis_yes_vb_6mo, data_vaxPP$dt13_diagnosis_yes_vb_8mo, data_vaxPP$dt13_diagnosis_yes_vb_10mo, data_vaxPP$dt13_diagnosis_yes_vb_14mo, sep = "_")
data_vaxPP$dose3_problems <- gsub("NA", "", data_vaxPP$dose3_problems)
data_vaxPP$dose3_problems <- gsub(" _", "", data_vaxPP$dose3_problems)
data_vaxPP$dose3_problems <- gsub("_", "", data_vaxPP$dose3_problems)



## ----plot of problems, fig.width = 8-------------------------------------------------------------------
prob.data <- reshape(data_vaxPP[, c("record_id_BL", "dose1_newprob", "dose1_act","dose1_sawhcp", "dose1_er", "dose1_hosp", "dose1_icu", "dose2_newprob", "dose2_act","dose2_sawhcp", "dose2_er", "dose2_hosp", "dose2_icu", "dose3_newprob", "dose3_act","dose3_sawhcp", "dose3_er", "dose3_hosp", "dose3_icu", "all_dose1_type", "all_dose2_type", "all_dose3_type", "dose1_problems", "dose2_problems", "dose3_problems", "vaccine_timing_fix")],
                   varying = list(
                     newprob = c("dose1_newprob", "dose2_newprob", "dose3_newprob"),
                     act = c("dose1_act", "dose2_act", "dose3_act"),
                     saw_hcp = c("dose1_sawhcp", "dose2_sawhcp", "dose3_sawhcp"),
                     er = c("dose1_er", "dose2_er", "dose3_er"),
                     hosp = c("dose1_hosp", "dose2_hosp", "dose3_hosp"),
                     icu = c("dose1_icu", "dose2_icu", "dose3_icu"),
                     type = c("all_dose1_type", "all_dose2_type", "all_dose3_type"),
                     problems = c("dose1_problems", "dose2_problems", "dose3_problems")
                   ),idvar = "record_id_BL", ids = "record_id_BL", times = c("Dose1", "Dose2", "Dose3"), direction = "long")

tab_xtab(prob.data$dose1_newprob, prob.data$time, show.col.prc = TRUE)

tab_xtab(prob.data$dose1_act, prob.data$time, show.col.prc = TRUE)

tab_xtab(prob.data$dose1_er, prob.data$time, show.col.prc = TRUE)

tab_xtab(prob.data$dose1_hosp, prob.data$time, show.col.prc = TRUE)

tab_xtab(prob.data$dose1_icu, prob.data$time, show.col.prc = TRUE)

describeFactors(prob.data$dose1_problems)



## need to ensure correct denominators
prob.mat <- matrix(nrow = 18, ncol = 3)
colnames(prob.mat) <- c("Effect", "N", "Dose")
prob.mat[, 1] <- rep(c("New or worse\nhealth problem", "Prevented/stopped\nnormal activities", "Saw a healthcare\nprovider", "Emergency room\nUrgent care", "Hospitalized", "ICU admission"), 3)

prob.mat[, 3] <- c(rep("Dose 1", 6), rep("Dose 2", 6), rep("Dose 3", 6))

prob.mat[1, 2] <- sum(prob.data$dose1_newprob[which(prob.data$time == "Dose1")])/length(which(prob.data$time == "Dose1"))
prob.mat[2, 2] <- sum(prob.data$dose1_act[which(prob.data$time == "Dose1")])/length(which(prob.data$time == "Dose1"))
prob.mat[3, 2] <- sum(prob.data$dose1_sawhcp[which(prob.data$time == "Dose1")])/length(which(prob.data$time == "Dose1"))
prob.mat[4, 2] <- sum(prob.data$dose1_er[which(prob.data$time == "Dose1")])/length(which(prob.data$time == "Dose1"))
prob.mat[5, 2] <- sum(prob.data$dose1_hosp[which(prob.data$time == "Dose1")])/length(which(prob.data$time == "Dose1"))
prob.mat[6, 2] <- sum(prob.data$dose1_icu[which(prob.data$time == "Dose1")])/length(which(prob.data$time == "Dose1"))

prob.mat[7, 2] <- sum(prob.data$dose1_newprob[which(prob.data$time == "Dose2")])/length(which(prob.data$time == "Dose2"))
prob.mat[8, 2] <- sum(prob.data$dose1_act[which(prob.data$time == "Dose2")])/length(which(prob.data$time == "Dose2"))
prob.mat[9, 2] <- sum(prob.data$dose1_sawhcp[which(prob.data$time == "Dose2")])/length(which(prob.data$time == "Dose2"))
prob.mat[10, 2] <- sum(prob.data$dose1_er[which(prob.data$time == "Dose2")])/length(which(prob.data$time == "Dose2"))
prob.mat[11, 2] <- sum(prob.data$dose1_hosp[which(prob.data$time == "Dose2")])/length(which(prob.data$time == "Dose2"))
prob.mat[12, 2] <- sum(prob.data$dose1_icu[which(prob.data$time == "Dose2")])/length(which(prob.data$time == "Dose2"))

prob.mat[13, 2] <- sum(prob.data$dose1_newprob[which(prob.data$time == "Dose3")])/length(which(prob.data$time == "Dose3"))
prob.mat[14, 2] <- sum(prob.data$dose1_act[which(prob.data$time == "Dose3")])/length(which(prob.data$time == "Dose3"))
prob.mat[15, 2] <- sum(prob.data$dose1_sawhcp[which(prob.data$time == "Dose3")])/length(which(prob.data$time == "Dose3"))
prob.mat[16, 2] <- sum(prob.data$dose1_er[which(prob.data$time == "Dose3")])/length(which(prob.data$time == "Dose3"))
prob.mat[17, 2] <- sum(prob.data$dose1_hosp[which(prob.data$time == "Dose3")])/length(which(prob.data$time == "Dose3"))
prob.mat[18, 2] <- sum(prob.data$dose1_icu[which(prob.data$time == "Dose3")])/length(which(prob.data$time == "Dose3"))


prob.mat <- as.data.frame(prob.mat)
prob.mat$Effect <- factor(prob.mat$Effect, levels = c("New or worse\nhealth problem", "Prevented/stopped\nnormal activities", "Saw a healthcare\nprovider", "Emergency room\nUrgent care", "Hospitalized", "ICU admission"))

prob.mat$N <- as.numeric(prob.mat$N)
prob.mat$N <- round(prob.mat$N*100, 1)

# quartz(width = 10, height = 4)
ggplot(prob.mat, aes(x = Effect, y = N, fill = Dose, group = Dose)) +
  geom_col(position = position_dodge(), colour = "black") +
  theme_pubr() +
  scale_fill_manual(values = ghibli_palette("KikiMedium")[c(4, 6, 2)]) +
  theme(panel.grid.major.y = element_line(linetype = 2, colour = "grey"),
        panel.grid.minor.y = element_line(linetype = 3, colour = "grey"),
        text = element_text(size = 13),
        axis.text.x = element_text(size = 10)) +
  xlab("") +
  ylab("% of Participants\nreceiving dose during lactation") +
  guides(fill = guide_legend(title = "")) #+
  # annotate("text", x = seq(0.75, 6.25, by = 0.5), y = prob.mat$N[c(1, 7, 2, 8, 3, 9, 4, 10, 5, 11, 6, 12)]+2, label = prob.mat$N[c(1, 7, 2, 8, 3, 9, 4, 10, 5, 11, 6, 12)])



## ----diagnoses-----------------------------------------------------------------------------------------
# describeFactors(data$do12_diagnosis_BL)
# describeFactors(data$do12_diagnosis_2mo)
# 
# describeFactors(data$do13_diagnosis_2moes)
# 
# describeFactors(data$dt12_diagnosis_BL)
# describeFactors(data$dt12_diagnosis_2mo)
# 
# describeFactors(data$dt13_diagnosis_2moes_BL)
# describeFactors(data$dt13_diagnosis_2moes_2mo)



## ----covid infections----------------------------------------------------------------------------------
# describeFactors(data$fu4_covid_pos_BL)
# describeFactors(data$fu4_covid_pos_2mo)
# 
# # are any of these overlapping?
# getDescriptionStatsBy(factor(data$fu4_covid_pos_2mo), data$fu4_covid_pos_BL)
# # no
# 
# ## severity
# describeFactors(data$fu5_covid_severi_2mo_BL)
# describeFactors(data$fu5_covid_severi_2mo_2mo)
# 
# 
# describeFactors(data$regular_followup_complete_BL)
# describeFactors(data$regular_followup_complete_2mo)



## ----follow up time------------------------------------------------------------------------------------
data$demographic_health_pregnancy_survey_timestamp <- as.Date(data$demographic_health_pregnancy_survey_timestamp)
# data$pregnancy_outcomes_timestamp <- as.Date(data$pregnancy_outcomes_timestamp)
data$pregnancy_outcomes_timestamp_BL <- as.Date(data$pregnancy_outcomes_timestamp_BL)
data$pregnancy_outcomes_timestamp_2mo <- as.Date(data$pregnancy_outcomes_timestamp_2mo)
# data$vaccine_outcomes_dose_one_timestamp <- as.Date(data$vaccine_outcomes_dose_one_timestamp)
data$vaccine_outcomes_dose_one_timestamp_BL <- as.Date(data$vaccine_outcomes_dose_one_timestamp_BL)
data$vaccine_outcomes_dose_one_timestamp_2mo <- as.Date(data$vaccine_outcomes_dose_one_timestamp_2mo)
# data$vaccine_outcomes_dose_two_timestamp <- as.Date(data$vaccine_outcomes_dose_two_timestamp)
data$vaccine_outcomes_dose_two_timestamp_BL <- as.Date(data$vaccine_outcomes_dose_two_timestamp_BL)
data$vaccine_outcomes_dose_two_timestamp_2mo <- as.Date(data$vaccine_outcomes_dose_two_timestamp_2mo)
# data$vaccine_outcomes_booster_dose_timestamp <- as.Date(data$vaccine_outcomes_booster_dose_timestamp)
data$vaccine_outcomes_booster_dose_timestamp_BL <- as.Date(data$vaccine_outcomes_booster_dose_timestamp_BL)
data$vaccine_outcomes_booster_dose_timestamp_2mo <- as.Date(data$vaccine_outcomes_booster_dose_timestamp_2mo)

data$bl_po1 <- as.numeric(data$pregnancy_outcomes_timestamp_BL - data$demographic_health_pregnancy_survey_timestamp)
summary(data$bl_po1) # days

data$bl_po2 <- as.numeric(data$pregnancy_outcomes_timestamp_BL - data$demographic_health_pregnancy_survey_timestamp)
summary(data$bl_po2) # days

data$bl_po3 <- as.numeric(data$pregnancy_outcomes_timestamp_2mo - data$demographic_health_pregnancy_survey_timestamp)
summary(data$bl_po3) # days

data$regular_followup_timestamp_4mo <- as.Date(data$regular_followup_timestamp_4mo)
data$regular_followup_timestamp_2mo <- as.Date(data$regular_followup_timestamp_2mo)

data$bl_fu1 <- as.numeric(data$regular_followup_timestamp_2mo - data$demographic_health_pregnancy_survey_timestamp)
summary(data$bl_fu1) # 2months

data$bl_fu2 <- as.numeric(data$regular_followup_timestamp_4mo - data$demographic_health_pregnancy_survey_timestamp)
summary(data$bl_fu2) # 4months

## when did they have their doses?
# data$do3_date <- as.Date(data$do3_date)
data$do3_date_BL <- as.Date(data$do3_date_BL)
data$do3_date_2mo <- as.Date(data$do3_date_2mo)

# data$dt3_date
data$dt3_date_BL
data$dt3_date_2mo

## I guess dose 1 is the important one

# data$dose1_fu <- as.numeric(data$regular_followup_timestamp_BL - data$do3_date) # negative
# summary(data$dose1_fu)

data$dose1_fu_BL <- as.numeric(data$regular_followup_timestamp_BL - data$do3_date_BL) 
summary(data$dose1_fu_BL)

data$dose1_fu_2mo <- as.numeric(data$regular_followup_timestamp_BL - data$do3_date_2mo) 
summary(data$dose1_fu_2mo)


# data$dose1_fu2 <- as.numeric(data$regular_followup_timestamp_2mo - data$do3_date)
# summary(data$dose1_fu2)

data$dose1_fu2_BL <- as.numeric(data$regular_followup_timestamp_2mo - data$do3_date_BL) 
summary(data$dose1_fu2_BL)

data$dose1_fu2_2mo <- as.numeric(data$regular_followup_timestamp_2mo - data$do3_date_2mo) 
summary(data$dose1_fu2_2mo)

# pick longest one per person
d1 <- data.frame(record_id = data$record_id, dose1_fu = data$dose1_fu_BL)
d2 <- data.frame(record_id = data$record_id, dose1_fu = data$dose1_fu_2mo)
d3 <- data.frame(record_id = data$record_id, dose1_fu = data$dose1_fu2)
d4 <- data.frame(record_id = data$record_id, dose1_fu = data$dose1_fu2_BL)
d5 <- data.frame(record_id = data$record_id, dose1_fu = data$dose1_fu2_2mo)

dall <- rbind(d1, d2, d3, d4, d5)

dall <- dall %>% 
  group_by(record_id) %>% 
  arrange(dose1_fu) %>% 
  slice_max(order_by = dose1_fu)

dall$dose1_fu[which(dall$dose1_fu < 0 | dall$dose1_fu > 500)] <- NA

summary(dall$dose1_fu/7)



