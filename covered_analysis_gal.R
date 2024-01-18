#covered analysis:
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
#setwd("\\\\PHSAhome2.phsabc.ehcnet.ca/sela.grays/covered")
getwd()


data <- read.csv("MainSurveyDatabaseCa-VaccineAttitudesIF_DATA_2024-01-12_1718.csv",header = TRUE)

## look at data

length(unique(data$record_id))
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

# #### UPDATE NOVEMBER 28 2023 ####
#### REIMPORTING OTHER PART OF COVERED SURVEY TO INCLUDE WHETHER FILLED OUT DURING PREGNANCY #####

#data_update <- read.csv("covered_11-28-2023.csv", header = TRUE)



#data_update <- data_update[-which(data_update$withdraw_answer == 1), ]

#ids.can2 <- data_update$record_id[which(data_update$bl16_country_res == 1)]

#length(unique(data_update$record_id))

#data_update_bl <- data_update[which(data_update$redcap_event_name == "baseline_arm_1" & data_update$record_id %in% ids.can2), ]
#data_update_2mo <- data_update[which(data_update$redcap_event_name == "followup_2_months_arm_1" & data_update$record_id %in% ids.can2), ]
#data_update_4mo <- data_update[which(data_update$redcap_event_name == "followup_4_months_arm_1" & data_update$record_id %in% ids.can2), ]
#data_update_6mo <- data_update[which(data_update$redcap_event_name == "followup_6_months_arm_1" & data_update$record_id %in% ids.can2), ]
#data_update_8mo <- data_update[which(data_update$redcap_event_name == "followup_8_months_arm_1" & data_update$record_id %in% ids.can2), ]
#data_update_10mo <- data_update[which(data_update$redcap_event_name == "followup_10_months_arm_1" & data_update$record_id %in% ids.can2), ]
#data_update_14mo <- data_update[which(data_update$redcap_event_name == "followup_14_months_arm_1" & data_update$record_id %in% ids.can2), ]

# followup_10_months_arm_1 "3,522"
# followup_14_months_arm_1 "411"
# followup_2_months_arm_1  "4,992"
# followup_4_months_arm_1  "4,725"
# followup_6_months_arm_1  "4,399"
# followup_8_months_arm_1  "3,975"

## clear out empty rows and columns
### some columns are entirely "" rather than NA
#data_update_bl <- data_update_bl %>%
#  mutate(across(where(is.factor), ~as.character(.x))) %>%
#  mutate(across(where(is.character), ~ifelse(.x == "", NA_character_, .)))

#data_update_2mo <- data_update_2mo %>%
#  mutate(across(where(is.factor), ~as.character(.x))) %>%
#  mutate(across(where(is.character), ~ifelse(.x == "", NA_character_, .)))

#data_update_4mo <- data_update_4mo %>%
#  mutate(across(where(is.factor), ~as.character(.x))) %>%
#  mutate(across(where(is.character), ~ifelse(.x == "", NA_character_, .)))

#data_update_6mo <- data_update_6mo %>%
#  mutate(across(where(is.factor), ~as.character(.x))) %>%
#  mutate(across(where(is.character), ~ifelse(.x == "", NA_character_, .)))

#data_update_8mo <- data_update_8mo %>%
#  mutate(across(where(is.factor), ~as.character(.x))) %>%
#  mutate(across(where(is.character), ~ifelse(.x == "", NA_character_, .)))

#data_update_10mo <- data_update_10mo %>%
#  mutate(across(where(is.factor), ~as.character(.x))) %>%
#  mutate(across(where(is.character), ~ifelse(.x == "", NA_character_, .)))

#data_update_14mo <- data_update_14mo %>%
#  mutate(across(where(is.factor), ~as.character(.x))) %>%
#  mutate(across(where(is.character), ~ifelse(.x == "", NA_character_, .)))

#data_update_bl <- janitor::remove_empty(data_update_bl)
#data_update_2mo <- janitor::remove_empty(data_update_2mo)
#data_update_4mo <- janitor::remove_empty(data_update_4mo)
#data_update_6mo <- janitor::remove_empty(data_update_6mo)
#data_update_8mo <- janitor::remove_empty(data_update_8mo)
#data_update_10mo <- janitor::remove_empty(data_update_10mo)
#data_update_14mo <- janitor::remove_empty(data_update_14mo)

# find variable names in common
#colnames(data_update_bl) <- paste0(colnames(data_update_bl), "_BL")
#colnames(data_update_2mo) <- paste0(colnames(data_update_2mo), "_2mo")
#colnames(data_update_4mo) <- paste0(colnames(data_update_4mo), "_4mo")
#colnames(data_update_6mo) <- paste0(colnames(data_update_6mo), "_6mo")
#colnames(data_update_8mo) <- paste0(colnames(data_update_8mo), "_8mo")
#colnames(data_update_10mo) <- paste0(colnames(data_update_10mo), "_10mo")
#colnames(data_update_14mo) <- paste0(colnames(data_update_14mo), "_14mo")

#data_update_bl$record_id <- data_update_bl$record_id_BL
#data_update_2mo$record_id <- data_update_2mo$record_id_2mo
#data_update_4mo$record_id <- data_update_4mo$record_id_4mo
#data_update_6mo$record_id <- data_update_6mo$record_id_6mo
#data_update_8mo$record_id <- data_update_8mo$record_id_8mo
#data_update_10mo$record_id <- data_update_10mo$record_id_10mo
#data_update_14mo$record_id <- data_update_14mo$record_id_14mo


#data_update_use <- left_join(data_update_bl, data_update_2mo, by = "record_id")
#data_update_use <- left_join(data_update_use, data_update_4mo, by = "record_id")
#data_update_use <- left_join(data_update_use, data_update_6mo, by = "record_id")
#data_update_use <- left_join(data_update_use, data_update_8mo, by = "record_id")
#data_update_use <- left_join(data_update_use, data_update_10mo, by = "record_id")
#data_update_use <- left_join(data_update_use, data_update_14mo, by = "record_id")




# 
# #### UPDATE NOVEMBER 28 2023 - EXCLUDE VACCINATED DURING LACTATION (only pregnancy) ####
#dim(data_use)
#dim(data_update_use)
#dim(data_update)
dim(data)
#length(unique(data_use$record_id_BL))
#length(unique(data_update_use$record_id_BL))

#sum(data_use$record_id_BL %in% unique(data_update_use$record_id_BL))

#data_update_use2 <- data_update_use[,-which(colnames(data_update_use) %in% colnames(data_use)[-1])]

#data_use <- inner_join(data_use,data_update_use, by = "record_id_BL")

#dim(data_use)
## check to see how many people had all three doses during pregnancy manually:
## dose 1 during pregnancy checked using:

## BASELINE

#VACCINATED IN PREG
data_use[which(data_use$record_id %in% ids.can.EDD.vacc),]

#UNVACCINATED IN PREG 
data_use[-which(data_use$record_id %in% ids.can.EDD.vacc),]

#sum(data_use$bl9_dose2_timing == 2, na.rm = TRUE)
## 1601 selected "first dose during pregnancy" using the checklist/ radio
# sum(data_use$do2_timing2___2_BL)
## 1383 selected "first dose during pregnancy" using the other (radio/ checklist, not sure which)

##2 Month
#sum(data_use$do2_timing2___2_2mo,na.rm = TRUE)
## 33 selected "first dose during pregnancy" using the other (radio/ checklist, not sure which) after 2 months

## 4 month
#sum(data_use$do2_timing2___2_4mo, na.rm = TRUE)
# 3 selected it after 4 months

## 6 months
#sum(data_use$do2_timing2___2_6mo, na.rm = TRUE)
#1 selected it after 6 months


## 8 months
#sum(data_use$do2_timing2___2_8mo, na.rm = TRUE)
#0 selected it after 8 months

## 10 months
#sum(data_use$do2_timing2___2_10mo, na.rm = TRUE)
#0 selected it after 10 months


## 14 months
#sum(data_use$do2_timing2___2_14mo, na.rm = TRUE)
#0 selected it after 14 months




#sum(data_use$dt2_timing_BL == 2, na.rm = TRUE)
## 1211 selected "second dose during pregnancy" using the checklist/ radio
#sum(data_use$dt2_timing2___2_BL)
## 1230 selected "second dose during pregnancy" using the other (radio/ checklist, not sure which)

#sum(data_use$dt2_timing_vb == 2, na.rm = TRUE)
## - selected
#sum(data_use$dt2_timing2_vb___2_BL)
## 300 had booster dose during pregnancy


## checking all three:
#sum(data_use$do2_timing2___2_BL*data_use$dt2_timing2___2_BL*data_use$dt2_timing2_vb___2_BL)


data_use <- data_use %>%
  mutate(
    had_dose_1 = case_when(
      do2_timing_BL == 2 ~ TRUE,
      do2_timing2___2_BL == 1  ~ TRUE,
      do2_timing2___2_2mo  == 1 ~ TRUE,
      do2_timing2___2_4mo  == 1 ~ TRUE,
      do2_timing2___2_6mo == 1  ~ TRUE,
      do2_timing2___2_8mo  == 1 ~ TRUE,
      do2_timing2___2_10mo  == 1 ~ TRUE,
      do2_timing2___2_14mo  == 1 ~ TRUE
    )
  )

# data_use <- data_use %>%
#   mutate(
#     had_dose_2 = case_when(
#       dt2_timing_BL == 2 ~ TRUE,
#       dt2_timing2___2_BL == 1  ~ TRUE,
#       dt2_timing2___2_2mo  == 1 ~ TRUE,
#       dt2_timing2___2_4mo  == 1 ~ TRUE,
#       dt2_timing2___2_6mo == 1  ~ TRUE,
#       dt2_timing2___2_8mo  == 1 ~ TRUE,
#       dt2_timing2___2_10mo  == 1 ~ TRUE,
#       dt2_timing2___2_14mo  == 1 ~ TRUE
#     )
#   )
# 
# data_use <- data_use %>%
#   mutate(
#     had_dose_3 = case_when(
#       dt2_timing2_vb___2_BL == 1  ~ TRUE,
#       dt2_timing2_vb___2_2mo  == 1 ~ TRUE,
#       dt2_timing2_vb___2_4mo  == 1 ~ TRUE,
#       dt2_timing2_vb___2_6mo == 1  ~ TRUE,
#       dt2_timing2_vb___2_8mo  == 1 ~ TRUE,
#       dt2_timing2_vb___2_10mo  == 1 ~ TRUE,
#       dt2_timing2_vb___2_14mo  == 1 ~ TRUE
#     )
#   )
# 
# 
# #### WHO HAD ALL 3 DOSES DURING PREGNANCY (153 cases TOTAL)
# data_use$all_3_during_preg <- data_use$had_dose_1*data_use$had_dose_2*data_use$had_dose_3

# data_use <- data_use %>%
#   mutate(
#     new_preg = case_when(
#       data_use$fu1d_confirmnewpreg_2mo == 1  ~ TRUE,
#       data_use$fu1d_confirmnewpreg_4mo == 1  ~ TRUE,
#       data_use$fu1d_confirmnewpreg_6mo == 1  ~ TRUE,
#       data_use$fu1d_confirmnewpreg_8mo == 1  ~ TRUE,
#       data_use$fu1d_confirmnewpreg_10mo == 1  ~ TRUE,
#       data_use$fu1d_confirmnewpreg_14mo == 1  ~ TRUE
#     )
#   )
# 
# summary(as.factor(data_use$new_preg))
# 
# sum(data_use$new_preg*data_use$all_3_during_preg, na.rm = TRUE)
# 
# data_use <- data_use %>%
#   mutate(
#     dose_3_date = case_when(
#       !is.na(data_use$dt3_date_vb_BL)  ~ dt3_date_vb_BL,
#       !is.na(data_use$dt3_date_vb_2mo) ~ dt3_date_vb_2mo,
#       !is.na(data_use$dt3_date_vb_4mo) ~ dt3_date_vb_4mo,
#       !is.na(data_use$dt3_date_vb_6mo) ~ dt3_date_vb_6mo,
#       !is.na(data_use$dt3_date_vb_8mo) ~ dt3_date_vb_8mo,
#       !is.na(data_use$dt3_date_vb_10mo)  ~ dt3_date_vb_10mo,
#       !is.na(data_use$dt3_date_vb_14mo) ~ dt3_date_vb_14mo
#     )
#   )
# 
# 
# data_use <- data_use %>%
#   mutate(
#     dose_2_date = case_when(
#       !is.na(data_use$dt3_date_BL)  ~ dt3_date_BL,
#       !is.na(data_use$dt3_date_2mo) ~ dt3_date_2mo,
#       !is.na(data_use$dt3_date_4mo) ~ dt3_date_4mo,
#       !is.na(data_use$dt3_date_6mo) ~ dt3_date_6mo,
#       !is.na(data_use$dt3_date_8mo) ~ dt3_date_8mo
#     )
#   )

# 
# data_use <- data_use %>%
#   mutate(
#     dose_1_date = case_when(
#       !is.na(data_use$do3_date_BL)  ~ do3_date_BL,
#       !is.na(data_use$do3_date_2mo) ~ do3_date_2mo,
#       !is.na(data_use$do3_date_4mo) ~ do3_date_4mo,
#       !is.na(data_use$do3_date_6mo) ~ do3_date_6mo,
#       !is.na(data_use$do3_date_8mo) ~ do3_date_8mo
#     )
#   )

# 
# data_use$dose_1_date <- as.Date(data_use$dose_1_date)
# data_use$dose_2_date <- as.Date(data_use$dose_2_date)
# data_use$dose_3_date <- as.Date(data_use$dose_3_date)

## ISOLATE THOSE WHO REPORTED GETTING ALL 3 DOSES DURING PREGNANCY AND LOOK AT THEIR DATES:


# all_3_data <- data_use %>% filter(all_3_during_preg == TRUE)
# dim(all_3_data)
# 
# 
# all_3_data <- all_3_data %>%
#   mutate(
#     dose_3_date = case_when(
#       !is.na(all_3_data$dt3_date_vb_BL)  ~ dt3_date_vb_BL,
#       !is.na(all_3_data$dt3_date_vb_2mo) ~ dt3_date_vb_2mo,
#       !is.na(all_3_data$dt3_date_vb_4mo) ~ dt3_date_vb_4mo,
#       !is.na(all_3_data$dt3_date_vb_6mo) ~ dt3_date_vb_6mo,
#       !is.na(all_3_data$dt3_date_vb_8mo) ~ dt3_date_vb_8mo,
#       !is.na(all_3_data$dt3_date_vb_10mo)  ~ dt3_date_vb_10mo,
#       !is.na(all_3_data$dt3_date_vb_14mo) ~ dt3_date_vb_14mo
#     )
#   )
# 
# 
# all_3_data <- all_3_data %>%
#   mutate(
#     dose_2_date = case_when(
#       !is.na(all_3_data$dt3_date_BL)  ~ dt3_date_BL,
#       !is.na(all_3_data$dt3_date_2mo) ~ dt3_date_2mo,
#       !is.na(all_3_data$dt3_date_4mo) ~ dt3_date_4mo,
#       !is.na(all_3_data$dt3_date_6mo) ~ dt3_date_6mo,
#       !is.na(all_3_data$dt3_date_8mo) ~ dt3_date_8mo
#     )
#   )
# 
# 
# all_3_data <- all_3_data %>%
#   mutate(
#     dose_1_date = case_when(
#       !is.na(all_3_data$do3_date_BL)  ~ do3_date_BL,
#       !is.na(all_3_data$do3_date_2mo) ~ do3_date_2mo,
#       !is.na(all_3_data$do3_date_4mo) ~ do3_date_4mo,
#       !is.na(all_3_data$do3_date_6mo) ~ do3_date_6mo,
#       !is.na(all_3_data$do3_date_8mo) ~ do3_date_8mo
#     )
#   )
# 
# 
# all_3_data$dose_1_date <- as.Date(all_3_data$dose_1_date)
# all_3_data$dose_2_date <- as.Date(all_3_data$dose_2_date)
# all_3_data$dose_3_date <- as.Date(all_3_data$dose_3_date)
# 
# sum(all_3_data$dose_2_date == all_3_data$dose_3_date, na.rm = TRUE)
# 
# all_3_data$date_diff <- all_3_data$dose_3_date - all_3_data$dose_1_date
# all_3_data$date_diff_weeks <- all_3_data$date_diff/7
# library(ggplot2)
# ggplot(all_3_data, aes(x = date_diff_weeks,  fill = new_preg)) + geom_histogram()
# 
# unique(all_3_data$dt3_date_vb_BL) ## "booster" dose date
# unique(all_3_data$dt3_date_vb_2mo) ## "booster" dose date
# unique(all_3_data$dt3_date_vb_4mo) ## "booster" dose date
# unique(all_3_data$dt3_date_vb_4mo) ## "booster" dose date
# 
# 
# all_3_data$do3_date_BL ## first dose date
# all_3_data$dt3_date_BL ## second dose date
# 
# sum(all_3_data$date_diff_weeks <= 0, na.rm = TRUE)
# sum(all_3_data$date_diff_weeks >= 0 & all_3_data$date_diff_weeks < 20, na.rm = TRUE)
# sum(all_3_data$date_diff_weeks >= 20 & all_3_data$date_diff_weeks < 40, na.rm = TRUE)
# sum(all_3_data$date_diff_weeks >= 40, na.rm = TRUE)
# sum(is.na(all_3_data$date_diff_weeks))

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
#   mutate(
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
# exclude "953 (16.8%)"
# include "4,735 (83.2%)"

# describeFactors(data_use$i1_dob1_BL)

tab_xtab(data_use$BABY_HAS_DOB_BL, data_use$include)

tab_xtab(data_use$dose1_T[which(data_use$BABY_HAS_DOB_BL == "Yes")], data_use$dose2_T[which(data_use$BABY_HAS_DOB_BL == "Yes")])


data_use <- data_use %>%
  mutate(
    dose3_T = case_when(
      dose1_T == "No vaccine NOT pregnant" ~ "No vaccine NOT pregnant",
      dose1_T == "No vaccine pregnant" ~ "No vaccine pregnant",
      bl8_dose2_BL == 0 ~ "No D2 or D3",
      dt2_timing2_vb___2_BL == 1 ~ "D3 during preg",
      dt2_timing2_vb___1_BL == 1 ~ "D3 before preg",
      dt2_timing2_vb___3_BL == 1 ~ "D3 post-partum/BF",
      dt2_timing2_vb___4_BL == 1 ~ "D3 post-partum/Not BF"
    )
  )

describeFactors(data_use$dose3_T)

tab_xtab(data_use$dose1_T, data_use$dose3_T)
tab_xtab(data_use$dose1_T, data_use$dose2_T)

#########
#### DOSE SPECIFIC SURVEYS WERE ONLY ASKED ONCE SO I THINK WE CAN COMBINE THEM ACROSS TIME POINTS FOR EACH PERSON
# IF DOSE 1 WAS BEFORE PREG AT BASELINE THEN EXCLUDE THEM
# IF D1 DURING PREG AT BASELINE THEN INCLUDE THEM
data_use <- data_use %>%
  mutate(include_BL = case_when(
    dose1_T == "D1 before preg" ~ "exclude",
    dose1_T == "D1 during preg" & dose2_T == "D2 during preg" & dose3_T == "D3 during preg" ~ "include D1&2&3 in preg",
    dose1_T == "D1 during preg" & dose2_T == "D2 during preg" ~ "D1&2 in preg",
    dose1_T == "D1 during preg" ~ "include D1 in preg",
    grepl("post", dose1_T) & dose2_T %in% c("D2 before preg", "D2 during preg") & dose3_T %in% c("D3 before preg", "D3 during preg") ~ "exclude",
    grepl("post", dose1_T) & dose2_T %in% c("D2 before preg", "D2 during preg") ~ "exclude",
    grepl("post", dose1_T) ~ "include D1 pp",
    dose1_T == "No vaccine NOT pregnant" ~ "exclude not preg",
    dose1_T == "No vaccine pregnant" ~ "include no vax BL"
  ))
describeFactors(data_use$include_BL)
# D1&2 in preg           "2,071 (36.4%)"
# exclude                "784 (13.8%)"
# exclude not preg       "59 (1.0%)"
# include D1 in preg     "892 (15.7%)"
# include D1 pp          "1,645 (28.9%)"
# include D1&2&3 in preg "20 (0.4%)"
# include no vax BL      "89 (1.6%)"
# Missing                "128 (2.3%)"

### then we need to look at self-reported timing for subsequent follow up and doses

data_use <- data_use %>%
  mutate(
    dose1_T_2mo = case_when(
      do2_timing2___2_2mo == 1 ~ "D1 during preg",
      do2_timing2___1_2mo == 1 ~ "D1 before preg",
      do2_timing2___3_2mo == 1 ~ "D1 post-partum/BF",
      do2_timing2___4_2mo == 1 ~ "D1 post-partum/Not BF",
      TRUE ~ NA_character_
    )
  )

describeFactors(data_use$dose1_T_2mo)
tab_xtab(data_use$include_BL, data_use$dose1_T_2mo)

data_use <- data_use %>%
  mutate(
    dose2_T_2mo = case_when(
      dt2_timing2___2_2mo == 1 ~ "D2 during preg",
      dt2_timing2___1_2mo == 1 ~ "D2 before preg",
      dt2_timing2___3_2mo == 1 ~ "D2 post-partum/BF",
      dt2_timing2___4_2mo == 1 ~ "D2 post-partum/Not BF",
      TRUE ~ NA_character_
    )
  )

describeFactors(data_use$dose2_T_2mo)
tab_xtab(data_use$include_BL, data_use$dose2_T_2mo)

## nothing surprising here
data_use <- data_use %>%
  mutate(
    dose3_T_2mo = case_when(
      dt2_timing2_vb___2_2mo == 1 ~ "D3 during preg",
      dt2_timing2_vb___1_2mo == 1 ~ "D3 before preg",
      dt2_timing2_vb___3_2mo == 1 ~ "D3 post-partum/BF",
      dt2_timing2_vb___4_2mo == 1 ~ "D3 post-partum/Not BF",
      TRUE ~ NA_character_
    )
  )

describeFactors(data_use$dose3_T_2mo)
tab_xtab(data_use$include_BL, data_use$dose3_T_2mo) # 10 will need to be excluded as they indicated D3 before preg with D1 in preg (n=1), and D1pp (n = 8)

# data_use <- data_use %>%
#   mutate(include_2mo = case_when(
#     grepl("exclude", include_BL) ~ "exclude",
#     include_BL == "include D1 in preg" &
#   ))
# describeFactors(data_use$include_BL)


###### Thinking about this in a different way. Can we determine when someone has entered data into the pregnancy outcomes survey?
# View(data_use[, c("record_id", "pregnancy_outcomes_timestamp_BL", "pregnancy_outcomes_timestamp_2mo", "pregnancy_outcomes_timestamp_4mo", "pregnancy_outcomes_timestamp_6mo", "pregnancy_outcomes_timestamp_8mo", "pregnancy_outcomes_timestamp_10mo", "pregnancy_outcomes_timestamp_14mo")])

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

# View(data_use[, c("record_id", "pregnancy_outcomes_timestamp_BL", "pregnancy_outcomes_timestamp_2mo", "pregnancy_outcomes_timestamp_4mo", "pregnancy_outcomes_timestamp_6mo", "pregnancy_outcomes_timestamp_8mo", "pregnancy_outcomes_timestamp_10mo", "pregnancy_outcomes_timestamp_14mo", "which_time_preg_out")])

describeFactors(data_use$which_time_preg_out)
# 1               "2,865 (50.4%)"
# 2               "860 (15.1%)"
# 3               "706 (12.4%)"
# 4               "536 (9.4%)"
# 5               "199 (3.5%)"
# 6               "22 (0.4%)"
# 7               "2 (0.0%)"
# No preg outcome "498 (8.8%)"

### this variable lets us choose which of the survey times need to be considered for deciding on the vaccination status and timing in pregnancy

tab_xtab(data_use$which_time_preg_out, data_use$include_BL)
# tab_xtab(data_use$which_time_preg_out, data_use$include_2mo)

#### then I think we can paste all the results for doses together?
data_use <- data_use %>%
  mutate(
    dose1_T_4mo = case_when(
      do2_timing2___2_4mo == 1 ~ "D1 during preg",
      do2_timing2___1_4mo == 1 ~ "D1 before preg",
      do2_timing2___3_4mo == 1 ~ "D1 post-partum/BF",
      do2_timing2___4_4mo == 1 ~ "D1 post-partum/Not BF",
      TRUE ~ NA_character_
    )
  )

describeFactors(data_use$dose1_T_4mo)
tab_xtab(data_use$include_BL, data_use$dose1_T_4mo)

data_use <- data_use %>%
  mutate(
    dose2_T_4mo = case_when(
      dt2_timing2___2_4mo == 1 ~ "D2 during preg",
      dt2_timing2___1_4mo == 1 ~ "D2 before preg",
      dt2_timing2___3_4mo == 1 ~ "D2 post-partum/BF",
      dt2_timing2___4_4mo == 1 ~ "D2 post-partum/Not BF",
      TRUE ~ NA_character_
    )
  )

describeFactors(data_use$dose2_T_4mo)
tab_xtab(data_use$include_BL, data_use$dose2_T_4mo)

data_use <- data_use %>%
  mutate(
    dose3_T_4mo = case_when(
      dt2_timing2_vb___2_4mo == 1 ~ "D3 during preg",
      dt2_timing2_vb___1_4mo == 1 ~ "D3 before preg",
      dt2_timing2_vb___3_4mo == 1 ~ "D3 post-partum/BF",
      dt2_timing2_vb___4_4mo == 1 ~ "D3 post-partum/Not BF",
      TRUE ~ NA_character_
    )
  )

describeFactors(data_use$dose3_T_4mo)
tab_xtab(data_use$include_BL, data_use$dose3_T_4mo)

### 6 mo
data_use <- data_use %>%
  mutate(
    dose1_T_6mo = case_when(
      do2_timing2___2_6mo == 1 ~ "D1 during preg",
      do2_timing2___1_6mo == 1 ~ "D1 before preg",
      do2_timing2___3_6mo == 1 ~ "D1 post-partum/BF",
      do2_timing2___4_6mo == 1 ~ "D1 post-partum/Not BF",
      TRUE ~ NA_character_
    )
  )

describeFactors(data_use$dose1_T_6mo)
# tab_xtab(data_use$include_BL, data_use$dose1_T_6mo)

data_use <- data_use %>%
  mutate(
    dose2_T_6mo = case_when(
      dt2_timing2___2_6mo == 1 ~ "D2 during preg",
      dt2_timing2___1_6mo == 1 ~ "D2 before preg",
      dt2_timing2___3_6mo == 1 ~ "D2 post-partum/BF",
      dt2_timing2___4_6mo == 1 ~ "D2 post-partum/Not BF",
      TRUE ~ NA_character_
    )
  )

describeFactors(data_use$dose2_T_6mo)
# tab_xtab(data_use$include_BL, data_use$dose2_T_6mo)

data_use <- data_use %>%
  mutate(
    dose3_T_6mo = case_when(
      dt2_timing2_vb___2_6mo == 1 ~ "D3 during preg",
      dt2_timing2_vb___1_6mo == 1 ~ "D3 before preg",
      dt2_timing2_vb___3_6mo == 1 ~ "D3 post-partum/BF",
      dt2_timing2_vb___4_6mo == 1 ~ "D3 post-partum/Not BF",
      TRUE ~ NA_character_
    )
  )

describeFactors(data_use$dose3_T_6mo)
# tab_xtab(data_use$include_BL, data_use$dose3_T_6mo)


### 8 mo
data_use <- data_use %>%
  mutate(
    dose1_T_8mo = case_when(
      do2_timing2___2_8mo == 1 ~ "D1 during preg",
      do2_timing2___1_8mo == 1 ~ "D1 before preg",
      do2_timing2___3_8mo == 1 ~ "D1 post-partum/BF",
      do2_timing2___4_8mo == 1 ~ "D1 post-partum/Not BF",
      TRUE ~ NA_character_
    )
  )

describeFactors(data_use$dose1_T_8mo)
# tab_xtab(data_use$include_BL, data_use$dose1_T_8mo)

data_use <- data_use %>%
  mutate(
    dose2_T_8mo = case_when(
      dt2_timing2___2_8mo == 1 ~ "D2 during preg",
      dt2_timing2___1_8mo == 1 ~ "D2 before preg",
      dt2_timing2___3_8mo == 1 ~ "D2 post-partum/BF",
      dt2_timing2___4_8mo == 1 ~ "D2 post-partum/Not BF",
      TRUE ~ NA_character_
    )
  )

describeFactors(data_use$dose2_T_8mo)
# tab_xtab(data_use$include_BL, data_use$dose2_T_8mo)

data_use <- data_use %>%
  mutate(
    dose3_T_8mo = case_when(
      dt2_timing2_vb___2_8mo == 1 ~ "D3 during preg",
      dt2_timing2_vb___1_8mo == 1 ~ "D3 before preg",
      dt2_timing2_vb___3_8mo == 1 ~ "D3 post-partum/BF",
      dt2_timing2_vb___4_8mo == 1 ~ "D3 post-partum/Not BF",
      TRUE ~ NA_character_
    )
  )

describeFactors(data_use$dose3_T_8mo)
# tab_xtab(data_use$include_BL, data_use$dose3_T_8mo)



### 10 mo
data_use <- data_use %>%
  mutate(
    dose1_T_10mo = case_when(
      do2_timing2___2_10mo == 1 ~ "D1 during preg",
      do2_timing2___1_10mo == 1 ~ "D1 before preg",
      do2_timing2___3_10mo == 1 ~ "D1 post-partum/BF",
      do2_timing2___4_10mo == 1 ~ "D1 post-partum/Not BF",
      TRUE ~ NA_character_
    )
  )

describeFactors(data_use$dose1_T_10mo)
# tab_xtab(data_use$include_BL, data_use$dose1_T_10mo)

data_use <- data_use %>%
  mutate(
    dose2_T_10mo = case_when(
      dt2_timing2___2_10mo == 1 ~ "D2 during preg",
      dt2_timing2___1_10mo == 1 ~ "D2 before preg",
      dt2_timing2___3_10mo == 1 ~ "D2 post-partum/BF",
      dt2_timing2___4_10mo == 1 ~ "D2 post-partum/Not BF",
      TRUE ~ NA_character_
    )
  )

describeFactors(data_use$dose2_T_10mo)
# tab_xtab(data_use$include_BL, data_use$dose2_T_10mo)

data_use <- data_use %>%
  mutate(
    dose3_T_10mo = case_when(
      dt2_timing2_vb___2_10mo == 1 ~ "D3 during preg",
      dt2_timing2_vb___1_10mo == 1 ~ "D3 before preg",
      dt2_timing2_vb___3_10mo == 1 ~ "D3 post-partum/BF",
      dt2_timing2_vb___4_10mo == 1 ~ "D3 post-partum/Not BF",
      TRUE ~ NA_character_
    )
  )

describeFactors(data_use$dose3_T_10mo)
# tab_xtab(data_use$include_BL, data_use$dose3_T_10mo)


### 14 mo
data_use <- data_use %>%
  mutate(
    dose1_T_14mo = case_when(
      do2_timing2___2_14mo == 1 ~ "D1 during preg",
      do2_timing2___1_14mo == 1 ~ "D1 before preg",
      do2_timing2___3_14mo == 1 ~ "D1 post-partum/BF",
      do2_timing2___4_14mo == 1 ~ "D1 post-partum/Not BF",
      TRUE ~ NA_character_
    )
  )

describeFactors(data_use$dose1_T_14mo)
# tab_xtab(data_use$include_BL, data_use$dose1_T_14mo)

data_use <- data_use %>%
  mutate(
    dose2_T_14mo = case_when(
      dt2_timing2___2_14mo == 1 ~ "D2 during preg",
      dt2_timing2___1_14mo == 1 ~ "D2 before preg",
      dt2_timing2___3_14mo == 1 ~ "D2 post-partum/BF",
      dt2_timing2___4_14mo == 1 ~ "D2 post-partum/Not BF",
      TRUE ~ NA_character_
    )
  )

describeFactors(data_use$dose2_T_14mo)
# tab_xtab(data_use$include_BL, data_use$dose2_T_14mo)

data_use <- data_use %>%
  mutate(
    dose3_T_14mo = case_when(
      dt2_timing2_vb___2_14mo == 1 ~ "D3 during preg",
      dt2_timing2_vb___1_14mo == 1 ~ "D3 before preg",
      dt2_timing2_vb___3_14mo == 1 ~ "D3 post-partum/BF",
      dt2_timing2_vb___4_14mo == 1 ~ "D3 post-partum/Not BF",
      TRUE ~ NA_character_
    )
  )

describeFactors(data_use$dose3_T_14mo)
# tab_xtab(data_use$include_BL, data_use$dose3_T_14mo)

###### exclusions
# 1. exclude everyone who does not have a pregnancy outcomes survey filled in
data_ex1 <- data_use[-which(data_use$which_time_preg_out == "No preg outcome"), ] # 5688 to 5190
describeFactors(data_ex1$include_BL)
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


#### now we need to sort out vaccine timing - will need to triple check the 3 doses in pregnancy people
data_ex2 <- data_ex2 %>%
  mutate(
    vaccine_timing = case_when(
      grepl("post", all_dose1_info) & grepl("post", all_dose2_info) & grepl("post", all_dose3_info) ~ "D123 pp",
      grepl("during", all_dose1_info) & grepl("post", all_dose2_info) & grepl("post", all_dose3_info) ~ "D1 preg, D23 pp",
      grepl("during", all_dose1_info) & grepl("during", all_dose2_info) & grepl("post", all_dose3_info) ~ "D12 in preg, D3 pp",
      grepl("during", all_dose1_info) & grepl("during", all_dose2_info) & grepl("during", all_dose3_info) ~ "D123 in preg",
      grepl("during", all_dose1_info) & grepl("post", all_dose2_info) ~ "D1 in preg, D2 pp, no D3",
      grepl("during", all_dose1_info) & grepl("during", all_dose2_info) ~ "D12 in preg, no D3",
      include_BL == "include D1 in preg" ~ "D1 in preg, no D23",
      include_BL == "include D1 pp" ~ "D1 pp, no D23",
      TRUE ~ include_BL
    )
  )

describeFactors(data_ex2$vaccine_timing)
# D1 in preg, D2 pp, no D3 "140 (3.1%)"
# D1 in preg, no D23       "73 (1.6%)"
# D1 pp, no D23            "623 (13.7%)"
# D1 preg, D23 pp          "466 (10.2%)"
# D12 in preg, D3 pp       "1,604 (35.3%)"
# D12 in preg, no D3       "474 (10.4%)"
# D123 in preg             "123 (2.7%)"
# D123 pp                  "1,006 (22.1%)"
# include no vax BL        "38 (0.8%)"

# View(data_ex2[which(data_ex2$vaccine_timing == "D123 in preg"), c("record_id", "include_BL", "all_dose1_info", "all_dose2_info", "all_dose3_info", "dose1_GA", "dose2_GA", "dose3_GA", "bl1a_delivery_date_BL", "all_baby_dob", "all_dose1_dates", "all_dose2_dates", "all_dose3_dates")])

## fix some of the timings based on calculated GA
data_ex2 <- data_ex2 %>%
  mutate(
    vaccine_timing_fix = case_when(
      vaccine_timing == "D123 in preg" & dose2_GA > GA_del & dose3_GA > GA_del ~ "D1 preg, D23 pp",
      vaccine_timing == "D123 in preg" & dose2_GA <= GA_del & dose3_GA > GA_del ~ "D12 in preg, D3 pp",
      vaccine_timing == "D123 in preg" & dose2_GA <= GA_del & dose3_GA <= GA_del ~ "D123 in preg",
      vaccine_timing == "D123 in preg" & dose2_GA > GA_del & is.na(all_dose3_dates) ~ "D1 in preg, D2 pp, no D3",
      vaccine_timing == "D123 in preg" & dose2_GA <= GA_del & is.na(all_dose3_dates) ~ "D12 in preg, no D3",
      TRUE ~ vaccine_timing
    )
  )

describeFactors(data_ex2$vaccine_timing)
describeFactors(data_ex2$vaccine_timing_fix)

## remove the ones with Baby DOB before March 2020
describeFactors(data_ex2$bl3_after_march_BL)
data_ex2 <- data_ex2[-which(data_ex2$all_baby_dob < "2020-03-01"), ] # from 4547 to 4539


# View(data_ex2[which(data_ex2$vaccine_timing_fix == "D123 in preg"), c("record_id", "include_BL", "all_dose1_info", "all_dose2_info", "all_dose3_info", "dose1_GA", "dose2_GA", "dose3_GA", "bl1a_delivery_date_BL", "all_baby_dob", "all_dose1_dates", "all_dose2_dates", "all_dose3_dates")])

#### make this the final designation
data_ex2 <- data_ex2 %>%
  mutate(
    vacc_in_preg = case_when(
      grepl("preg", vaccine_timing_fix) ~ "vaccine in pregnancy",
      grepl("pp|vax", vaccine_timing_fix) ~ "no vaccine in pregnancy"
    )
  )

describeFactors(data_ex2$vacc_in_preg)

dim(data_use)
data_use$bl1_currently_preg


############ Vaccine attitudes #################

# 
# length(which(is.na(data_use$vaccine_attitudes_survey_timestamp_BL)))
# # 2304 do not have vaccine attitudes survey
# length(which(is.na(data$v1_dose1_likely_BL)))
# 
# data <- data[-which(is.na(data$vaccine_attitudes_survey_timestamp_BL)), ]
# data <- data[-which(is.na(data$v1_dose1_likely_BL)),]
# 
# describeFactors(data$v1_dose1_likely_BL)
# 
# data <- data %>%
#   mutate(
#     likely_full = case_when(
#       v1_dose1_likely_BL == 1 ~ "Very Unlikely",
#       v1_dose1_likely_BL == 2 ~ "Unlikely",
#       v1_dose1_likely_BL == 3 ~ "Neutral",
#       v1_dose1_likely_BL == 4 ~ "Somewhat Likely",
#       v1_dose1_likely_BL == 5 ~ "Very Likely",
#       v1_dose1_likely_BL == 6 ~ "I have already received a COVID-19 Vaccine"
#     )
#   )
# describeFactors(data$likely_full)
# 
# dim(data)
# 
# data <- data %>% 
#   mutate(
#     likely = case_when(
#       v1_dose1_likely_BL %in% c(1, 2, 3) ~ "No",
#       v1_dose1_likely_BL %in% c(4, 5, 6) ~ "Yes"
#     )
#   )
# describeFactors(data$likely)
# # No      "339 (9.9%)"  
# # Yes "3,079 (90.1%)"


######### OTHER VARIABLES ############
## age
data$bl14_dob_month_BL
data$bl14_dob_year_BL

data$dob <- paste(data$bl14_dob_year_BL, data$bl14_dob_month_BL, "01", sep = "-")
data$dob <- as.Date(data$dob, format = "%Y-%m-%d")
data$bl_completed_date_BL <- as.Date(as.character(data$bl_completed_date_BL), format = "%Y-%m-%d")
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

# now to make gravidity a categorical variable and analyze it that way

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

# data <- data %>%
#   mutate(
#     eth = case_when(
#       bl19_ethnicity___997_BL == 1 ~ "Prefer not to answer",
#       bl19_ethnicity___999_BL == 1 ~ "Unknown",
#     )
#   )

describeFactors(data$bl19_ethnicity___1_BL)
describeFactors(data$bl19_ethnicity___2_BL)
describeFactors(data$bl19_ethnicity___3_BL)
describeFactors(data$bl19_ethnicity___4_BL)
describeFactors(data$bl19_ethnicity___5_BL)
describeFactors(data$bl19_ethnicity___6_BL)
describeFactors(data$bl19_ethnicity___7_BL)
describeFactors(data$bl19_ethnicity___8_BL)
describeFactors(data$bl19_ethnicity___9_BL)
describeFactors(data$bl19_ethnicity___10_BL)
describeFactors(data$bl19_ethnicity___998_BL)
describeFactors(data$bl19_ethnicity___997_BL)
describeFactors(data$bl19_ethnicity___999_BL)

describeFactors(data$eth)

data <- data %>% 
  mutate(
    eth2 = case_when(
      eth == "Prefer not to answer" ~ NA_character_,
      eth == "Unknown" ~ NA_character_,
      TRUE ~ eth
    )
  )
describeFactors(data$eth2)
data$eth2 <- factor(data$eth2, levels = c("White/Caucasian", "East Asian", "South Asian", "South East Asian", "Hispanic/Latino/Latina", "African/Caribbean/Black", "West Central Asian/Middle Eastern", "Other"))


# data$bl19_ethnicity___1_BL <- factor(data$bl19_ethnicity___1_BL, levels = c(0,1), labels = c("No", "Yes"))
# data$bl19_ethnicity___2_BL <- factor(data$bl19_ethnicity___2_BL, levels = c(0,1), labels = c("No", "Yes"))
# data$bl19_ethnicity___3_BL <- factor(data$bl19_ethnicity___3_BL, levels = c(0,1), labels = c("No", "Yes"))
# data$bl19_ethnicity___4_BL <- factor(data$bl19_ethnicity___4_BL, levels = c(0,1), labels = c("No", "Yes"))
# data$bl19_ethnicity___5_BL <- factor(data$bl19_ethnicity___5_BL, levels = c(0,1), labels = c("No", "Yes"))
# data$bl19_ethnicity___6_BL <- factor(data$bl19_ethnicity___6_BL, levels = c(0,1), labels = c("No", "Yes"))
# data$bl19_ethnicity___7_BL <- factor(data$bl19_ethnicity___7_BL, levels = c(0,1), labels = c("No", "Yes"))
# data$bl19_ethnicity___8_BL <- factor(data$bl19_ethnicity___8_BL, levels = c(0,1), labels = c("No", "Yes"))
# data$bl19_ethnicity___9_BL <- factor(data$bl19_ethnicity___9_BL, levels = c(0,1), labels = c("No", "Yes"))
# data$bl19_ethnicity___10_BL <- factor(data$bl19_ethnicity___10_BL, levels = c(0,1), labels = c("No", "Yes"))
# data$bl19_ethnicity___997_BL <- factor(data$bl19_ethnicity___997_BL, levels = c(0,1, NA), labels = c(0,1,1))
# data$bl19_ethnicity__998_BL <- factor(data$bl19_ethnicity___998_BL, levels = c(0,1), labels = c("No", "Yes"))
# data$bl19_ethnicity___999_BL <- factor(data$bl19_ethnicity___999_BL, levels = c(0,1, NA), labels = c(0,1,1))

#make new "other" variable to include indigenous and mixed
data <- data %>%
  mutate(
    other_new = case_when(
      bl19_ethnicity___8_BL == 1 ~ "Other",
      bl19_ethnicity___9_BL == 1 ~ "Other", 
      bl19_ethnicity___10_BL == 1 ~ "Other",
      bl19_ethnicity___998_BL== 1 ~ "Other",
      bl19_ethnicity___8_BL == 0 ~ "Not Other",
      bl19_ethnicity___9_BL == 0 ~ "Not Other", 
      bl19_ethnicity___10_BL == 0 ~ "Not Other",
      bl19_ethnicity___998_BL== 0 ~ "Not Other",))

describeFactors(data$other_new)


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

data <- data %>% 
  mutate(
    prov_cat = case_when(
      bl16a_province_res_BL %in% c("NB", "NL", "NS", "PE") ~ "Atlantic Provinces",
      bl16a_province_res_BL %in% c("BC", "YK") ~ "BC/YK",
      TRUE ~ as.character(bl16a_province_res_BL)
    )
  )
describeFactors(data$prov_cat)

## education
data$bl123_education_BL
describeFactors(data$bl23_education_BL)
data$bl23_education_BL <- factor(data$bl23_education_BL, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 997, 999), labels = c("Less than high school graduation", "High school graduation", "Trade certificate/Vocational", "Apprenticeship", "Non-University certificate or diploma", "Community college/CEGEP", "Bachelor's degree", "Graduate degree", NA, NA))


describeFactors()
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

###### Other descriptive analyses #####

# How likely are you to receive the vaccine? 

data <- data %>% 
  mutate(
    likely = case_when(
      v1_dose1_likely_BL %in% c(1, 2, 3) ~ "No",
      v1_dose1_likely_BL %in% c(4, 5, 6) ~ "Yes"
    )
  )
describeFactors(data$likely)

# Which vaccine did you receive? (First dose)
describeFactors(data$v1a_dose1_type_BL)
describeFactors(data$v1a_dose1_type_other_BL)

data$v1a_dose1_type_BL <- factor(data$v1a_dose1_type_BL, levels = c(1, 2, 3, 4, 5, 998, 999), labels = c("Pfizer-BioNTech", "Moderna", "AstraZeneca/COVISHIELD", "Johnson & Johnson/Janssen", "Novavax", "Other", "Unknown"))
describeFactors(data$v1a_dose1_type_BL)

# Have you received a second dose? 

describeFactors(data$v1b_dose2_BL)
data$v1b_dose2_BL <- factor(data$v1b_dose2_BL, levels = c(0, 1), labels = c("No", "Yes"))


# How likely are you to receive a second dose when offered? 

describeFactors(data$v1c_dose2_likely_BL)
data$v1c_dose2_likely_BL <- factor(data$v1c_dose2_likely_BL, levels = c(1, 2, 3, 4, 5), labels = c("Very Unlikely", "Unlikely", "Neutral", "Likely", "Very Likely"))

# When you are not preg/lactating, do you receive all recommended vaccines?

data$v2_notpreg_vaccines_BL
describeFactors(data$v2_notpreg_vaccines_BL)
data$v2_notpreg_vaccines_BL <- factor(data$v2_notpreg_vaccines_BL, levels = c(0, 1, 999), labels = c("No", "Yes", "Not Sure"))

# When you are pregnant/lactating do you receive all recommended vaccines?

describeFactors(data$v3_notpreg_vaccines_BL)
data$v3_notpreg_vaccines_BL <- factor(data$v3_notpreg_vaccines_BL, levels = c(0, 1, 999), labels = c("No", "Yes", "Not sure"))

#Thinking back to before the beginning of the COVID-19 pandemic (December 2019), how much did you value vaccines? 

describeFactors(data$v4_before_value_BL)
data$v4_before_value_BL <- as.numeric(data$v4_before_value_BL)

summary(data$v4_before_value_BL)
summary(data.vacc.value)
data.vacc.value <- data %>% 
  ungroup() %>% 
  summarise(across(v4_before_value_BL, list(mean = mean, sd = sd), na.rm = TRUE))

data.vacc.value <- t(data.vacc.value)


# 	Today, how much do you value vaccines? 

describeFactors(data$v5_today_value_BL)
data$v5_today_value_BL <- as.numeric(data$v5_today_value_BL)

data.value.now <- data %>% 
  ungroup() %>% 
  summarise(across(v5_today_value_BL, list(mean = mean, sd = sd), na.rm = TRUE))

summary(data$v5_today_value_BL)

data.value.now <- t(data.value.now)


# Have you received the influenza vaccine in the past 5 years?	 

data$v6_flu_BL <- factor(data$v6_flu_BL, levels = c(1,2,3,4,999), labels = c("Never", "1-2 times", "3-4 times", "Every year", "Don't know"))

describeFactors(data$v6_flu_BL)
# Where do you get your healthcare information? 
# (select all that apply)

describeFactors(data$v7_healthinfo___1_BL)
describeFactors(data$v7_healthinfo___2_BL)
describeFactors(data$v7_healthinfo___3_BL)
describeFactors(data$v7_healthinfo___4_BL)
describeFactors(data$v7_healthinfo___5_BL)
describeFactors(data$v7_healthinfo___6_BL)
describeFactors(data$v7_healthinfo___7_BL)
describeFactors(data$v7_healthinfo___8_BL)

data$v7_healthinfo___1_BL <- factor(data$v7_healthinfo___1_BL, levels = c(0,1), labels = c("No", "Yes"))
data$v7_healthinfo___2_BL <- factor(data$v7_healthinfo___2_BL, levels = c(0,1), labels = c("No", "Yes"))
data$v7_healthinfo___3_BL <- factor(data$v7_healthinfo___3_BL, levels = c(0,1), labels = c("No", "Yes"))
data$v7_healthinfo___4_BL <- factor(data$v7_healthinfo___4_BL, levels = c(0,1), labels = c("No", "Yes"))
data$v7_healthinfo___5_BL <- factor(data$v7_healthinfo___5_BL, levels = c(0,1), labels = c("No", "Yes"))
data$v7_healthinfo___6_BL <- factor(data$v7_healthinfo___6_BL, levels = c(0,1), labels = c("No", "Yes"))
data$v7_healthinfo___7_BL <- factor(data$v7_healthinfo___7_BL, levels = c(0,1), labels = c("No", "Yes"))
data$v7_healthinfo___8_BL <- factor(data$v7_healthinfo___8_BL, levels = c(0,1), labels = c("No", "Yes"))


data <- data %>% 
  mutate(
    health.info = case_when(
      v7_healthinfo___1_BL == 1 ~ "Family Physician",
      v7_healthinfo___2_BL == 1 ~ "Prenatal care provider",
      v7_healthinfo___3_BL == 1 ~ "Other healthcare provider",
      v7_healthinfo___4_BL == 1 ~ "Family & friends",
      v7_healthinfo___5_BL == 1 ~ "Social media",
      v7_healthinfo___6_BL == 1 ~ "Traditional media (the news)",
      v7_healthinfo___7_BL == 1 ~ "Google search",
      v7_healthinfo___8_BL == 1 ~ "Government",
    )
  )
describeFactors(data$health.info)


# l.	The health information I receive from…(very reliablevery unreliable)
# i.	my family physician is..

describeFactors(data$v8_healthinfo_famphys_BL)

as.numeric(data$v8_healthinfo_famphys_BL)

data <- data %>%
  mutate(
    famphys = case_when(
      v8_healthinfo_famphys_BL %in% 1 ~ 5,
      v8_healthinfo_famphys_BL %in% 2 ~ 4,
      v8_healthinfo_famphys_BL %in% 3 ~ 3,
      v8_healthinfo_famphys_BL %in% 4 ~ 2,
      v8_healthinfo_famphys_BL %in% 5 ~ 1,
      TRUE ~ as.numeric(v8_healthinfo_famphys_BL)
    ))

data$famphys <- replace(data$famphys, which(data$famphys == 999), NA)

data.famphys <- data %>% 
  ungroup() %>% 
  summarise(across(famphys, list(mean = mean, sd = sd), na.rm = TRUE))

data.famphys<- t(data.famphys)

summary(data.famphys)

data$v8_healthinfo_famphys_BL <- factor(data$v8_healthinfo_famphys_BL, levels = c(1,2,3,4,5,999), labels = c("Very reliable", "Somewhat reliable", "Neutral", "Somewhat unreliable", "Very unreliable", "NA"))


# ii.	my prenatal care provider is…

describeFactors(data$v9_healthinfo_prenatal_BL)
data$v9_healthinfo_prenatal_BL <- factor(data$v9_healthinfo_prenatal_BL, levels = c(1,2,3,4,5,999), labels = c("Very reliable", "Somewhat reliable", "Neutral", "Somewhat unreliable", "Very unreliable", "NA"))

as.numeric(data$v9_healthinfo_prenatal_BL)

data <- data %>%
  mutate(
    prenatal = case_when(
      v9_healthinfo_prenatal_BL %in% 1 ~ 5,
      v9_healthinfo_prenatal_BL %in% 2 ~ 4,
      v9_healthinfo_prenatal_BL %in% 3 ~ 3,
      v9_healthinfo_prenatal_BL %in% 4 ~ 2,
      v9_healthinfo_prenatal_BL %in% 5 ~ 1,
      TRUE ~ as.numeric(v9_healthinfo_prenatal_BL)
    ))

data$prenatal <- replace(data$prenatal, which(data$prenatal == 999), NA)

data.prenatal <- data %>% 
  ungroup() %>% 
  summarise(across(prenatal, list(mean = mean, sd = sd), na.rm = TRUE))

data.prenatal <- t(data.prenatal)


# iii.	my other healthcare provider is…

describeFactors(data$v10_healthinfo_otherhcp_BL)
data$v10_healthinfo_otherhcp_BL <- factor(data$v10_healthinfo_otherhcp_BL, levels = c(1,2,3,4,5,999), labels = c("Very reliable", "Somewhat reliable", "Neutral", "Somewhat unreliable", "Very unreliable", "NA"))  


as.numeric(data$v10_healthinfo_otherhcp_BL)

data <- data %>%
  mutate(
    other.hcp = case_when(
      v10_healthinfo_otherhcp_BL %in% 1 ~ 5,
      v10_healthinfo_otherhcp_BL %in% 2 ~ 4,
      v10_healthinfo_otherhcp_BL %in% 3 ~ 3,
      v10_healthinfo_otherhcp_BL %in% 4 ~ 2,
      v10_healthinfo_otherhcp_BL %in% 5 ~ 1,
      TRUE ~ as.numeric(v10_healthinfo_otherhcp_BL)
    ))

data$other.hcp <- replace(data$other.hcp, which(data$other.hcp == 999), NA)

data.other.hcp <- data %>% 
  ungroup() %>% 
  summarise(across(other.hcp, list(mean = mean, sd = sd), na.rm = TRUE))

data.other.hcp <- t(data.other.hcp)

# iv.	family & friends is…

describeFactors(data$v11_healthinfo_fam_BL)
data$v11_healthinfo_fam_BL <- factor(data$v11_healthinfo_fam_BL, levels = c(1,2,3,4,5,999), labels = c("Very reliable", "Somewhat reliable", "Neutral", "Somewhat unreliable", "Very unreliable", "NA"))


as.numeric(data$v11_healthinfo_fam_BL)

data <- data %>%
  mutate(
    famfri = case_when(
      v11_healthinfo_fam_BL %in% 1 ~ 5,
      v11_healthinfo_fam_BL %in% 2 ~ 4,
      v11_healthinfo_fam_BL %in% 3 ~ 3,
      v11_healthinfo_fam_BL %in% 4 ~ 2,
      v11_healthinfo_fam_BL %in% 5 ~ 1,
      TRUE ~ as.numeric(v11_healthinfo_fam_BL)
    ))

data$famfri <- replace(data$famfri, which(data$famfri == 999), NA)

data.famfri <- data %>% 
  ungroup() %>% 
  summarise(across(famfri, list(mean = mean, sd = sd), na.rm = TRUE))

data.famfri <- t(data.famfri)

# v.	social media is…

describeFactors(data$v12_healthinfo_social_BL)
data$v12_healthinfo_social_BL <- factor(data$v12_healthinfo_social_BL, levels = c(1,2,3,4,5,999), labels = c("Very reliable", "Somewhat reliable", "Neutral", "Somewhat unreliable", "Very unreliable", "NA"))  

as.numeric(data$v12_healthinfo_social_BL)

data <- data %>%
  mutate(
    social = case_when(
      v12_healthinfo_social_BL %in% 1 ~ 5,
      v12_healthinfo_social_BL %in% 2 ~ 4,
      v12_healthinfo_social_BL %in% 3 ~ 3,
      v12_healthinfo_social_BL %in% 4 ~ 2,
      v12_healthinfo_social_BL %in% 5 ~ 1,
      TRUE ~ as.numeric(v12_healthinfo_social_BL)
    ))

data$social <- replace(data$social, which(data$social == 999), NA)

data.social <- data %>% 
  ungroup() %>% 
  summarise(across(social, list(mean = mean, sd = sd), na.rm = TRUE))

data.social <- t(data.social)


# vi.	traditional media (the news) is…

describeFactors(data$v13_healthinfo_media_BL)
data$v13_healthinfo_media_BL <- factor(data$v13_healthinfo_media_BL, levels = c(1,2,3,4,5,999), labels = c("Very reliable", "Somewhat reliable", "Neutral", "Somewhat unreliable", "Very unreliable", "NA"))  


as.numeric(data$v13_healthinfo_media_BL)

data <- data %>%
  mutate(
    news = case_when(
      v13_healthinfo_media_BL %in% 1 ~ 5,
      v13_healthinfo_media_BL %in% 2 ~ 4,
      v13_healthinfo_media_BL %in% 3 ~ 3,
      v13_healthinfo_media_BL %in% 4 ~ 2,
      v13_healthinfo_media_BL %in% 5 ~ 1,
      TRUE ~ as.numeric(v13_healthinfo_media_BL)
    ))

data$news <- replace(data$news, which(data$news == 999), NA)

data.news <- data %>% 
  ungroup() %>% 
  summarise(across(news, list(mean = mean, sd = sd), na.rm = TRUE))

data.news <- t(data.news)


# vii.	Google search is…

describeFactors(data$v14_healthinfo_google_BL)
data$v14_healthinfo_google_BL <- factor(data$v14_healthinfo_google_BL, levels = c(1,2,3,4,5,999), labels = c("Very reliable", "Somewhat reliable", "Neutral", "Somewhat unreliable", "Very unreliable", "NA"))  

as.numeric(data$v14_healthinfo_google_BL)

data <- data %>%
  mutate(
    google = case_when(
      v14_healthinfo_google_BL %in% 1 ~ 5,
      v14_healthinfo_google_BL %in% 2 ~ 4,
      v14_healthinfo_google_BL %in% 3 ~ 3,
      v14_healthinfo_google_BL %in% 4 ~ 2,
      v14_healthinfo_google_BL %in% 5 ~ 1,
      TRUE ~ as.numeric(v14_healthinfo_google_BL)
    ))

data$google <- replace(data$google, which(data$google == 999), NA)

data.google <- data %>% 
  ungroup() %>% 
  summarise(across(google, list(mean = mean, sd = sd), na.rm = TRUE))

data.google <- t(data.google)


# viii.	the government is…

describeFactors(data$v15_healthinfo_gov_BL)
data$v15_healthinfo_gov_BL <- factor(data$v15_healthinfo_gov_BL, levels = c(1,2,3,4,5,999), labels = c("Very reliable", "Somewhat reliable", "Neutral", "Somewhat unreliable", "Very unreliable", "NA")) 


as.numeric(data$v15_healthinfo_gov_BL)

data <- data %>%
  mutate(
    gov = case_when(
      v15_healthinfo_gov_BL %in% 1 ~ 5,
      v15_healthinfo_gov_BL %in% 2 ~ 4,
      v15_healthinfo_gov_BL %in% 3 ~ 3,
      v15_healthinfo_gov_BL %in% 4 ~ 2,
      v15_healthinfo_gov_BL %in% 5 ~ 1,
      TRUE ~ as.numeric(v15_healthinfo_gov_BL)
    ))

data$gov <- replace(data$gov, which(data$gov == 999), NA)

data.gov <- data %>% 
  ungroup() %>% 
  summarise(across(gov, list(mean = mean, sd = sd), na.rm = TRUE))

data.gov<- t(data.gov)





# sum to form attitude scale
# all attitudes anchored in same direction
# v18 to v28

a.ca <- alpha(data[, c("v18_serious_BL", "v19_serious_preg_BL", "v20_beneficial_BL", "v21_beneficial_preg_BL", "v22_tested_BL", "v23_tested_preg_BL", "v24_safe_BL", "v25_safe_preg_BL", "v26_safe_fetus_BL", "v27_effective_BL", "v28_effective_preg_BL")])
a.ca


#          lower alpha upper
# Feldt     0.88  0.89  0.89 


data <- data %>% 
  rowwise() %>% 
  mutate(ATT = sum(c(v18_serious_BL, v19_serious_preg_BL, v20_beneficial_BL, v21_beneficial_preg_BL, v22_tested_BL, v23_tested_preg_BL, v24_safe_BL, v25_safe_preg_BL, v26_safe_fetus_BL, v27_effective_BL, v28_effective_preg_BL)))
summary(data$ATT)

data.att <- data %>% 
  ungroup() %>% 
  summarise(across(c(v18_serious_BL, v19_serious_preg_BL, v20_beneficial_BL, v21_beneficial_preg_BL, v22_tested_BL, v23_tested_preg_BL, v24_safe_BL, v25_safe_preg_BL, v26_safe_fetus_BL, v27_effective_BL, v28_effective_preg_BL, ATT), list(mean = mean, sd = sd), na.rm = TRUE))
(data.att2 <- t(data.att))



describeFactors(data$v18_serious_BL)
describeFactors(data$v19_serious_preg_BL)
describeFactors(data$v20_beneficial_BL)
describeFactors(data$v21_beneficial_preg_BL)
describeFactors(data$v22_tested_BL)
describeFactors(data$v23_tested_preg_BL)
describeFactors(data$v24_safe_BL)
describeFactors(data$v25_safe_preg_BL)
describeFactors(data$v26_safe_fetus_BL)
describeFactors(data$v27_effective_BL)
describeFactors(data$v28_effective_preg_BL)


# direct subjective norms
# v29 to v30
describeFactors(data$v29_important_BL)
describeFactors(data$v30_socialpressure_BL)

snd.ca <- alpha(data[, c("v29_important_BL", "v30_socialpressure_BL")])
snd.ca
#          lower alpha upper
# Feldt    -0.05  0.02  0.08
# these 2 are not agreeing and should be reported separately

data.dsn <- data %>% 
  ungroup() %>% 
  summarise(across(c(v29_important_BL:v30_socialpressure_BL), list(mean = mean, sd = sd), na.rm = TRUE))

(data.dsn2 <- t(data.dsn))


# perceived behavioural control
# v39 to v41
data$v39_easilyreceive_BL[which(data$v1_dose1_likely_BL != 6)]
describeFactors(data$v39_easilyreceive_BL[which(data$v1_dose1_likely_BL != 6)])
data$v40_choice_BL[which(data$v1_dose1_likely_BL != 6)]
data$v41_control_BL[which(data$v1_dose1_likely_BL != 6)]

pbc.ca <- alpha(data[which(data$v1_dose1_likely_BL != 6), c("v39_easilyreceive_BL", "v40_choice_BL", "v41_control_BL")])
pbc.ca
#          lower alpha upper
# Feldt     0.53  0.59  0.65

data <- data %>% 
  rowwise() %>% 
  mutate(PBC = sum(c(v39_easilyreceive_BL, v40_choice_BL, v41_control_BL)))

summary(data$PBC)

data.pbc <- data %>% 
  ungroup() %>% 
  summarise(across(c(v39_easilyreceive_BL, v40_choice_BL, v41_control_BL, PBC), list(mean = mean, sd = sd), na.rm = TRUE))

(data.pbc2 <- t(data.pbc))

describeFactors(data$v39_easilyreceive_BL)
describeFactors(data$v40_choice_BL)
describeFactors(data$v41_control_BL)


# Indirect social norms (Ianna's version, no re-coding)

data <- data %>% 
  rowwise() %>% 
  mutate(isn.aug = sum(c(v31_pho_approve_BL, v32_pho_important_BL, v33_partner_approve_BL, v34_partner_important_BL, v35_fam_approve_BL, v36_fam_important_BL, v37_prenatal_approve_BL, v38_prenatal_important_BL)))

summary(data$isn.aug)

data.isn.aug <- data %>% 
  ungroup() %>% 
  summarise(across(c(v31_pho_approve_BL, v32_pho_important_BL, v33_partner_approve_BL, v34_partner_important_BL, v35_fam_approve_BL, v36_fam_important_BL, v37_prenatal_approve_BL, v38_prenatal_important_BL, isn.aug), list(mean = mean, sd = sd), na.rm = TRUE))

(data.isn.aug <- t(data.isn.aug))

isn.ca <- alpha(data[, c("v31_pho_approve_BL", "v32_pho_important_BL", "v33_partner_approve_BL", "v34_partner_important_BL", "v35_fam_approve_BL", "v36_fam_important_BL", "v37_prenatal_approve_BL", "v38_prenatal_important_BL")])
isn.ca

#       lower alpha upper
#Feldt  0.627 0.645 0.663

describeFactors(data$v31_pho_approve_BL)
describeFactors(data$v32_pho_important_BL)
describeFactors(data$v33_partner_approve_BL)
describeFactors(data$v34_partner_important_BL)
describeFactors(data$v35_fam_approve_BL)
describeFactors(data$v36_fam_important_BL)
describeFactors(data$v37_prenatal_approve_BL)
describeFactors(data$v38_prenatal_important_BL)




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

