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
    e                                       data$bl1a_delivery_date != "" &
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

#VACCINATED IN PREG
vacc_in_preg <- data[which(data$record_id %in% ids.can.EDD.vacc),]

#UNVACCINATED IN PREG 
unvacc_in_preg <- data[-which(data$record_id %in% ids.can.EDD.vacc),]

#preg_outcomes <- rowSums(!is.na(data[, c("pregnancy_outcomes_timestamp")]))
preg_outcomes_table <- data %>% group_by(record_id) %>% 
  summarise(preg_outcomes = sum(!is.na(pregnancy_outcomes_timestamp) & pregnancy_outcomes_timestamp != ""))


###### exclusions
# 1. exclude everyone who does not have a pregnancy outcomes survey filled in
data_ex1 <- merge(data, preg_outcomes_table, by = "record_id") %>% filter(preg_outcomes == 1) %>% select(-preg_outcomes)

# 2. exclude the ones that should be excluded from using BL information (eg vaxx before preg or not pregnant) - COMPLETE

 <- data_ex2[-which(data_ex2$bl1a_delivery_date < "2020-03-01"),]
data_ex2 <- data[which(data$bl6_dose1_date < data$bl1a_delivery_date),]


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






