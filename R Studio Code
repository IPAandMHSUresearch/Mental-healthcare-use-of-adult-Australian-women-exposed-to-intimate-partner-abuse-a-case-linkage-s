#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Mental Healthcare use of adult Australian women exposed to intimate partner abuse: a case-linkage study  

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### I) Table of Contents 

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# I: Table of Contents 
#
# 1: Packages
#
# 2: Data cleaning and wrangling 
#     2.1: Data
#     2.2: Set exclusion criteria and separate missing cases 
#     2.3: Recode relevant cases 
#     2.4: Sample characteristics/descriptives
#
#     MENTAL HEALTH SERVICE USE (MHSU) OF WOMEN EXPOSED TO IPA COMPARED TO WOMEN IN THE GENERAL POPULATION 
#
# 3:  Acute Public Mental Health Service Use Comparisons 
#     3.1: Clean VAHI data (general population dataset)  
#     3.2: Covert count variables to numeric 
#     3.3: Summarise dataset to aggregate number from all programs for each demographic
#     3.4: Understand data distribution, identify & remove outliers 
#     3.5: Create equivalent IPA victims dataframe (i.e. dataset containing same variables as VAHI data)
#     3.5: Insert population numbers into VAHI dataset 
#     3.7: Bind IPA victims dataset with VAHI dataset & ensure variable names are identical between both datasets)
#     3.8: Calculate rates of acute MHSU for both VAHI and IPA victims datasets
#
# 4:  Outpatient Public Mental Health Service Use Comparisons
#     4.1: Clean VAHI data   
#     4.2: Covert count variables to numeric 
#     4.3: Summarise dataset to aggregate number from all programs for each demographic
#     4.4: Understand data distribution, identify & remove outliers 
#     4.5: Create equivalent IPA victims dataframe (i.e. dataset containing same variables as VAHI data)
#     4.5: Insert population numbers into VAHI dataset 
#     4.7: Bind IPA victims dataset with VAHI dataset & ensure variable names are identical between both datasets)
#     4.8: Calculate rates of acute MHSU for both VAHI and IPA victims datasets
#
#     REGRESSION ANALYSES (LOGISTIC AND NEGATIVE BINOMIAL) 
#
# 5:  Within Group Analysis - Acute Mental Health Service Use 
#     5.1: Set data distribution ready for regression
#     5.2: Q1: Controlling for index age, how does general victimisation (count) predict acute MHSU?
#     5.3: Q2: Controlling for index age & general victimisation, how does historical FV victimisation predict acute MHSU?
#     5.4: Q3: Controlling for index age & general victimisation, how does historical FV and estimated age at first FV victimisation predict acute MHSU?
#     5.5: Q4: Is there an interaction effect between historical general victimisation and historical FV victimisation in predicting acute MHSU?
#
# 6:  Within Group Analysis - Outpatient Mental Health Service Use
#     6.1: Q5: Controlling for index age, how does general victimisation (count) predict outpatient MHSU? 
#     6.2: Q6: Controlling for index age & general victimisation, how does historical FV victimisation predict outpatient MHSU?
#     6.3: Q7: Controlling for index age & general victimisation, how does historical FV and estimated age at first FV victimisation predict outpatient MHSU?
#     6.4: Q8: Is there an interaction effect between historical general victimisation and historical FV victimisation in predicting outpatient MHSU?
#
# 7:  Within Group Analysis - Combined Mental Health Service Use 
#     7.1: Q9: Controlling for index age, how does general victimisation (count) predict combined MHSU?
#     7.2: Q10: Controlling for index age & general victimisation, how does historical FV victimisation predict combined MHSU
#     7.3: Q11: Controlling for index age & general victimisation, how does historical FV and estimated age at first FV victimisation predict combined MHSU?
#     7.4: Q12: Is there an interaction effect between historical general victimisation and historical FV victimisation in predicting combined MHSU?

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 1) Packages ----------------------------------------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

library(tidyverse)
library(Hmisc)
library(psych)
library(readxl)
library(MASS)
library(AER)
library(rms)
library(tidyselect)
library(dplyr)
library(sjPlot)
library(sjstats)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 2) Data cleaning and wrangling ---------------------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 2.1) Data ------------------------------------------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Import data 

setwd("G:/Projects/Maddie B analysis")
sp<-read.csv("SUPPRESSED.csv")
rels<-read.csv("G:/Projects/Maddie B analysis/SUPPRESSED.csv")
str(rels)

rels<-rels%>%dplyr::select(RQ494_CLUSTER_ID1,index_relation_recode)

sp<-left_join(sp,rels)

# Read csv file 
analysis_data<-read.csv("G:/Projects/Maddie B analysis/SUPPRESSED.csv")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 2.2) Set exclusion criteria and separate missing cases ---------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

sp_fems<-sp%>%filter(index_sex=="F")
sp_fems_IPA<-sp_fems%>%filter(index_relation_recode=="IPA")
sp_fems_IPAafms<-sp_fems_IPA%>%filter(Cluster_id_type=="AFM")

# Clean data to remove constant & perpetrator only variables
sp_fems_IPAafms<-sp_fems_IPAafms%>%dplyr::select(-ODS_D_eating_disors, 
                                                 -ODS_D_non_organic_sleep_disors, -ODS_D_Paranoid_person_disor, -ODS_D_Schizoid_person_disor, -ODS_D_Anankastic_person_disor, -ODS_D_Axious_avoid_person_disor, -ODS_D_Mixed_person_disor, -ODS_D_Enduring_person_changes, 
                                                 -ODS_D_Habit_impulse_disors, -ODS_D_Mental_retardation, -ODS_D_Disordered_psych_develop, -ODS_D_Pervasive_develop_disors,
                                                 -ODS_D_Hyperkinetic_disors, -ODS_D_Opposit_defian_disor, -ODS_D_Childhood_anxiet_disors, -hist_general_perp_count_iresp, -hist_general_perp_iresp, -hist_any_perp_iresp, -hist_general_perp_count_iafm, -hist_general_perp_iafm,
                                                 -hist_any_perp_iafm, -hist_any_perp_iresp, -hist_general_vic_count_iresp, -hist_general_vic_iresp, -hist_any_vic_iresp, -unique_vics, -multiple_victims)
sp_fems_IPAafms<-sp_fems_IPAafms%>%dplyr::select(-contains("resp"), -contains("perp"), -contains("ODS"), -contains("ODSA"))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 2.3) Recode relevant variables ---------------------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Recode 'acute MHSU' to binary 
sp_fems_IPAafms$bin_acute_MHSU<-ifelse(sp_fems_IPAafms$MHSU_Acute_intensive_cat_total_lifetime>0,"Used MHS","Did not use MHS")
sp_fems_IPAafms$bin_acute_MHSU<-ifelse(sp_fems_IPAafms$bin_acute_MHSU=="Used MHS",1,0)

# Recode 'outpatient/community MHSU' to binary 
sp_fems_IPAafms$bin_outpatient_MHSU<-ifelse(sp_fems_IPAafms$MHSU_outpatient_public_community_cat_total_lifetime>0,"Used MHS","Did not use MHS")
sp_fems_IPAafms$bin_outpatient_MHSU<-ifelse(sp_fems_IPAafms$bin_outpatient_MHSU=="Used MHS",1,0)

# Create variable for combined MHSU 
sp_fems_IPAafms<-sp_fems_IPAafms %>%
  mutate(combined_MHSU_lifetime = MHSU_Acute_intensive_cat_total_lifetime + MHSU_outpatient_public_community_cat_total_lifetime)

# Confirm this is the sum of acute and outpatient 
sum(sp_fems_IPAafms$MHSU_Acute_intensive_cat_total_lifetime) # total number of contacts - acute
sum(sp_fems_IPAafms$MHSU_outpatient_public_community_cat_total_lifetime) # total number of contacts - community
sum(sp_fems_IPAafms$MHSU_Acute_intensive_cat_total_lifetime, sp_fems_IPAafms$MHSU_outpatient_public_community_cat_total_lifetime) # total number of contacts - community and acute
sum(sp_fems_IPAafms$combined_MHSU_lifetime) # total number of contacts- combined variable

# Recode 'combined MHSU' binary 
sp_fems_IPAafms$bin_combined_MHSU<-ifelse(sp_fems_IPAafms$combined_MHSU_lifetime>0,"Used MHS","Did not use MHS")
sp_fems_IPAafms$bin_combined_MHSU<-ifelse(sp_fems_IPAafms$bin_combined_MHSU=="Used MHS",1,0)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 2.4) Sample Characteristics/descriptives -----------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Provide the mean, maximum, minimum, SD of age at time of index for female IPA
table(sp_fems_IPAafms$index_age, useNA = "always")
describe(sp_fems_IPAafms$index_age)
ggplot(sp_fems_IPAafms, aes(x=index_age)) + geom_histogram(bins = 100)

# Provide the prevalence of lifetime acute, outpatient & combined lifetime MHSU in female IPA victims
describe(sp_fems_IPAafms$MHSU_Acute_intensive_cat_total_lifetime)
table(sp_fems_IPAafms$MHSU_Acute_intensive_cat_total_lifetime>0, useNA = "always")

describe(sp_fems_IPAafms$MHSU_outpatient_public_community_cat_total_lifetime)
table(sp_fems_IPAafms$MHSU_outpatient_public_community_cat_total_lifetime>0, useNA = "always")
describe(sp_fems_IPAafms$combined_MHSU_lifetime)
table(sp_fems_IPAafms$combined_MHSU_lifetime>0, useNA = "always")

# Provide the prevalence of post-index acute & outpatient MHSU IN female IPA victims 
describe(sp_fems_IPAafms$MHSU_Acute_intensive_cat_total_post_index)
table(sp_fems_IPAafms$MHSU_Acute_intensive_cat_total_post_index>0, useNA = "always")
describe(sp_fems_IPAafms$MHSU_outpatient_public_community_cat_total_post_index)
table(sp_fems_IPAafms$MHSU_outpatient_public_community_cat_total_post_index>0, useNA = "always")

# Provide the number of women with their first FV incident before/after 15 and under 
describe(sp_fems_IPAafms$est_age_first_afm)
table(sp_fems_IPAafms$est_age_first_afm<=15, useNA = "always")

# Provide the mean, maximum, minimum, SD of total historical FV victimisations (as victim) 
# note; 0 means first time victimisation at index incident leading to sample inclusion 
describe(sp_fems_IPAafms$num_hist_vic)
table(sp_fems_IPAafms$num_hist_vic>0)
table(sp_fems_IPAafms$num_hist_vic==1)
prop.table(table(sp_fems_IPAafms$num_hist_vic))*100

# Provide the mean, maximum, minimum, SD of total general victimisations (as victim) 
# note; 0 means first time victimisation at index incident leading to sample inclusion 
describe(sp_fems_IPAafms$hist_general_vic_count_iafm)
table(sp_fems_IPAafms$hist_general_vic_count_iafm>0)
table(sp_fems_IPAafms$num_hist_vic==1)
prop.table(table(sp_fems_IPAafms$num_hist_vic))*100

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 3) General Population Comparisons - Acute Public Mental Health Services ----

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-####################
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-####################

VAHI_adm<-read_excel("G:/raw_data/SUPPRESSED.xlsx", sheet = x, skip = x)

### 3.1) Clean General Population (VAHI) Data  ---------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

glimpse(VAHI_adm)
str(VAHI_adm)
table(VAHI_adm$Program)
table(VAHI_adm$Sex)
table(VAHI_adm$Admissions)

# Check missing data 
sum(is.na(VAHI_adm))
table(VAHI_adm$Program, useNA = 'always')
table(VAHI_adm$Sex, useNA = 'always')
table(VAHI_adm$`Age Group`, useNA = 'always')
table(VAHI_adm$Admissions, useNA = 'always')

# Remove irrelevant values (remove males)
VAHI_adm<-VAHI_adm%>%filter(Sex=="Female")

# Remove 'other' programs
VAHI_adm<-VAHI_adm%>%filter(Program!="Other")

### 3.2) Convert count variables to numeric  ----------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Recode <5 values to either 0 or 4
VAHI_adm$Admissions_0<-as.numeric(ifelse(VAHI_adm$Admissions=="<5",0,VAHI_adm$Admissions))
VAHI_adm$Admissions_4<-as.numeric(ifelse(VAHI_adm$Admissions=="<5",4,VAHI_adm$Admissions))

### 3.3) Summarise dataset to aggregate number from all programs for each demographic ---------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

VAHI_collapsed_0<-VAHI_adm%>%group_by(Sex, Program)%>% summarise(Admissions = sum(Admissions_0))
VAHI_collapsed_4<-VAHI_adm%>%group_by(Sex, Program)%>% summarise(Admissions = sum(Admissions_4))

### 3.4) Understand data distribution, identify & remove outliers --------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

sp_fems_IPAafms$MHSU_Acute_intensive_cat_total_post_index
ggplot(sp_fems_IPAafms, aes(x=MHSU_Acute_intensive_cat_total_post_index)) + geom_histogram(bins = 8)
prop.table(table(sp_fems_IPAafms$MHSU_Acute_intensive_cat_total_post_index))
psych::describe(sp_fems_IPAafms$MHSU_Acute_intensive_cat_total_post_index, quant = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99))

# Exclude 99th percentile 
quantile_0.99<-quantile(sp_fems_IPAafms$MHSU_Acute_intensive_cat_total_post_index, probs = 0.99)
sp_fems_IPAafms_agg<-sp_fems_IPAafms%>%dplyr::select(index_age,index_sex,MHSU_Acute_intensive_cat_total_post_index)%>%
  filter(sp_fems_IPAafms$MHSU_Acute_intensive_cat_total_post_index < quantile_0.99)%>%
  pivot_longer(cols = c(MHSU_Acute_intensive_cat_total_post_index), values_to = "Admissions_0", names_to = "Program")

### 3.5) Create equivalent IPA victims dataframe (i.e. dataset containing same variables as VAHI data) --------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Remove all rows where index age is missing 
sp_fems_IPAafms_agg<-sp_fems_IPAafms_agg[!is.na(sp_fems_IPAafms_agg$index_age),]

# Recode to equivalent ages
sp_fems_IPAafms_agg$`Age Group`<-ifelse(sp_fems_IPAafms_agg$index_age<18, "0-17",
                                        ifelse(sp_fems_IPAafms_agg$index_age>17 & sp_fems_IPAafms_agg$index_age<25, "18-24",
                                               ifelse(sp_fems_IPAafms_agg$index_age>24 & sp_fems_IPAafms_agg$index_age<35, "25-34",
                                                      ifelse(sp_fems_IPAafms_agg$index_age>34 & sp_fems_IPAafms_agg$index_age<45, "35-44",
                                                             ifelse(sp_fems_IPAafms_agg$index_age>44, "45+", NA_character_)))))
# Check for missing data 
table(sp_fems_IPAafms_agg$`Age Group`, useNA = "always")
table(sp_fems_IPAafms_agg$`Age Group`, sp_fems_IPAafms_agg$index_age)

# Assign sp_fems_IPAafms_agg to spfemsafms_ppl
# Group spfemsafms_ppl by age & program and then count observations in each group, stored in column 'n' 
spfemsafms_ppl<-sp_fems_IPAafms_agg%>%group_by(Program)%>%summarise(n=n())

# Group spfemsafms_ppl by age & program and then calculate sum of 0 admissions in each group, stored in Admissions_0 column 
sp_fems_IPAafms_agg<-sp_fems_IPAafms_agg%>%group_by(Program)%>%summarise(Admissions_0=sum(Admissions_0))%>%ungroup()

# Make new column Admissions_4 with same values as Admissions_0 
sp_fems_IPAafms_agg$Admissions_4<-sp_fems_IPAafms_agg$Admissions_0

# Merge sp_fems_IPAafms_agg and spfemsafms_ppl 
sp_fems_IPAafms_agg<-left_join(sp_fems_IPAafms_agg, spfemsafms_ppl)

# Assign female to sex, specify by AFMS
sp_fems_IPAafms_agg$Sex<-"F"
sp_fems_IPAafms_agg$Group<-"Female Afms"

### 3.6) Insert population numbers into VAHI dataset ---------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

vicpop<-read_excel("G:/raw_data/Victoria population counts.xlsx")
vicpop_f<-vicpop%>%filter(Sex=="Female")
names(vicpop_f)<-c("Sex","Age Group", "n")

### 3.7) Bind victims dataset with VAHI dataset & ensure variable names are identical between both datasets -----------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

VAHI_adm<-left_join(VAHI_adm, vicpop_f)
VAHI_adm$Group<-"Gen pop"
VAHI_adm
sp_fems_IPAafms_agg

# Exclude non-acute data; 'Other Program' and 'Adult Rehabilitation, Extended Care & Residential' 
adm_compare_afms<-bind_rows(VAHI_adm, sp_fems_IPAafms_agg)%>%filter(Program!="Other Program", Program!="Adult Rehabilitation, Extended Care & Residential")
adm_compare_afms_overall<-adm_compare_afms%>%group_by(Group, Program)%>%summarise(Admissions_0=sum(Admissions_0), Admissions_4=sum(Admissions_4), `n`=sum(`n`))%>%ungroup()

### 3.8) Calculate rates of acute MHSU for both VAHI and IPA victim datasets -----------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Calculate rate of adm per 1000 by dividing by number of admissions divided by n(pop size) and multiplied by 1000 
adm_compare_afms$ratepp_0<-(adm_compare_afms$Admissions_0/adm_compare_afms$n)*1000
adm_compare_afms$ratepp_4<-(adm_compare_afms$Admissions_4/adm_compare_afms$n)*1000

# Store new column 'ratepp_0' in adm_compare_afms_overall dataframe
adm_compare_afms_overall$ratepp_0<-(adm_compare_afms_overall$Admissions_0/adm_compare_afms_overall$n)*1000

# Store new column 'ratepp_4' in adm_compare_afms_overall dataframe
adm_compare_afms_overall$ratepp_4<-(adm_compare_afms_overall$Admissions_4/adm_compare_afms_overall$n)*1000

adm_compare_afms_overall
View(adm_compare_afms_overall)

VAHI_outpatient<-read_excel("G:/raw_data/Data Request 5066 - Michael Trood.xlsx", sheet = 8, skip = 14)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 4) General Population Comparisons - Outpatient Public Mental Health Services ----

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

VAHI_outpatient<-read_excel("G:/raw_data/Data Request 5066 - Michael Trood.xlsx", sheet = 8, skip = 14)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 4.1) Clean General VAHI data -------------------------------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

glimpse(VAHI_outpatient)
str(VAHI_outpatient)
table(VAHI_outpatient$Program)
table(VAHI_outpatient$Sex)
table(VAHI_outpatient$Contacts)

# Check missing data 
sum(is.na(VAHI_outpatient))
table(VAHI_outpatient$Program, useNA = 'always')
table(VAHI_outpatient$Sex, useNA = 'always')
table(VAHI_outpatient$`Age Group`, useNA = 'always')
table(VAHI_outpatient$Contacts, useNA = 'always')

# Remove irrelevant values (remove males)
VAHI_outpatient<-VAHI_outpatient%>%filter(Sex=="Female")

# Remove 'other' programs
VAHI_outpatient<-VAHI_outpatient%>%filter(Program!="Other")

### 4.2) Convert count variables to numeric ------------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Recode <5 values to either 0 or 4
VAHI_outpatient$Contacts_0<-as.numeric(ifelse(VAHI_outpatient$Contacts=="<5",0,VAHI_outpatient$Contacts))
VAHI_outpatient$Contacts_4<-as.numeric(ifelse(VAHI_outpatient$Contacts=="<5",4,VAHI_outpatient$Contacts))

### 4.3) Summarise dataset to aggregate number from all programs, for each demographic ---------- 

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

VAHI_collapsed_0<-VAHI_outpatient%>%group_by(Sex, Program)%>% summarise(Contacts = sum(Contacts_0))
VAHI_collapsed_4<-VAHI_outpatient%>%group_by(Sex, Program)%>% summarise(Contacts = sum(Contacts_4))

### 4.4) Understand data distribution, identify & remove outliers 

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

sp_fems_IPAafms$MHSU_outpatient_public_community_cat_total_post_index
ggplot(sp_fems_IPAafms, aes(x=MHSU_outpatient_public_community_cat_total_post_index)) + geom_histogram(bins = 8)
prop.table(table(sp_fems_IPAafms$MHSU_outpatient_public_community_cat_total_post_index))
describe(sp_fems_IPAafms$MHSU_outpatient_public_community_cat_total_post_index)
table(sp_fems_IPAafms$MHSU_outpatient_public_community_cat_total_post_index>0)
prop.table(table(sp_fems_IPAafms$MHSU_outpatient_public_community_cat_total_post_index))
table(sp_fems_IPAafms$MHSU_outpatient_public_community_cat_total_post_index>260)

# Exclude 99th percentile
psych::describe(sp_fems_IPAafms$MHSU_outpatient_public_community_cat_total_post_index, quant = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99))
quantile_0.99<-quantile(sp_fems_IPAafms$MHSU_outpatient_public_community_cat_total_post_index, probs = 0.99)
sp_fems_IPAafms_agg<-sp_fems_IPAafms%>%dplyr::select(index_age,index_sex,MHSU_outpatient_public_community_cat_total_post_index)%>%
  filter(sp_fems_IPAafms$MHSU_outpatient_public_community_cat_total_post_index < quantile_0.99)%>%
  pivot_longer(cols = c(MHSU_outpatient_public_community_cat_total_post_index), values_to = "Contacts_0", names_to = "Program")

### 4.5) Create equivalent IPA victims dataframe (i.e. dataset containing same variables as VAHI data) --------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Remove all rows where index age is missing 
sp_fems_IPAafms_agg<-sp_fems_IPAafms_agg[!is.na(sp_fems_IPAafms_agg$index_age),]

# Recode to equivalent ages
sp_fems_IPAafms_agg$`Age Group`<-ifelse(sp_fems_IPAafms_agg$index_age<18, "0-17",
                                        ifelse(sp_fems_IPAafms_agg$index_age>17 & sp_fems_IPAafms_agg$index_age<25, "18-24",
                                               ifelse(sp_fems_IPAafms_agg$index_age>24 & sp_fems_IPAafms_agg$index_age<35, "25-34",
                                                      ifelse(sp_fems_IPAafms_agg$index_age>34 & sp_fems_IPAafms_agg$index_age<45, "35-44",
                                                             ifelse(sp_fems_IPAafms_agg$index_age>44, "45+", NA_character_)))))
# Check for missing data 
table(sp_fems_IPAafms_agg$`Age Group`, useNA = "always")
table(sp_fems_IPAafms_agg$`Age Group`, sp_fems_IPAafms_agg$index_age)

# Assign sp_fems_IPAafms_agg to spfemsafms_ppl
# Group spfemsafms_ppl by age & program and then count observations in each group, stored in column 'n' 
spfemsafms_ppl<-sp_fems_IPAafms_agg%>%group_by(Program)%>%summarise(n=n())

# Group spfemsafms_ppl by age & program and then calculate sum of 0 Contacts in each group, stored in Contacts_0 column 
sp_fems_IPAafms_agg<-sp_fems_IPAafms_agg%>%group_by(Program)%>%summarise(Contacts_0=sum(Contacts_0))%>%ungroup()

# Make new column Contacts_4 with same values as Contacts_0 
sp_fems_IPAafms_agg$Contacts_4<-sp_fems_IPAafms_agg$Contacts_0

# Merge sp_fems_IPAafms_agg and spfemsafms_ppl 
sp_fems_IPAafms_agg<-left_join(sp_fems_IPAafms_agg, spfemsafms_ppl)

# Assign female to sex, specify by AFMS
sp_fems_IPAafms_agg$Sex<-"F"
sp_fems_IPAafms_agg$Group<-"Female Afms"

### 4.6) Insert population numbers into VAHI dataset ---------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

vicpop<-read_excel("G:/raw_data/Victoria population counts.xlsx")
vicpop_f<-vicpop%>%filter(Sex=="Female")
names(vicpop_f)<-c("Sex","Age Group", "n")

### 4.7) Bind victims dataset with VAHI dataset & ensure variable names are identical between both datasets -----------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

VAHI_outpatient<-left_join(VAHI_outpatient, vicpop_f)
VAHI_outpatient$Group<-"Gen pop"
#VAHI_outpatient<-VAHI_outpatient%>%select(-Contacts)
VAHI_outpatient
sp_fems_IPAafms_agg

# Exclude non-outpatient data; 'Other Program' and 'Adult Rehabilitation, Extended Care & Residential' 
contacts_compare_afms<-bind_rows(VAHI_outpatient, sp_fems_IPAafms_agg)%>%filter(Program!="Other Program", Program!="Adult Rehabilitation, Extended Care & Residential")
contacts_compare_afms_overall<-contacts_compare_afms%>%group_by(Group, Program)%>%summarise(Contacts_0=sum(Contacts_0), Contacts_4=sum(Contacts_4), `n`=sum(`n`))%>%ungroup()

### 4.8) Calculate rates of outpatient MHSU for both VAHI and IPA victim datasets -----------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Calculate rate of contacts per 1000 by dividing by number of Contacts divided by n(pop size) and multiplied by 1000 
contacts_compare_afms$ratepp_0<-(contacts_compare_afms$Contacts_0/contacts_compare_afms$n)*1000
contacts_compare_afms$ratepp_4<-(contacts_compare_afms$Contacts_4/contacts_compare_afms$n)*1000

# Store new column 'ratepp_0' in contacts_compare_afms_overall dataframe
contacts_compare_afms_overall$ratepp_0<-(contacts_compare_afms_overall$Contacts_0/contacts_compare_afms_overall$n)*1000

# Store new column 'ratepp_4' in contacts_compare_afms_overall dataframe
contacts_compare_afms_overall$ratepp_4<-(contacts_compare_afms_overall$Contacts_4/contacts_compare_afms_overall$n)*1000

contacts_compare_afms_overall
View(contacts_compare_afms_overall)

# Ensure all population sizes equivalent 
contacts_compare_afms_overall[7,5]<-3035206
contacts_compare_afms_overall[7,5]<-(2421/3035206)*1000
contacts_compare_afms_overall[7,7]<-(2421/3035206)*1000
options(scipen = 999)

contacts_compare_afms_overall
View(contacts_compare_afms_overall)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 5) Regression Analyses - Acute mental health service use ---------------------------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 5.1) Set data distribution ready for regression  ---------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

dd<-datadist(sp_fems_IPAafms)
options(datadist = "dd")

### 5.2) Q1: Controlling for index age, how does general victimisation predict acute MHSU? ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 1a

victim_logreg_model_1a<-lrm(bin_acute_MHSU ~ index_age + hist_general_vic_count_iafm, data = sp_fems_IPAafms)

# Diagnostics

vif_values<-vif(victim_logreg_model_1a)
print(vif_values)
library(lmtest)
dw_test<-dwtest(victim_logreg_model_1a)
print(dw_test)

# Model 1a Summary & Visualisation 

summary(victim_logreg_model_1a, index_age = c(21, 47), hist_general_vic_count_iafm=c(1,2))
ggplot(Predict(victim_logreg_model_1a, fun = plogis))
anova(victim_logreg_model_1a)

# Negative Binomial Model 1b 

victim_1b_nb<-glm.nb(MHSU_Acute_intensive_cat_total_lifetime ~ index_age + hist_general_vic_count_iafm, data = sp_fems_IPAafms)

# Diagnostics

library(MASS)
phi<-sum(residuals(victim_1b_nb, type = "pearson")^2 / df.residual(victim_1b_nb))
print(paste("Overdisperso paramter (phi):", phi))
vif_values<-car::vif(victim_1b_nb)
print(vif_values)
dw_test<-dwtest(victim_1b_nb)
print(dw_test)

# Model 1b Summary 

summary(victim_1b_nb)
exp(coef(victim_1b_nb))
exp(confint(victim_1b_nb))

### 5.3) Q2: Controlling for index age & general victimisation, how does historical FV victimisation predict acute MHSU? ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 2a

victim_logreg_model_2a<-lrm(bin_acute_MHSU ~ index_age + hist_general_vic_count_iafm + num_hist_vic, data = sp_fems_IPAafms) 

# Diagnostics

vif_values<-vif(victim_logreg_model_2a)
print(vif_values)
dw_test<-dwtest(victim_logreg_model_2a)
print(dw_test)

# Model 2a Summary & ANOVA

victim_logreg_model_2a
summary(victim_logreg_model_2a)
summary(victim_logreg_model_2a, index_age = c(21, 47), hist_general_vic_count_iafm = c(1, 2), num_hist_vic = c(1, 2))
anova(victim_logreg_model_2a)

# Negative Binomial Model 2b

victim_2b_nb<-glm.nb(MHSU_Acute_intensive_cat_total_lifetime ~ index_age + hist_general_vic_count_iafm + num_hist_vic, data = sp_fems_IPAafms) 
summary(victim_2b_nb, index_age = c(21, 47), hist_general_vic_count_iafm = c(1, 2), num_hist_vic = c(1, 2))

# Diagnostics

phi<-sum(residuals(victim_1b_nb, type = "pearson")^2 / df.residual(victim_1b_nb))
print(paste("Overdisperso paramter (phi):", phi))
vif_values<-car::vif(victim_1b_nb)
print(vif_values)
dw_test<-dwtest(victim_1b_nb)
print(dw_test)

# Model 2b Summary

summary(victim_2b_nb)
exp(coef(victim_2b_nb))
exp(confint(victim_2b_nb))

### 5.4) Q3: Controlling for index age & general victimisation, how does historical FV and estimated age at first FV victimisation predict acute MHSU? ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 3a

victim_logreg_model_3a<-lrm(bin_acute_MHSU ~ index_age + hist_general_vic_count_iafm + num_hist_vic + est_age_first_afm, data = sp_fems_IPAafms) 

# Diagnostics 

vif_values<-vif(victim_logreg_model_3a)
print(vif_values)
dw_test<-dwtest(victim_logreg_model_3a)
print(dw_test)

# Model 3a Summary & ANOVA

victim_logreg_model_3a
summary(victim_logreg_model_3a, index_age = c(21, 47), hist_general_vic_count_iafm = c(1, 2), num_hist_vic = c(1, 2), est_age_first_afm = c(15, 28))
anova(victim_logreg_model_3a)

# Negative Binomial Model 3b 

victim_3b_nb<-glm.nb(MHSU_Acute_intensive_cat_total_lifetime ~ index_age + hist_general_vic_count_iafm + num_hist_vic + est_age_first_afm, data = sp_fems_IPAafms) 

# Diagnostics 

phi<-sum(residuals(victim_3b_nb, type = "pearson")^2 / df.residual(victim_3b_nb))
print(paste("Overdisperso paramter (phi):", phi))
vif_values<-car::vif(victim_3b_nb)
print(vif_values)
# note; high VIF for est_age_first_afm and index_age - investigate correlation 

correlation_matrix<-cor(model.matrix(victim_3b_nb))
print(correlation_matrix)
# note; 0.84 correlation between age and est age at first AFM - which makes sense.

dw_test<-dwtest(victim_3b_nb)
print(dw_test)

# Model 3b Summary 

summary(victim_3b_nb, index_age = c(21, 47), hist_general_vic_count_iafm = c(1, 2), num_hist_vic = c(1, 2), est_age_first_afm = c(15, 28))
exp(coef(victim_3b_nb))
exp(confint.default(victim_3b_nb))

### 5.5) Q4: Is there an interaction effect between historical general victimisation and historical FV victimisation in predicting acute MHSU? ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 4a

victim_logreg_model_4a<-lrm(bin_acute_MHSU ~ index_age + hist_general_vic_count_iafm + num_hist_vic + est_age_first_afm + hist_general_vic_count_iafm*num_hist_vic, data = sp_fems_IPAafms) 

# Diagnostics

vif_values<-vif(victim_logreg_model_4a)
print(vif_values)
dw_test<-dwtest(victim_logreg_model_4a)
print(dw_test)

# Model 4a Summary 

summary(victim_logreg_model_4a)
summary(victim_logreg_model_4a, index_age = c(21, 47), hist_general_vic_count_iafm = c(1, 2), num_hist_vic = c(1, 2), est_age_first_afm = c(15, 28))
victim_logreg_model_4a

exp(victim_logreg_model_4a)

# Negative Binomial Model 4b 

victim_4b_nb<-glm.nb(MHSU_Acute_intensive_cat_total_lifetime ~ index_age + hist_general_vic_count_iafm + num_hist_vic + est_age_first_afm + hist_general_vic_count_iafm*num_hist_vic, data = sp_fems_IPAafms) 

# Diagnostics 

phi<-sum(residuals(victim_4b_nb, type = "pearson")^2 / df.residual(victim_4b_nb))
print(paste("Overdisperso paramter (phi):", phi))
vif_values<-car::vif(victim_4b_nb)
print(vif_values)

correlation_matrix<-cor(model.matrix(victim_4b_nb))
print(correlation_matrix)
dw_test<-dwtest(victim_4b_nb)
print(dw_test)

# Model 4b Summary 

summary(victim_4b_nb, index_age = c(21, 47), hist_general_vic_count_iafm = c(1, 2), num_hist_vic = c(1, 2), est_age_first_afm = c(15, 28))
exp(coef(victim_4b_nb))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 6) Regression Analyses - Outpatient mental health service use ---------------------------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-


### 6.1) Q5: Controlling for index age, how does general victimisation (count) predict outpatient MHSU? ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 5a

victim_logreg_model_5a<-lrm(bin_outpatient_MHSU ~ index_age + hist_general_vic_count_iafm, data = sp_fems_IPAafms)

# Diagnostics

vif_values<-vif(victim_logreg_model_5a)
print(vif_values)
dw_test<-dwtest(victim_logreg_model_5a)
print(dw_test)

# Model 5a Summary 

summary(victim_logreg_model_5a, index_age = c(21, 47), hist_general_vic_count_iafm=c(1,2))
ggplot(Predict(victim_logreg_model_5a, fun = plogis))
anova(victim_logreg_model_5a)

# Negative Binomial Model 5b

victim_5b_nb<-glm.nb(MHSU_outpatient_public_community_cat_total_lifetime ~ index_age + hist_general_vic_count_iafm, data = sp_fems_IPAafms)

# Diagnostics 

phi<-sum(residuals(victim_5b_nb, type = "pearson")^2 / df.residual(victim_5b_nb))
print(paste("Overdisperso paramter (phi):", phi))
vif_values<-car::vif(victim_5b_nb)

print(vif_values)
dw_test<-dwtest(victim_5b_nb)
print(dw_test)

# Negative Binomial Model Summary 5b 

summary(victim_5b_nb)
exp(coef(victim_5b_nb))
exp(confint(victim_5b_nb))

### 6.2) Q6: Controlling for index age & general victimisation, how does historical FV victimisation predict outpatient MHSU? ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 6a

victim_logreg_model_6a<-lrm(bin_outpatient_MHSU ~ index_age + hist_general_vic_count_iafm + num_hist_vic, data = sp_fems_IPAafms) 

# Diagnostics

vif_values<-vif(victim_logreg_model_6a)
print(vif_values)
dw_test<-dwtest(victim_logreg_model_6a)
print(dw_test)

# Model 6a Summary & ANOVA

victim_logreg_model_6a
summary(victim_logreg_model_6a)
summary(victim_logreg_model_6a, index_age = c(21, 47), hist_general_vic_count_iafm = c(1, 2), num_hist_vic = c(1, 2))
anova(victim_logreg_model_6a)

# Negative Binomial Model 6b

victim_6b_nb<-glm.nb(MHSU_outpatient_public_community_cat_total_lifetime ~ index_age + hist_general_vic_count_iafm + num_hist_vic, data = sp_fems_IPAafms) 
summary(victim_6b_nb, index_age = c(21, 47), hist_general_vic_count_iafm = c(1, 2), num_hist_vic = c(1, 2))

# Diagnostics

phi<-sum(residuals(victim_6b_nb, type = "pearson")^2 / df.residual(victim_6b_nb))
print(paste("Overdisperso paramter (phi):", phi))
vif_values<-car::vif(victim_6b_nb)
print(vif_values)
dw_test<-dwtest(victim_6b_nb)
print(dw_test)

# Model 6b Summary

exp(coef(victim_6b_nb))
exp(confint(victim_6b_nb))

### 6.3) Q7: Controlling for index age & general victimisation, how does historical FV and estimated age at first FV victimisation predict outpatient MHSU? ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 7a

victim_logreg_model_7a<-lrm(bin_outpatient_MHSU ~ index_age + hist_general_vic_count_iafm + num_hist_vic + est_age_first_afm, data = sp_fems_IPAafms) 

# Diagnostics 

vif_values<-vif(victim_logreg_model_7a)
print(vif_values)
dw_test<-dwtest(victim_logreg_model_7a)
print(dw_test)

# Model 7a Summary & ANOVA

victim_logreg_model_7a
summary(victim_logreg_model_7a, index_age = c(21, 47), hist_general_vic_count_iafm = c(1, 2), num_hist_vic = c(1, 2), est_age_first_afm = c(15, 28))
anova(victim_logreg_model_7a)

# Negative Binomial Model 7b 

victim_7b_nb<-glm.nb(MHSU_outpatient_public_community_cat_total_lifetime ~ index_age + hist_general_vic_count_iafm + num_hist_vic + est_age_first_afm, data = sp_fems_IPAafms) 

# Diagnostics

phi<-sum(residuals(victim_7b_nb, type = "pearson")^2 / df.residual(victim_7b_nb))
print(paste("Overdisperso paramter (phi):", phi))
vif_values<-car::vif(victim_7b_nb)
print(vif_values)

correlation_matrix<-cor(model.matrix(victim_7b_nb))
print(correlation_matrix)

dw_test<-dwtest(victim_7b_nb)
print(dw_test)

# Model 7b Summary

summary(victim_7b_nb, index_age = c(21, 47), hist_general_vic_count_iafm = c(1, 2), num_hist_vic = c(1, 2), est_age_first_afm = c(15, 28))
exp(coef(victim_7b_nb))
exp(confint.default(victim_7b_nb))

### 6.4) Q8: Explore possible interaction effect between historical general victimisation and historical FV victimisation in predicting outpatient MHSU ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 8a

victim_logreg_model_8a<-lrm(bin_outpatient_MHSU ~ index_age + hist_general_vic_count_iafm + num_hist_vic + est_age_first_afm + hist_general_vic_count_iafm*num_hist_vic, data = sp_fems_IPAafms) 

# Lrm not providing needed stats for interaction - try dif package
victim_logreg_model_8a_test<-glm(bin_outpatient_MHSU ~ index_age + hist_general_vic_count_iafm + num_hist_vic + est_age_first_afm + hist_general_vic_count_iafm*num_hist_vic, data = sp_fems_IPAafms, 
                                 family=binomial(link='logit')) 

# Diagnostics

vif_values<-vif(victim_logreg_model_8a_test)
print(vif_values)
dw_test<-dwtest(victim_logreg_model_8a_test)
print(dw_test)

# Model 8a Summary & ANOVA

summary(victim_logreg_model_8a_test)
exp(coef(victim_logreg_model_8a_test))
exp(confint(victim_logreg_model_8a_test))
anova(victim_logreg_model_8a_test)

# Negative Binomial Model 8b 

victim_8b_nb<-glm.nb(MHSU_outpatient_public_community_cat_total_lifetime ~ index_age + hist_general_vic_count_iafm + num_hist_vic + est_age_first_afm + hist_general_vic_count_iafm*num_hist_vic, data = sp_fems_IPAafms) 

# Diagnostics

phi<-sum(residuals(victim_8b_nb, type = "pearson")^2 / df.residual(victim_8b_nb))
print(paste("Overdisperso paramter (phi):", phi))
vif_values<-car::vif(victim_8b_nb)
print(vif_values)

correlation_matrix<-cor(model.matrix(victim_8b_nb))
print(correlation_matrix)

dw_test<-dwtest(victim_8b_nb)
print(dw_test)

# Model 8b Summary 

summary(victim_8b_nb, index_age = c(21, 47), hist_general_vic_count_iafm = c(1, 2), num_hist_vic = c(1, 2), est_age_first_afm = c(15, 28))
exp(coef(victim_8b_nb))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 7) Regression Analyses - combined mental health service use ------------------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 7.1) Q9: Controlling for index age, how does general victimisation (count) predict combined MHSU? -------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 9a

victim_logreg_model_9a<-lrm(bin_combined_MHSU ~ index_age + hist_general_vic_count_iafm, data = sp_fems_IPAafms)

# Diagnostics

vif_values<-vif(victim_logreg_model_9a)
print(vif_values)
dw_test<-dwtest(victim_logreg_model_9a)
print(dw_test)

# Model 9a Summary, Visualisation & ANOVA

summary(victim_logreg_model_9a, index_age = c(21, 47), hist_general_vic_count_iafm=c(1,2))
ggplot(Predict(victim_logreg_model_9a, fun = plogis))
anova(victim_logreg_model_9a)

# Negative Binomial Model 9b 

victim_9b_nb<-glm.nb(combined_MHSU_lifetime ~ index_age + hist_general_vic_count_iafm, data = sp_fems_IPAafms)

# Diagnostics

phi<-sum(residuals(victim_9b_nb, type = "pearson")^2 / df.residual(victim_9b_nb))
print(paste("Overdisperso paramter (phi):", phi))
vif_values<-car::vif(victim_9b_nb)
print(vif_values)
dw_test<-dwtest(victim_9b_nb)
print(dw_test)

# Model 9b Summary 

summary(victim_9b_nb)
exp(coef(victim_9b_nb))
exp(confint(victim_9b_nb))

### 7.2) Q10: Controlling for index age & general victimisation, how does historical FV victimisation predict combined MHSU? ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 10a 

victim_logreg_model_10a<-lrm(bin_combined_MHSU ~ index_age + hist_general_vic_count_iafm + num_hist_vic, data = sp_fems_IPAafms) 

# Diagnostics 

vif_values<-vif(victim_logreg_model_10a)
print(vif_values)
dw_test<-dwtest(victim_logreg_model_10a)
print(dw_test)

# Model 10a Summary & ANOVA

victim_logreg_model_10a
summary(victim_logreg_model_10a)
summary(victim_logreg_model_10a, index_age = c(21, 47), hist_general_vic_count_iafm = c(1, 2), num_hist_vic = c(1, 2))
anova(victim_logreg_model_10a)

# Negative Binomial Model 10b 

victim_10b_nb<-glm.nb(combined_MHSU_lifetime ~ index_age + hist_general_vic_count_iafm + num_hist_vic, data = sp_fems_IPAafms) 
summary(victim_10b_nb, index_age = c(21, 47), hist_general_vic_count_iafm = c(1, 2), num_hist_vic = c(1, 2))

# Diagnostics

phi<-sum(residuals(victim_10b_nb, type = "pearson")^2 / df.residual(victim_10b_nb))
print(paste("Overdisperso paramter (phi):", phi))
vif_values<-car::vif(victim_10b_nb)
print(vif_values)
dw_test<-dwtest(victim_10b_nb)
print(dw_test)

# Model 10b Summary 

exp(coef(victim_10b_nb))
exp(confint(victim_10b_nb))

### 7.3) Q11: Controlling for index age & general victimisation, how does historical FV and estimated age at first FV victimisation predict combined MHSU? ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 11a

victim_logreg_model_11a<-lrm(bin_combined_MHSU ~ index_age + hist_general_vic_count_iafm + num_hist_vic + est_age_first_afm, data = sp_fems_IPAafms) 

# Diagnostics 

vif_values<-vif(victim_logreg_model_11a)
print(vif_values)
dw_test<-dwtest(victim_logreg_model_11a)
print(dw_test)

# Model 11a Summary & ANOVA 

victim_logreg_model_11a
summary(victim_logreg_model_11a, index_age = c(21, 47), hist_general_vic_count_iafm = c(1, 2), num_hist_vic = c(1, 2), est_age_first_afm = c(15, 28))
anova(victim_logreg_model_11a)

# Negative Binomial Model 11b 

victim_11b_nb<-glm.nb(combined_MHSU_lifetime ~ index_age + hist_general_vic_count_iafm + num_hist_vic + est_age_first_afm, data = sp_fems_IPAafms) 

# Diagnostics

phi<-sum(residuals(victim_11b_nb, type = "pearson")^2 / df.residual(victim_11b_nb))
print(paste("Overdisperso paramter (phi):", phi))
vif_values<-car::vif(victim_11b_nb)
print(vif_values)

correlation_matrix<-cor(model.matrix(victim_11b_nb))
print(correlation_matrix)

dw_test<-dwtest(victim_11b_nb)
print(dw_test)

# Model 11b Summary 

summary(victim_11b_nb, index_age = c(21, 47), hist_general_vic_count_iafm = c(1, 2), num_hist_vic = c(1, 2), est_age_first_afm = c(15, 28))
exp(coef(victim_11b_nb))
exp(confint.default(victim_11b_nb))

### 7.4) Q12: Explore possible interaction effect between historical general victimisation and historical FV victimisation in predicting combined MHSU ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 12a

victim_logreg_model_12a<-lrm(bin_combined_MHSU ~ index_age + hist_general_vic_count_iafm + num_hist_vic + est_age_first_afm + hist_general_vic_count_iafm*num_hist_vic, data = sp_fems_IPAafms) 

#lrm not providing needed stats for interaction - try dif package

victim_logreg_model_12a_test<-glm(bin_combined_MHSU ~ index_age + hist_general_vic_count_iafm + num_hist_vic + est_age_first_afm + hist_general_vic_count_iafm*num_hist_vic, data = sp_fems_IPAafms, 
                                 family=binomial(link='logit')) 

# Diagnostics 

vif_values<-vif(victim_logreg_model_12a)
print(vif_values)
dw_test<-dwtest(victim_logreg_model_12a)
print(dw_test)

# Model 12a Summary & ANOVA

summary(victim_logreg_model_12a_test)

exp(coef(victim_logreg_model_12a_test))
exp(confint(victim_logreg_model_12a_test))

summary(victim_logreg_model_12a_test, index_age = c(21, 47), hist_general_vic_count_iafm = c(1, 2), num_hist_vic = c(1, 2), est_age_first_afm = c(15, 28), hist_general_vic_count_iafm*num_hist_vic, data = sp_fems_IPAafms, 
        family=binomial(link='logit'))

anova(victim_logreg_model_12a_test)

# Negative Binomial Model 12b 

victim_12b_nb<-glm.nb(combined_MHSU_lifetime ~ index_age + hist_general_vic_count_iafm + num_hist_vic + est_age_first_afm + hist_general_vic_count_iafm*num_hist_vic, data = sp_fems_IPAafms) 

# Check for overdispersion, multicol & indep of observations

phi<-sum(residuals(victim_12b_nb, type = "pearson")^2 / df.residual(victim_4b_nb))
print(paste("Overdisperso paramter (phi):", phi))
vif_values<-car::vif(victim_12b_nb)
print(vif_values)
 
correlation_matrix<-cor(model.matrix(victim_12b_nb))
print(correlation_matrix)
dw_test<-dwtest(victim_12b_nb)
print(dw_test)

# Model 12b Summary 

summary(victim_12b_nb, index_age = c(21, 47), hist_general_vic_count_iafm = c(1, 2), num_hist_vic = c(1, 2), est_age_first_afm = c(15, 28))
exp(coef(victim_12b_nb))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
