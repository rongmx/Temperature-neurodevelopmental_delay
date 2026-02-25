# -----------------------------------------------------------------------------
# SCRIPT DESCRIPTION
# -----------------------------------------------------------------------------
# This script performs data cleaning, definition for suspected developmental 
# delay (SDD) and baseline descriptive analysis to prepare a dataset for 
# evaluating environmental impacts on child neurodevelopment.
#
# -----------------------------------------------------------------------------
# DATA STRUCTURES
# -----------------------------------------------------------------------------
# The script loads 'original_cncmd_data.CSV'. This original
# dataframe is expected to contain:
#
# OUTCOME VARIABLE:
#   - score_cm (Numeric): Total score for the ASQ-3 Communication domain (Range: 0–60).
#   - score_gm (Numeric): Total score for the ASQ-3 Gross Motor domain (Range: 0–60).
#   - score_fm (Numeric): Total score for the ASQ-3 Fine Motor domain (Range: 0–60).
#   - score_cg (Numeric): Total score for the ASQ-3 Problem Solving domain (Range: 0–60).
#   - score_ps (Numeric): Total score for the ASQ-3 Person-Social domain (Range: 0–60).
#   - CM_SDD (factor): SDD in Communication domain ("0","1").
#   - GM_SDD (factor): SDD in Gross Motor domain ("0","1").
#   - FM_SDD (factor): SDD in Fine Motor domain ("0","1").
#   - CG_SDD (factor): SDD in Problem Solving domain ("0","1").
#   - PS_SDD (factor): SDD in Person-Social domain ("0","1").
#   - SDD (factor): Overall SDD ("0","1").
#
# EXPOSURE VARIABLE:
#   - Average daily mean temperatures (Numeric):"preg_avg_tem" and "X0_3y_avg_tem"
#   - Average daily minimal temperatures (Numeric):"preg_min_tem" and "X0_3y_min_tem"
#   - Heatwave Exposures (Numeric): A series of variables like "num_hw_preg_1" and "num_hw_birth_1"
#     for the number of heatwave days experienced during pregnancy and after birth, respectively.
#
# Covariates:
#   - "sex" (Factor), "child_age" (Numeric), "BAZ" (body mass index z-score,Numeric), 
#     "birth_weight" (Numeric), "delivery_mode" (Factor: vaginal, caesarean),
#     "neonatal_ward" (Factor: yes, no),
#     "drug" (Factor: yes, no), "breastfeeding" (Factor: yes, no),
#     "birth_season" (Factor), "pre_momage" (Numeric), "maternal_gravidity" (Factor), 
#     "mom_edu" (Factor), "dad_edu" (Factor), "mom_occupation" (Factor),
#     "marital_status" (Factor), "family" (Factor), "HouseholdIncome" (Factor),
#     "urban" (Factor), "PM25_0_3y" (Numeric),"PM25_preg" (Numeric),
#     "preg_rhu" (Numeric),"X0_3y_rhu" (Numeric), "ac_count" (Numeric),
#     "preg_complications" (Factor), "birth_week" (Numeric), "Region" (Factor: north, south)
# -----------------------------------------------------------------------------


# --- SCRIPT START ---
# Load R libraries
library(dplyr)
library(readr)
library(table1)
library(magrittr)

# Read the original data
dat <- read.csv("original_cncmd_data.CSV",fileEncoding = "GBK")

model_dat <- dat %>%
# Inclusion and exclusion criteria
  filter(
    child_age >= 3 & child_age <= 5.5,
    twin == 0,
    pre_mommage >= 18,
    PM25_0_3y >0
  ) %>%
  filter(across(
    c(CM_SDD, GM_SDD, FM_SDD, CG_SDD, PS_SDD, score_cm, score_gm, score_fm, score_cg, score_ps,
      preg_avg_tem, X0_3y_avg_tem, sex, BAZ, birth_weight, marital_status, preg_complications, birth_week), 
    ~!is.na(.x)
  )) %>%
# Add and define the SDD outcome variable
  mutate(SDD = if_else(
    CM_SDD == 0 & GM_SDD == 0 & FM_SDD == 0 & CG_SDD == 0 & PS_SDD == 0, 
    0, 1
  )) %>%
# Add and define the birth season variable
  mutate(
    birth = as.Date(birth, format="%Y/%m/%d"),
    month = as.numeric(format(birth, "%m")),
    birth_season = case_when(
      month %in% c(12, 1, 2) ~ "winter",
      month %in% c(3, 4, 5) ~ "spring",
      month %in% c(6, 7, 8) ~ "summer",
      month %in% c(9, 10, 11) ~ "autumn"
    )
  )

#Factorize variables
model_dat <- model_dat %>%
  mutate(
    sex = factor(sex, levels = c(1, 2), labels = c("Boys", "Girls")),
    delivery_mode = factor(delivery_mode, levels = c(1, 2), labels = c("Vaginal delivery", "Cesarean delivery")),
    neonatal_ward = factor(neonatal_ward, levels = c(1, 0), labels = c("Yes", "No")),
    breastfeeding = factor(breastfeeding, levels = c(1, 0), labels = c("Yes", "No")),
    drug = factor(drug, levels = c(1, 0), labels = c("Yes", "No")),
    maternal_gravidity = factor(maternal_gravidity, levels = c(0, 1), labels = c("Primigravida", "Multigravida")),
    mom_edu = factor(mom_edu, levels = c(1, 2, 3), labels = c("Middle school or below", "High school", "College or above")),
    dad_edu = factor(dad_edu, levels = c(1, 2, 3), labels = c("Middle school or below", "High school", "College or above")),
    mom_occupation = factor(mom_occupation, levels = c(1, 2, 3), labels = c("Worker/Business", "Unemployed", "Others")),
    marital_status = factor(marital_status, levels = c(1, 2), labels = c("First marriage", "Others")),
    family = factor(family, levels = c(1, 2, 3), labels = c("Nuclear", "Linear", "Joint")),
    HouseholdIncome = factor(HouseholdIncome, levels = c(1, 2, 3, 0), labels = c("≤30k", "(30,100]k", ">100k", "unknown")),
    SDD = factor(SDD, levels = c(1, 0), labels = c("SDD", "Normal"))
  )

# Descriptive statistics
# Table 1
table1(~ sex + child_age + BAZ + birth_weight + factor(delivery_mode) + factor(neonatal_ward) + factor(drug) + factor(family) + factor(HouseholdIncome) + factor(preg_complications)
       + factor(breastfeeding) + pre_momage + factor(maternal_gravidity) + factor(mom_edu) + factor(dad_edu) + factor(mom_occupation) + factor(marital_status) 
       + factor(birth_season)| SDD, data=model_dat)

# Table 2
## pregnancy
summary(model_dat$preg_avg_tem)
quantile(model_dat$preg_avg_tem, c(0.05, 0.15, 0.85, 0.95))
## 0-3 years
summary(model_dat$X0_3y_avg_tem)
quantile(model_dat$X0_3y_avg_tem, c(0.05, 0.15, 0.85, 0.95))

# Table S1
table1(~  score_cm + score_gm + score_fm + score_cg + score_ps + factor(SDD)| sex, data=model_dat)



##The model data "model_dat" has been prepared and is ready for subsequent modeling and analysis.
