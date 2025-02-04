library(scorekeeper)
library(dplyr)
library(cgwtools)
library(tidyr)
library(datawizard)
library(stringr)
library(haven)

load('data/ALSPAC_long.RData')
load('data/ALSPAC_wide.RData')

ALSPAC_wide <- ALSPAC_wide |> 
rename_with(~ str_replace_all(.x, pattern = "_([0-9]{2,3})", replacement = ".\\1"))

load('data/ALSPAC_cleaned.RData') 

BMI_data <- ALSPAC_cleaned$BMI |> 
  select(!contains('bestavail')) 

library(tidyr)

BMI_data <- BMI_data |> 
  pivot_longer(cols = c(height_clinic, height_pub, weight_clinic, weight_pub, bmi_clinic, bmi_pub, bmiz_clinic, bmiz_pub),
library(tidyr)

load('data/ALSPAC_Cleaned.RData')

BMI_data <- ALSPAC_cleaned$BMI |> 
  select(!contains('bestavail')) |> 
  pivot_longer(cols = c(height_clinic, height_pub, weight_clinic, weight_pub, bmi_clinic, bmi_pub, bmiz_clinic, bmiz_pub, agemos_clinic, agemos_pub, agedays_clinic, agedays_pub, agewks_clinic, agewks_pub),
               names_sep = "\\_",
               names_to = c(".value", "bmi_collection_type")) |> 
  filter(!rowSums(across(height:bmiz, is.na)) == 4)

BMIZ_7_12 <- BMI_data |> 
  filter(as.numeric(assess_agemos) > 7*12 & as.numeric(assess_agemos) < 12*12) |> 
  group_by(id) |> 
  mutate(mbmiz_7_12 = mean(bmiz, na.rm = TRUE)) |> 
  select(id, mbmiz_7_12) |> 
  unique() |> 
  filter(!is.na (mbmiz_7_12))

ALSPAC_wide <- full_join(ALSPAC_wide, BMI_7_12)

ALSPAC_wide <- ALSPAC_wide |> 
  mutate(bmiz_bestavail.168 =  if_else(!is.na(bmiz.1990_clinic.165), bmiz.1990_clinic.165, bmiz_pub.157)) |> 
  mutate(bmiz_drop.167_1 = mbmiz_7_12 - bmiz_bestavail.168) |> 
  mutate(bmiz_drop.167 = case_when(bmiz_drop.167_1 > .5 ~ 1,
                                   bmiz_drop.167_1 < .5 ~ 0 ))
  


AN_dx <- scorekeeper::scorekeep(ALSPAC_wide, AN_score)[[3]]
AAN_dx <- scorekeeper::scorekeep(ALSPAC_wide, AAN_score)[[3]]
Restrict_dx <- full_join(AAN_dx, AN_dx)

save(Restrict_dx, file = 'data/Restrict_Dx.RData')


ALSPAC_wide <- ALSPAC_wide |> 
  rename(parent_highest_occ = parent_highest_occupation_104) |> 
  rename(ethnicity_m = ethnicity_mum_104) |> 
  rename(ehtnicity_p = ethnicity_ptnr_104) |> 
  select(!contains('parent_highest_occupation')) |> 
  select(!contains('ethnicity_mum')) |> 
  select(!contains('ethnicity_ptnr'))

ALSPAC_wide <- ALSPAC_wide |> 
  rename_with(~ str_replace_all(.x, pattern = "_([0-9]{2,3})", replacement = ".\\1"))

load('data/ALSPAC_cleaned.RData'
