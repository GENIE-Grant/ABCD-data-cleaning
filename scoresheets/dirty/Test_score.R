load('data/ALSPAC_wide.RData')

library(readxl)

BN_scoresheet <- read_excel('scoresheets/dx_scoresheets/BN_scoresheet.xlsx')

library(scorekeeper)



library(dplyr)
library(cgwtools)
library(tidyr)
library(datawizard)
library(stringr)
library(haven)

ALSPAC_wide.2 <- ALSPAC_wide |> 
  rename_with(~ str_replace(.x, pattern = '_167', replacement = '.167')) |> 
  rename_with(~ str_replace(.x, pattern = '_198', replacement = '.198')) |> 
  rename_with(~ str_replace(.x, pattern = '_216', replacement = '.216')) |> 
  rename_with(~ str_replace(.x, pattern = '_288', replacement = '.288'))

scorekeep(ALSPAC_wide.2, BN_scoresheet)
