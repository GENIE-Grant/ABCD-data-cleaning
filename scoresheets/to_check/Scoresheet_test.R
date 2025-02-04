library(scorekeeper)
library(dplyr)
library(cgwtools)
library(tidyr)
library(datawizard)
library(stringr)
library(haven)


BN_scoresheet <- readxl::read_xlsx('scoresheets/dx_scoresheets/BN_scoresheet.xlsx')
load('data/ALSPAC_wide.RData')

ALSPAC_wide.2 <- ALSPAC_wide |> 
  rename_with(~ str_replace(.x, pattern = '_167', replacement = '.167')) |> 
  rename_with(~ str_replace(.x, pattern = '_198', replacement = '.198')) |> 
  rename_with(~ str_replace(.x, pattern = '_216', replacement = '.216')) |> 
  rename_with(~ str_replace(.x, pattern = '_288', replacement = '.288'))

ALSPAC_BN <- scorekeep(ALSPAC_wide.2, BN_scoresheet)

ALSPAC_BN <- ALSPAC_BN[[5]]



