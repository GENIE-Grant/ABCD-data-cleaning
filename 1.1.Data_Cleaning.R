
# Load and clean anthropometric data ######
ant <- read.csv('ABCD_5.0/abcd-data-release-5.0/core/physical-health/ph_y_anthro.csv', header = TRUE)

# Load demographic data
demos <- read.csv('ABCD_5.0/abcd-data-release-5.0/core/abcd-general/abcd_p_demo.csv', 
                  header = TRUE)

sex <- demos |> 
  select(src_subject_id, demo_sex_v2) |> 
  # remove rows with NA
  filter(!is.na(demo_sex_v2))

age <- demos <- read.csv('ABCD_5.0/abcd-data-release-5.0/core/abcd-general/abcd_y_lt.csv', header = TRUE)
age <- age |> 
  select(src_subject_id, eventname, interview_age)

# add sex to ant data
ant <- full_join(ant, sex, by = c("src_subject_id"))
ant <- full_join(ant, age, by = c("src_subject_id", "eventname"))

# Derive needed variables for z-scoring

ant <- ant |> 
  mutate(anthroweight1lb = as.numeric(anthroweight1lb)) |> 
  mutate(anthroweight2lb = as.numeric(anthroweight2lb)) |> 
  mutate(anthroweight3lb = as.numeric(anthroweight3lb)) |> 
  mutate(BMI = (703 * anthroweightcalc) / as.numeric(anthroheightcalc)^2)

ant <- ant |> 
  mutate(age_days = as.numeric(interview_age)*30.437) |> 
  mutate(ht = as.numeric(anthroheightcalc)*2.54)  |> 
  mutate(wt = anthroweightcalc*0.453592) |> 
  # make sex missing if it is not == 1 or 2
  mutate(sex = case_when(
    demo_sex_v2 == 1 ~ 1, 
    demo_sex_v2 == 2 ~ 2,
    TRUE ~ NA_real_) )


ant <- TeenGrowth::add_bmiz(data = ant, 
                            wt ='wt', 
                            ht = 'ht', 
                            age = 'age_days', 
                            sex = 'sex')


save(ant, file = 'data/ant.RData')

# Load and clean ED data ######

rm(list = ls())

ed_parents <- read.csv('ABCD_5.0/abcd-data-release-5.0/core/mental-health/mh_p_ksads_ed.csv', header = TRUE)
ed_youth <- read.csv('ABCD_5.0/abcd-data-release-5.0/core/mental-health/mh_y_ksads_ed.csv', header = TRUE)

# parent ksads symptosm and diagnoses
ed_symptoms_parents <- read.csv('ABCD_5.0/abcd-data-release-5.0/core/mental-health/mh_p_ksads_ss.csv', header = TRUE)
ed_symptoms_youth <- read.csv('ABCD_5.0/abcd-data-release-5.0/core/mental-health/mh_y_ksads_ss.csv', header = TRUE)


# filter ed data to only include rows for ed_youth where ED data is not all missing

# ED sympotms 
ed_s_vars_y <- c("ksads_13_66_t", # fear of obese current
                 "ksads_13_67_t", # fear of obese past
                 "ksads_13_68_t", # emaciation current
                 "ksads_13_69_t", # emaciation past
                 "ksads_13_70_t", # vomitting current
                 "ksads_13_71_t", # vomitting past
                 "ksads_13_72_t", # laxative/dietpills/exercise current
                 "ksads_13_73_t", # laxative/dietpills/exercise past
                 "ksads_13_74_t", # binge eating current
                 "ksads_13_75_t" # binge eating past
)

ed_svars_p <- c("ksads_13_66_p", # fear of obese current
                "ksads_13_67_p", # fear of obese past
                "ksads_13_68_p", # emaciation current
                "ksads_13_69_p", # emaciation past
                "ksads_13_70_p", # vomitting current
                "ksads_13_71_p", # vomitting past
                "ksads_13_72_p", # laxative/dietpills/exercise current
                "ksads_13_73_p", # laxative/dietpills/exercise past
                "ksads_13_74_p", # binge eating current
                "ksads_13_75_p" # binge eating past
)

# select ed_svars_y columns from ed_symptoms_youth, along with src_subject_id and eventname
ed_s_youth <- ed_symptoms_youth %>%
  select(src_subject_id, eventname, ed_s_vars_y)

# filter ed_s_youth to only include rows where columns 3:12 are not all NA
ed_s_youth <- ed_s_youth %>%
  filter(rowSums(is.na(ed_s_youth[,3:12])) < 10)

# select ed_svars_p columns from ed_symptoms_parents, along with src_subject_id and eventname
ed_s_parents <- ed_symptoms_parents %>%
  select(src_subject_id, eventname, ed_svars_p)

# filter ed_s_parents to only include rows where columns 3:12 are not all NA
ed_s_parents <- ed_s_parents %>%
  filter(rowSums(is.na(ed_s_parents[,3:12])) < 10)

# remove ed_symptoms_youth and ed_symptoms_parents from memory
rm(ed_symptoms_youth)
rm(ed_symptoms_parents)

# rename columns in ed_s_youth and ed_s_parents to be more descriptive
colnames(ed_s_youth) <-c("src_subject_id", "eventname",
                         "fowg_current_scored", "fowg_past_scored",
                         "lowwt_current_scored", "lowwt_fowg_past_scored", 
                         "wtcontrol_vomit_current_scored", "wtcontrol_vomit_past_scored", 
                         "wtcontrol_other_current_scored", "wtcontrol_other_past_scored", 
                         "binge_current_scored", "binge_past_scored")

colnames(ed_s_parents) <- c("src_subject_id", "eventname",
                            "fowg_current_scored", "fowg_past_scored",
                            "lowwt_current_scored", "lowwt_fowg_past_scored", 
                            "wtcontrol_vomit_current_scored", "wtcontrol_vomit_past_scored", 
                            "wtcontrol_other_current_scored", "wtcontrol_other_past_scored", 
                            "binge_current_scored", "binge_past_scored")


# add a column to ed_s_youth and ed_s_parents that indicates reporter type (youth or parent)
ed_s_youth$reporter <- "youth"
ed_s_parents$reporter <- "parent"

# combine ed_s_youth and ed_s_parents into a single dataframe
ed_symptoms_scored <- rbind(ed_s_youth, ed_s_parents)

# remove ed_s_youth and ed_s_parents from memory
rm(ed_s_youth)
rm(ed_s_parents)


# read in ED_vars_datadictionary
ed_vars_dictionary_youth <- readxl::read_excel('ABCD_5.0/ED_vars_datadictionary.xlsx', sheet = 1)
ed_vars_dictionary_parents <- readxl::read_excel('ABCD_5.0/ED_vars_datadictionary.xlsx', sheet = 2)

# 3. Rename the data using the key
ed_parents_renamed <- ed_parents
names(ed_parents_renamed) <- ed_vars_dictionary_parents$new_name[match(names(ed_parents_renamed),
                                                                       ed_vars_dictionary_parents$var_name)]

# 4. (Optional) check final column names
names(ed_parents_renamed)

ed_youth_renamed <- ed_youth
names(ed_youth_renamed) <- ed_vars_dictionary_youth$new_name[match(names(ed_youth_renamed),
                                                                   ed_vars_dictionary_youth$var_name)]

names(ed_youth_renamed)

ed_youth_renamed$reporter <- "youth"
ed_parents_renamed$reporter <- "parent"

ed_all <- rbind(ed_youth_renamed, ed_parents_renamed)

# Combine ed_all and ed_symptoms scored into a single dataframe with a join by src_subject_id and eventname
ed_all <- full_join(ed_all, ed_symptoms_scored, by = c("src_subject_id", "eventname", "reporter"))

# merge all the ed_aao variables into one variable

ed_all <- ed_all %>%
  mutate(across(starts_with("edsymptoms_aoo"), as.Date, format = "%Y-%m-%d"))  # Adjust format as needed


ed_all <- ed_all %>%
  mutate(
    edsymptoms_aoo = coalesce(
      edsymptoms_aoo_1,  edsymptoms_aoo_2,  edsymptoms_aoo_3,  edsymptoms_aoo_4,
      edsymptoms_aoo_5,  edsymptoms_aoo_6,  edsymptoms_aoo_7,  edsymptoms_aoo_8,
      edsymptoms_aoo_9,  edsymptoms_aoo_10, edsymptoms_aoo_11, edsymptoms_aoo_12,
      edsymptoms_aoo_13, edsymptoms_aoo_14, edsymptoms_aoo_15, edsymptoms_aoo_16,
      edsymptoms_aoo_17, edsymptoms_aoo_18, edsymptoms_aoo_19, edsymptoms_aoo_20
    )
  )

# remove individual edsymptoms_aoo columns from ed_all
ed_all <- ed_all %>%
  select(-starts_with("edsymptoms_aoo_"))

# pull out binge eating columns from ed_all

# save the ed_all dataframe
save(ed_all, file = "data/ed_all.RData")

# clear environment
rm(list = ls())

load("data/ed_all.RData")

ed_vars_dictionary <- readxl::read_excel('ABCD_5.0/ED_vars_datadictionary.xlsx', sheet = 1)

# Ensure your dataset's column names match the data dictionary's `var_name`

# Match and apply labels
for (i in seq_len(nrow(ed_vars_dictionary))) {
  var <- ed_vars_dictionary$new_name[i]
  label <- ed_vars_dictionary$var_label[i]
  
  # Check if the variable exists in the dataset
  if (var %in% names(ed_all)) {
    ed_all[[var]] <- labelled::set_variable_labels(ed_all[[var]], label)
  }
}



binge <- ed_all |> 
  select(src_subject_id, eventname, reporter, starts_with("binge"))

# if binge_current_scored == 1, binge_past == 1
binge <- binge |>
  mutate(binge_lifetime = ifelse(binge_current_scored == 1, 1, binge_past))

binge_keyvars <- binge |> 
  select(src_subject_id, eventname, reporter, binge_frequency, binge_current_scored, binge_past_scored, binge_lifetime)

# 1. Create a named vector that maps each eventname to a numeric code
timepoint_map <- c(
  "baseline_year_1_arm_1"     = 0,
  "1_year_follow_up_y_arm_1"  = 1,
  "2_year_follow_up_y_arm_1"  = 2,
  "3_year_follow_up_y_arm_1"  = 3,
  "4_year_follow_up_y_arm_1"  = 4
)

# 2. Use it to create a numeric variable:
binge_keyvars$timepoint_num <- timepoint_map[binge_keyvars$eventname]


# make a variable called binge_rolling. For each subject id, start with the lowest timepoint. If binge_lifetime == 1 at that timepoint, set binge_rolling to 1. Set binge_rolling to 1 for all subsequent timepoints for that subject id. If binge_rolling == 0 for the first timepoint, move to the next timepoint and repeat the process.

binge_keyvars <- binge_keyvars %>%
  # Arrange by subject ID and timepoint
  arrange(src_subject_id, timepoint_num) %>%
  # Group by subject ID
  group_by(src_subject_id, reporter) %>%
  # Create binge_rolling
  mutate(
    binge_rolling = if_else(cumsum(binge_lifetime) > 0, 1, 0)
  ) %>%
  # Ungroup to remove grouping
  ungroup()

# quick table of binge_rolling by reporter and timepoint
table(binge_keyvars$reporter, binge_keyvars$timepoint_num, binge_keyvars$binge_rolling)

# save the binge_keyvars dataframe
save(binge_keyvars, file = "data/binge_keyvars.RData")

# select all the wt_control variables
wt_control <- ed_all |> 
  select(src_subject_id, eventname, reporter, starts_with("wtcontrol"))
# Example var_map
var_map <- list(
  restrict = list(
    main = "wtcontrol_zerocal"
  ),
  exercise = list(
    main = "wtcontrol_exercise"
  ),
  dietpills = list(
    main = "wtcontrol_dietpills"
  ),
  waterpills = list(
    main = "wtcontrol_waterpill"
  ),
  laxative = list(
    main = "wtcontrol_laxative"
  ),
  vomit = list(
    main = "wtcontrol_vomit"
  )
)

# Loop over the behaviors
for (b in names(var_map)) {
  
  # Build the new column names for current and past
  new_current_col <- paste0("wtcontrol_", b, "_current")  
  new_past_col    <- paste0("wtcontrol_", b, "_past")     

  # Build the existing column names for current and past
  main_current_col <- var_map[[b]]$main
  main_past_col    <- paste0(var_map[[b]]$main, "_past")     

  # Create the "current" column
  wt_control[[new_current_col]] <- dplyr::case_when(
    wt_control$wtcontrol_other_current_scored == 0 ~ 0,
    wt_control[[main_current_col]] == 0            ~ 0,
    wt_control[[main_current_col]] == 1            ~ 1
  )

  # Create the "past" column
  wt_control[[new_past_col]] <- dplyr::case_when(
    wt_control$wtcontrol_other_past_scored == 0 ~ 0,
    wt_control[[main_past_col]] == 0           ~ 0,
    wt_control[[main_past_col]] == 1           ~ 1
  )
}

# combine wwaterpills and dietpills into one column -- make it 'pills' and set it to 1 if either waterpills or dietpills is 1
wt_control$wtcontrol_pills_current <- ifelse(wt_control$wtcontrol_waterpills_current == 1 | wt_control$wtcontrol_dietpills_current == 1, 1, 0)
wt_control$wtcontrol_pills_past <- ifelse(wt_control$wtcontrol_waterpills_past == 1 | wt_control$wtcontrol_dietpills_past == 1, 1, 0)

# now make lifetime versions of the variables. If the current version is 1, set the lifetime version to 1. If the current version is 0, set the lifetime version to the past version.
wt_control$wtcontrol_pills_lifetime <- ifelse(wt_control$wtcontrol_pills_current == 1, 1, wt_control$wtcontrol_pills_past)
wt_control$wtcontrol_restrict_lifetime <- ifelse(wt_control$wtcontrol_restrict_current == 1, 1, wt_control$wtcontrol_restrict_past)
wt_control$wtcontrol_exercise_lifetime <- ifelse(wt_control$wtcontrol_exercise_current == 1, 1, wt_control$wtcontrol_exercise_past)
wt_control$wtcontrol_laxative_lifetime <- ifelse(wt_control$wtcontrol_laxative_current == 1, 1, wt_control$wtcontrol_laxative_past)
wt_control$wtcontrol_vomit_lifetime <- ifelse(wt_control$wtcontrol_vomit_current == 1, 1, wt_control$wtcontrol_vomit_past)

# 1. Create a named vector that maps each eventname to a numeric code
timepoint_map <- c(
  "baseline_year_1_arm_1"     = 0,
  "1_year_follow_up_y_arm_1"  = 1,
  "2_year_follow_up_y_arm_1"  = 2,
  "3_year_follow_up_y_arm_1"  = 3,
  "4_year_follow_up_y_arm_1"  = 4
)

# 2. Use it to create a numeric variable:
wt_control$timepoint_num <- timepoint_map[wt_control$eventname]


# now , using the same logic as binge rolling, create pills, restrict, exercise, and laxative rolling variables
wt_control <- wt_control %>%
  # Arrange by subject ID and timepoint
  arrange(src_subject_id, timepoint_num) %>%
  # Group by subject ID
  group_by(src_subject_id, reporter) %>%
  # Create binge_rolling
  mutate(
    wtcontrol_pills_rolling = if_else(cumsum(wtcontrol_pills_lifetime) > 0, 1, 0),
    wtcontrol_restrict_rolling = if_else(cumsum(wtcontrol_restrict_lifetime) > 0, 1, 0),
    wtcontrol_exercise_rolling = if_else(cumsum(wtcontrol_exercise_lifetime) > 0, 1, 0),
    wtcontrol_laxative_rolling = if_else(cumsum(wtcontrol_laxative_lifetime) > 0, 1, 0),
    wtcontrol_vomit_rolling = if_else(cumsum(wtcontrol_vomit_lifetime) > 0, 1, 0)
  ) %>%
  # Ungroup to remove grouping
  ungroup()

# Check results
names(wt_control)



# remove wtcontrol_ from the beginning of all column names
names(wt_control) <- gsub("wtcontrol_", "", names(wt_control))

# make a list of key variables

key_vars <- c("src_subject_id", "eventname","timepoint_num", "reporter", 
"pills_current", "pills_past", "pills_lifetime", "pills_rolling", 
              "restrict_current", "restrict_past", "restrict_lifetime", "restrict_rolling",
              "exercise_current", "exercise_past", "exercise_lifetime", "exercise_rolling",
              "laxative_current", "laxative_past", "laxative_lifetime", "laxative_rolling",
              "vomit_current", "vomit_past", "vomit_lifetime", "vomit_rolling", 
              "past_duration", "past_date", "other_frequency", "vomit_frequency")

# select only the key variables
wt_control <- wt_control[, key_vars]

# rename past_duration to wtcontrol_past_duration and past_date to wtcontrol_past_date, and other_frequency to wtcontrol_other_frequency
names(wt_control)[names(wt_control) == "past_duration"] <- "wtcontrol_past_duration"
names(wt_control)[names(wt_control) == "past_date"] <- "wtcontrol_past_date"
names(wt_control)[names(wt_control) == "other_frequency"] <- "wtcontrol_other_frequency"

# save
save(wt_control, file = "data/wt_control.RData")
