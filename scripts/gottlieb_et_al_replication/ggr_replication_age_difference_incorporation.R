# Do Men and Women Have Different Policy Preferences in Africa? Determinants and Implications of Gender Gaps in Policy Prioritization
# Add in an age difference variables 

library(tidyverse)
library(readr)
library(lubridate)
library(haven)

####################
#### MERGE DATA ####
####################

ab5 <- read_spss("data_raw/afrobarometer_data/merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav")
ab4 <- read_spss("data_raw/afrobarometer_data/merged_r4_data.sav")

gottlieb <- read_dta("data_raw/gottlieb_data/dataverse_files/ggross_r4r5append.dta")

# SELECT ONLY RESPNO AND AGE DIFF
# CREATE UNIQUE IDENTIFIER FOR MERGING (BECAUSE RESP #s are repeated across waves)
ab5 <- ab5 %>%
  dplyr::select(Q101, Q114, Q113, RESPNO, DATEINTR) %>%
  dplyr::rename(respno_join = RESPNO,
         int_age = Q113,
         int_gender = Q114,
         resp_gender = Q101) %>%
  mutate(respno_round = paste0(respno_join, "5"))



# SELECT ONLY RESPNO AND AGE DIFF
# CREATE UNIQUE IDENTIFIER FOR MERGING (BECAUSE RESP #s are repeated across waves)
ab4 <- ab4 %>%
  dplyr::select(Q101, Q112, Q111, RESPNO, DATEINTR) %>%
  dplyr::rename(respno_join = RESPNO,
         int_age = Q111, 
         int_gender = Q112,
         resp_gender = Q101) %>%
  mutate(respno_round = paste0(respno_join, "4"))

# COMBINE AB4 and 5
ab45 <- rbind(ab5, ab4)

# Keep ab45 observations where there is a respno match in gottlieb
ab45 <- ab45 %>%
  filter(respno_join %in% gottlieb$RESPNO) %>%
  mutate(respno_join = as.character(respno_join))

# Convert gottlieb respno to character (to get around Stata/SPSS attribute mismatch problem)
gottlieb$respno_join <- as.character(gottlieb$RESPNO)

# Create unique identifier (this is the same identifier as in ab45) for merging
gottlieb$respno_round <- paste0(gottlieb$respno_join, gottlieb$ROUND)

# Merge in interviewer age data
gottlieb <- gottlieb %>%
  left_join(ab45)



# gottlieb <- gottlieb_copy
# gottlieb_copy <- gottlieb

##########################################
#### GENERATE AGE DIFFERENCE VARIABLE ####
##########################################

#### CREATE BIRTHDAYS FOR RESPONDENTS ####


gottlieb <- gottlieb %>%
  mutate(maximum_bday = ifelse( (is.na(DATEINTR) | is.na(Q1)),
                                as.character(NA),
                                paste0(year(DATEINTR) - Q1, "-", format(DATEINTR, format="%m-%d"))),
         minimum_bday = ifelse( (is.na(DATEINTR) | is.na(Q1)), 
                                as.character(NA), 
                                paste0(year(DATEINTR) - Q1 - 1, "-", format(DATEINTR, format="%m-%d")))
  ) %>%
  mutate(maximum_bday = as.Date(maximum_bday),
         minimum_bday = as.Date(minimum_bday)) %>%
  rowwise() %>%
  mutate(RESP_BDAY = ifelse(!is.na(minimum_bday),
                       as.character(sample(seq.Date(minimum_bday, maximum_bday, by = "day"), 1)),
                       as.character(NA)),
         RESP_BDAY = as.Date(RESP_BDAY)
  )

#### CREATE BIRTHDAYS FOR INTERVIEWERS ####

# No longer need rowwise df, convert back to normal settings
class(gottlieb) <- c("tbl_df", "tbl", "data.frame")

gottlieb <- gottlieb %>%
  mutate(maximum_bday = ifelse( (is.na(DATEINTR) | is.na(int_age)),
                                as.character(NA),
                                paste0(year(DATEINTR) - int_age, "-", format(DATEINTR, format="%m-%d"))),
         minimum_bday = ifelse( (is.na(DATEINTR) | is.na(int_age)), 
                                as.character(NA), 
                                paste0(year(DATEINTR) - int_age - 1, "-", format(DATEINTR, format="%m-%d")))
  ) %>%
  mutate(maximum_bday = as.Date(maximum_bday),
         minimum_bday = as.Date(minimum_bday)) %>%
  rowwise() %>%
  mutate(INT_BDAY = ifelse(!is.na(minimum_bday),
                            as.character(sample(seq.Date(minimum_bday, maximum_bday, by = "day"), 1)),
                            as.character(NA)),
         INT_BDAY = as.Date(INT_BDAY)
  )

class(gottlieb) <- c("tbl_df", "tbl", "data.frame")

## CREATE AGE DIFFERENCE VARIABLE
gottlieb$age_difference <- time_length(interval(gottlieb$INT_BDAY, gottlieb$RESP_BDAY), unit = "year")

## CREATE COARSENED AGE VARIABLE
## 10-year
gottlieb <- gottlieb %>%
  mutate(coarsened_age = case_when(age_difference < -10 ~ "Interviewer younger",
                                   age_difference > 10 ~ "Inteviewer older",
                                   age_difference >= -10 | age_difference <= -10 ~ "Same age",
                                   TRUE ~ NA_character_))
## Older/younger than 35
gottlieb <- gottlieb %>%
  mutate(coarsened_age_35 = case_when(int_age > 35 & Q1 <= 35 ~ "Interviewer older",
                                      int_age <= 35 & Q1 > 35 ~ "Interviewer younger",
                                      int_age <= 35 & Q1 <= 35 ~ "Both younger",
                                      int_age > 35 & Q1 > 35 ~ "Both older",
                                      TRUE ~ NA_character_))

gottlieb$same_gender <- ifelse(gottlieb$resp_gender == gottlieb$int_gender, 1, 0)
gottlieb$different_gender <- ifelse(gottlieb$resp_gender == gottlieb$int_gender, 0, 1)

gottlieb <- gottlieb %>%
  mutate(gender_dyad = case_when(int_gender == 2 & resp_gender == 2 ~ "Both female",
                                 int_gender == 1 & resp_gender == 1 ~ "Both male",
                                 int_gender == 2 & resp_gender == 1 ~ "Int. female, resp. male",
                                 int_gender == 1 & resp_gender == 2 ~ "Int. male, resp. female"))

## Create variable of all combinations of gender dyad and age differences
gottlieb$age_difference_gender <- paste0(gottlieb$coarsened_age_35, ", ", gottlieb$gender_dyad)
# Remove dyads that include NA (749 in total)
gottlieb$age_difference_gender[grepl("NA", gottlieb$age_difference_gender)] <- NA

write_dta(gottlieb, "data_clean/gottlieb_clean/ggross_r4r5append_agediff.dta", version = 13)

## Respondent older than interviewer is positive, if interviewer is older then negative
par(mfrow = c(1,3))
hist(gottlieb$Q1, breaks = 40, main = "Respondent age", xlim = c(18, 90))
hist(gottlieb$int_age, breaks = 40, main = "Interviewer age", xlim = c(18, 90))
hist(gottlieb$age_difference, breaks = 40, main = "Age difference")
