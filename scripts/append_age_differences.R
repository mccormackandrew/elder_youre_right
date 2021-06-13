# CREATING AGE DIFFERENCE VARIABLES IN THE AFROBAROMETER DATASET ----
# This file incorporates age difference into the Adida et al Afrobarometer 
# dataset (Rounds 3 and 4)

library(tidyverse)
library(haven)
library(lubridate)

# "Not in" convenience operator
'%!in%' <- function(x,y)!('%in%'(x,y))

# Import original dataset ----
afpr <- read_rds("data_clean/afpr_append.rds")

# Unique idenitifier to merge in age difference dataframe later on
afpr$unique_identifier <- 1:nrow(afpr)

# GENERATE AGE DIFFERENCES ----
afpr_ages <- afpr[ , c("age", "intage", "dateintr", "unique_identifier")]

# Respondent birthdays
afpr_ages <- afpr_ages %>%
  mutate(# Get maximum possible birthday based on age and interview date
         # i.e. the date of the interview
         maximum_bday = ifelse( (is.na(dateintr) | is.na(age)),
                                as.character(NA),
                                paste0(year(dateintr) - age, "-", 
                                       format(dateintr, format="%m-%d"))),
         # Get minimum possible birthday based on age and interview date
         # i.e. exactly one year before the interview
         minimum_bday = ifelse( (is.na(dateintr) | is.na(age)), 
                                as.character(NA), 
                                paste0(year(dateintr) - age - 1, "-", 
                                       format(dateintr, format="%m-%d")))) %>%
  mutate(maximum_bday = as.Date(maximum_bday),
         minimum_bday = as.Date(minimum_bday)) %>%
  
  # For each respondent, randomly sample one of the birthdays in the sequence 
  # of possible birthdays
  rowwise() %>%
  mutate(resp_bday = ifelse(!is.na(minimum_bday),
                            as.character(sample(seq.Date(minimum_bday, 
                                                         maximum_bday, 
                                                         by = "day"), 1)),
                            as.character(NA)),
         resp_bday = as.Date(resp_bday)) %>%
  ungroup()

# Interviewer birthdays
afpr_ages <- afpr_ages %>%
  mutate(# Get maximum possible birthday based on age and interview date
         # i.e. the date of the interview
         maximum_bday = ifelse( (is.na(dateintr) | is.na(intage)),
                                as.character(NA),
                                paste0(year(dateintr) - intage, "-", 
                                       format(dateintr, format="%m-%d"))),
         # Get minimum possible birthday based on age and interview date
         # i.e. exactly one year before the interview
         minimum_bday = ifelse( (is.na(dateintr) | is.na(intage)), 
                                as.character(NA), 
                                paste0(year(dateintr) - intage - 1, "-", 
                                       format(dateintr, format="%m-%d")))
  ) %>%
  mutate(maximum_bday = as.Date(maximum_bday),
         minimum_bday = as.Date(minimum_bday)) %>%
  # For each respondent, randomly sample one of the birthdays in the sequence 
  # of possible birthdays
  rowwise() %>%
  mutate(int_bday = ifelse(!is.na(minimum_bday),
                           as.character(sample(seq.Date(minimum_bday, 
                                                        maximum_bday, 
                                                        by = "day"), 1)),
                           as.character(NA)),
         int_bday = as.Date(int_bday)) %>%
  ungroup()

# Age difference is interviewer birthday minus respondent birthday
afpr_ages$age_difference <- time_length(interval(afpr_ages$int_bday, 
                                                 afpr_ages$resp_bday), 
                                        unit = "year")

# GENERATE COARSENED AGE DIFFERENCES ----

afpr_ages <- afpr_ages %>%
  mutate(coarsened_age_10 = 
           case_when(age_difference < -10 ~ "Interviewer younger (10 year)",
                     age_difference > 10 ~ "Interviewer older (10 year)",
                     age_difference >= -10 | age_difference <= 10 ~ "Same age",
                     TRUE ~ NA_character_),
         coarsened_age_35 = 
           case_when(intage > 35 & age <= 35 ~ "Interviewer older (age 35 cutoff)",
                     intage <= 35 & age > 35 ~ "Interviewer younger (age 35 cutoff)",
                     intage <= 35 & age <= 35 ~ "Both younger (age 35 cutoff)",
                     intage > 35 & age > 35 ~ "Both older (age 35 cutoff)",
                     TRUE ~ NA_character_),
         coarsened_age_40 = 
           case_when(intage > 40 & age <= 40 ~ "Interviewer older (age 40 cutoff)",
                     intage <= 40 & age > 40 ~ "Interviewer younger (age 40 cutoff)",
                     intage <= 40 & age <= 40 ~ "Both younger (age 40 cutoff)",
                     intage > 40 & age > 40 ~ "Both older (age 40 cutoff)",
                     TRUE ~ NA_character_))

# JOIN IN ALL OTHER VARIABLES ---- 
afpr_ages <- afpr_ages %>% 
  dplyr::select(unique_identifier,
                resp_bday, 
                int_bday, 
                age_difference, 
                coarsened_age_10, 
                coarsened_age_35, 
                coarsened_age_40) %>%
  left_join(afpr, by = "unique_identifier")

# RECODE COARSENED AGE VARIABLES AS FACTORS ----
afpr_ages <- afpr_ages %>%
  mutate(coarsened_age_10 = fct_relevel(coarsened_age_10,
                                        "Same age", 
                                        "Interviewer older (10 year)",
                                        "Interviewer younger (10 year)"),
         coarsened_age_35 = fct_relevel(coarsened_age_35,
                                        "Both older (age 35 cutoff)", 
                                        "Interviewer younger (age 35 cutoff)",
                                        "Both younger (age 35 cutoff)",
                                        "Interviewer older (age 35 cutoff)"),
         coarsened_age_40 = fct_relevel(coarsened_age_40,
                                        "Both older (age 40 cutoff)", 
                                        "Interviewer younger (age 40 cutoff)",
                                        "Both younger (age 40 cutoff)",
                                        "Interviewer older (age 40 cutoff)"))



# Not clear from codebook what 7 represents 
# But relatively small number of cases suggests
# these are missing values
afpr_ages$trust_opposition <- ifelse(afpr_ages$trust_opposition == 7, NA, afpr_ages$trust_opposition)

# TRIBE ----
# Remove invalid tribes
afpr_ages <- afpr_ages %>%
  filter(!grepl("doesn’t think of self in those terms", tribe, ignore.case = TRUE)) %>%
  filter(tribe %!in% c("Refused to answer", 
                       "Don't know", 
                       "Don’t know",
                       "Not asked in the country")) %>%
  filter(!grepl("^Related to", tribe)) %>%
  filter(!is.na(tribe))

# 8343 cases removed from tribe

# Function to calculate modal value in a character vector
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

afpr_ages <- afpr_ages %>%
  group_by(country, round) %>%
  summarise(modal_tribe = calculate_mode(tribe)) %>%
  right_join(afpr_ages, by = c("country", "round")) %>%
  ungroup()

afpr_ages <- afpr_ages %>%
  mutate(minority = ifelse(modal_tribe != tribe, 1, 0))

# Noncoethnic dyad
afpr_ages <- afpr_ages %>%
  # Noncoeth is 1 when the respondent tribe doesn't match the interviewer tribe
  mutate(noncoeth_r7 = ifelse(tribe == enumeth, 0, 1)) %>%
  # If not round 7. we use Adida et al.'s original variable
  mutate(noncoeth = ifelse(is.na(noncoeth), noncoeth_r7, noncoeth))


# SAVE .rds FILE OF DATA ----
dir.create("./data_clean/", showWarnings = FALSE)
saveRDS(afpr_ages, "./data_clean/afpr_ages.rds")
