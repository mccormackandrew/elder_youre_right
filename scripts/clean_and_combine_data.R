library(haven)
library(labelled)
library(tidyverse)
source("scripts/variable_labels.R")

# "Not in" convenience operator
'%!in%' <- function(x,y)!('%in%'(x,y))

# Function to convert .sav variable labels into a vector

label_to_vec <- function(data_set) {

  column_names <- names(data_set)

  labels <- lapply(column_names, function(x) {
    data.frame(var = x,
               label = attr(data_set[[x]], "label"))
  }) %>%
    bind_rows()

  return(labels)
}

# Import round 4 and 5 Afrobarometer data ----
afpr <- read_dta("data_raw/AFPR_data.dta")
afpr$int_id <- afpr$q110

# Investiage missing noncoeth and minority
afpr %>%
  filter(is.na(noncoeth) | is.na(minority)) %>%
  select(tribe, enumeth) 

# All missing noncoeth and minority in round 3 and 4 are because
# either respondent or interviewer tribe are missing

# Import merged AB round 7 data ----
ab7 <- read_sav("data_raw/r7_merged_data_34ctry.release.sav") 

error("What's the matter with Mozambique")
# Enumeth not available for Mozambique in round 3
afpr %>%
  filter(round == 3) %>%
  filter(country == 12) %>%
  pull(enumeth)

# Get variable names and save as csv for easy look-up
label_to_vec(ab7) %>%
  write.csv("data_clean/ab7_variables.csv")

# Remove Mauritius and Uganda
ab7 <- ab7 %>%
  # These countries will be added later with country-specific data
  filter(!(COUNTRY %in% c(32, # UGANDA
                          18))) # MAURITIUS

# Import Mauritius AB round 7 data ----
ab7_mauritius <- read_sav("data_raw/mau_r7_data_eng.sav")

label_to_vec(ab7_mauritius) %>%
  write.csv("data_clean/ab7_mauritius_variables.csv")

# Import Uganda AB round 7 data ----
ab7_uganda <- read_sav("data_raw/uga_r7_data.sav")

label_to_vec(ab7_uganda) %>%
  write.csv("data_clean/ab7_uganda_variables.csv")

# MATCHING MAIN OUTCOME AND CONTROL VARIABLES IN AB7 TO THOSE IN AFPR ----

# Create a vector where the values are the original variable names
# and the names are the names we need to match afpr

variable_vector <- c(Q10B = "crime",
                     Q8E = "noincome",
                     Q8B = "nocleanwater",
                     Q8A = "notenoughfood",
                     Q4B = "ec_conditions_self",
                     Q56A = "gov_manage_economy",
                     Q4A = "ec_conditions_ctry",
                     Q58A = "performance",
                     Q21A = "meeting",
                     Q28 = "demosupp",
                     Q43F = "trust_opposition",
                     Q43E = "trust_rulingparty",
                     Q85A = "treatedunfairly_group",
                     Q85B = "idrank",
                     Q110C = "uncooperative",
                     Q110E = "suspicious",
                     Q110A = "hostile",
                     Q110D = "impatient",
                     Q56R = "youth_needs",
                     Q82A_UGA = "youth_employment",
                     Q82A_MAU = "youth_employment",
                     Q82B_MAU = "youth_pregnancy",
                     Q82C_MAU = "youth_drugabuse",
                     Q82D_MAU = "youth_alcohol",
                     Q82E_MAU = "youth_delinquency",
                     Q82F_MAU = "youth_smoking",
                     Q2B = "homelanguage",
                     Q103= "interviewlanguage",
                     Q112 = "int_id",
                     Q117= "enumeth",
                     Q84 = "tribe",
                     DATEINTR = "dateintr",
                     Q1 = "age",
                     Q113 = "intage",
                     URBRUR = "urban",
                     Q101 = "gender",
                     REGION= "region",
                     COUNTRY = "country", 
                     Q97 = "edu")


# RENAME VARIABLES TO MATCH AFPR ----
names(ab7) <- dplyr::recode(names(ab7), !!!variable_vector)
names(ab7_mauritius) <- dplyr::recode(names(ab7_mauritius), !!!variable_vector)
names(ab7_uganda) <- dplyr::recode(names(ab7_uganda), !!!variable_vector)

# CONVERT REGION, COUNTRY, RESPONDENT TRIBE AND INTERVIEWER TRIBE TO CHARACTER ----
ab7$country <- as.character(as_factor(ab7$country))
ab7_mauritius$country <- "Mauritius"
ab7_uganda$country <- "Uganda"

ab7$region <- remove_attributes(to_character(ab7$region), "label")
ab7_mauritius$region <- remove_attributes(to_character(ab7_mauritius$region), "label")
ab7_uganda$region <- remove_attributes(to_character(ab7_uganda$region), "label")

ab7$tribe <- remove_attributes(to_character(ab7$tribe), "label")
ab7_mauritius$tribe <- remove_attributes(to_character(ab7_mauritius$tribe), "label")
ab7_uganda$tribe <- remove_attributes(to_character(ab7_uganda$tribe), "label")

ab7$enumeth <- remove_attributes(to_character(ab7$enumeth), "label")
ab7_mauritius$enumeth <- remove_attributes(to_character(ab7_mauritius$enumeth), "label")
ab7_uganda$enumeth <- remove_attributes(to_character(ab7_uganda$enumeth), "label")

# KEEP ONLY THE VARIABLES WE NEED ----
ab7 <- dplyr::select(ab7, one_of(variable_vector))
ab7_mauritius <- dplyr::select(ab7_mauritius, one_of(variable_vector))
ab7_uganda <- dplyr::select(ab7_uganda, one_of(variable_vector))

# COMBINE ALL AB7 DATA ----
# This includes ab7 (excluding Mauritius), and Mauritius ab7 
ab7_full <- bind_rows(ab7, ab7_mauritius, ab7_uganda) 

# Remove North Africa
ab7_full <- ab7_full %>%
  filter(country %!in% c("Morocco", "Tunisia", "Sudan"))

ab7_full <- ab7_full %>%
  mutate(
    across(
      one_of("crime", "noincome", "notenoughfood", "nocleanwater", "edu",
             "ec_conditions_self", "gov_manage_economy", "ec_conditions_ctry", "meeting",
             "demosupp", "trust_opposition", "trust_rulingparty", "uncooperative",
             "suspicious", "hostile", "impatient", 
             "youth_needs", "youth_employment",
             "youth_pregnancy", "youth_drugabuse", 
             "youth_delinquency",
             "youth_alcohol", "youth_smoking",
             "youth_smoking"),
      ~ifelse(. %in% c(-1, 8, 9, 99, 98), NA, .)
      )
    ) %>%
  mutate(performance = case_when(performance %in% c(1, 2) ~ 0, # Disapprove
                                 performance %in% c(3, 4) ~ 1, # Approve
                                 performance %in% c(99, 9, 8, -1) ~ NA_real_)) %>%
  mutate(demosupp = case_when(demosupp %in% c(1, 2) ~ 0, # Doesn't matter/sometimes non-democratic preferable
                              demosupp == 3 ~ 1, # Democracy preferable
                              demosupp %in% c(-1, 8, 9) ~ NA_real_)) %>%
  mutate(treatedunfairly_group =ifelse(treatedunfairly_group %in% c(-1, 7, 8, 9, 99), 
                                       NA, 
                                       treatedunfairly_group)) %>%
  mutate(idrank = ifelse(idrank %in% c(-1, 7, 8, 9, 99), NA, idrank - 1)) %>%
  mutate(gender = case_when(gender == 2 ~ 0,
                            gender == 1 ~ 1,
                            gender == -1 ~ NA_real_)) %>%
  mutate(urban = ifelse(urban == 1, 1, 0)) %>%
  mutate(round = 7)



# FIX THE REGION VARIABLE SO IT MATCHES IN ALL ROUNDS ----

ab7_full$region <- tolower(ab7_full$region)
afpr$region <- remove_attributes(to_character(afpr$region), "label")
afpr$region[grepl("ou\x8em\x8e", afpr$region)] <- "oueme"

# Some manual edits to make regions match between afpr and ab7
ab7_full <- ab7_full %>%
  mutate(region = case_when(region == "akwa ibom" ~ "akwa-ibom",
                            region == "centre est" ~ "centre-east",
                            region == "centre sud" ~ "centre-south",
                            region == "centre ouest" ~ "centre-west",
                            region == "centre nord" ~ "centre-north",
                            region == "est" ~ "east",
                            region == "nord" ~ "north",
                            region %in% c("kavango east", "zambezi") ~ "caprivi",
                            region == "collines" ~ "collilnes",
                            region == "copperbelt" ~ "copper belt",
                            region == "cross river" ~ "cross-river",
                            region == "fct abuja" ~ "fct",
                            region == "hauts bassins" ~ "hauts-bassins",
                            region == "kavango west" ~ "kavango",
                            region == "mashonaland east" ~ "mahonaland east",
                            region == "mashonaland west" ~ "mahonaland west",
                            region == "nasarawa" ~ "nassarawa",
                            region == "north western" ~ "north-western",
                            region == "otjozondjupa" ~ "otjozundjupa",
                            region == "saint-louis" ~ "saint louis",
                            region == "southern region" ~ "south",
                            region == "central region" ~ "central",
                            region == "northern region" ~ "north",
                            region == "zambézia" ~ "zambezia",
                            region == "matabeleland north" ~ "matebeleland north",
                            region == "matabeleland south" ~ "matebeleland south",
                            TRUE ~ region))

afpr$tribe <- remove_attributes(to_character(afpr$tribe), "label")
afpr$country <- remove_attributes(to_character(afpr$country), "label")
ab7_full$country <- remove_attributes(to_character(ab7_full$country), "label")


# GENERATE IN HOME LANGUAGE VARIABLE ----
ab7_full <- ab7_full %>%
  mutate(inhomelang = ifelse(homelanguage == interviewlanguage, 1, 0))

# Convert afpr enumeth variable to character so it matches
# the format of the ab7_full enumeth
afpr$enumeth <- remove_attributes(to_character(afpr$enumeth), "label")

ab7_full <- ab7_full %>%
  mutate(tribe = case_when(country == "São Tomé and Príncipe" & tribe == "Africans" ~ "Africanos",
                           country == "São Tomé and Príncipe" & tribe == "Doesn’t think of self in those terms" ~ "National identity",
                           country == "Mauritius" & grepl("Mauritian only", tribe) ~ "Mauritian",
                           TRUE ~ tribe)) %>%
  mutate(enumeth = case_when(country == "São Tomé and Príncipe" & enumeth == "Africans" ~ "Africanos",
                             country == "São Tomé and Príncipe" & grepl("National identity only", enumeth) ~ "National identity",
                             country == "Mauritius" & grepl("Mauritian only", enumeth) ~ "Mauritian",
                             TRUE ~ enumeth)) 

# KEEP ONLY THE VARIABLES WE NEED ----
afpr <- afpr %>%
  dplyr::select(# Covariates
                enumeth, tribe, int_id, round,
                dateintr, age, intage, urban, gender, region,
                country, edu, inhomelang, minority, noncoeth,
                # Outcomes of interest
                one_of(all_outcomes)) 

afpr <- bind_rows(afpr, ab7_full)

# STANDARDIZE VARIABLES ----
afpr <- afpr %>%
  mutate(across(one_of(all_outcomes),
                ~as.vector(scale(.)),
                .names = "z_{col}")) 

# Make it so that age is not class "labelled double"
# but just double
afpr$age <- as.numeric(afpr$age)
afpr$age <- ifelse(afpr$age %in% c(998, 999), NA, afpr$age)

# FIX MISSING VALUES IN trust_opposition ----
# Not clear from codebook what 7 represents in trust opposition
# But relatively small number of cases suggests
# these are missing values
afpr$trust_opposition <- ifelse(afpr$trust_opposition == 7, NA, afpr$trust_opposition)

# TRIBE ----
# Remove invalid tribes
# afpr <- afpr %>%
#   filter(!grepl("doesn’t think of self in those terms", tribe, ignore.case = TRUE)) %>%
#   filter(tribe %!in% c("Refused to answer",
#                        "Don't know",
#                        "Don’t know",
#                        "Not asked in the country")) %>%
#   filter(!grepl("^Related to", tribe)) %>%
#   filter(!is.na(tribe))

# 8343 cases removed from tribe

# Function to calculate modal value in a character vector
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

afpr <- afpr %>%
  group_by(country, region, round) %>%
  summarise(modal_tribe = calculate_mode(tribe)) %>%
  right_join(afpr, by = c("country", "region", "round")) %>%
  ungroup()

afpr <- afpr %>%
  mutate(minority_new = ifelse(modal_tribe != tribe, 1, 0)) %>%
  # Keep Adida et al.'s minority variable for rounds 3 and 4
  # Use newly constructed version (constructed with same procedure as Adida et al.)
  # for round 7
  mutate(minority = ifelse(is.na(minority), minority_new, minority))

# Noncoethnic dyads ----
afpr <- afpr %>%
  # Noncoeth is 1 when the respondent tribe doesn't match the interviewer tribe
  mutate(noncoeth_r7 = ifelse(tribe == enumeth, 0, 1)) %>%
  # Keep Adida et al.'s noncoethnic variable for rounds 3 and 4
  # Use newly constructed version (constructed with same procedure as Adida et al.)
  # for round 7
  mutate(noncoeth = ifelse(is.na(noncoeth), noncoeth_r7, noncoeth))

write_rds(afpr, "data_clean/afpr_append.rds")