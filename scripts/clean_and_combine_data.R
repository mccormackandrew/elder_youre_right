library(haven)
library(labelled)
library(tidyverse)
source("scripts/variable_labels.R")

# "Not in" convenience operator
'%!in%' <- function(x,y)!('%in%'(x,y))

# Function to convert .sav variable labels into a vector

# label_to_vec <- function(data_set) {
#   
#   column_names <- names(data_set)
#   
#   labels <- lapply(column_names, function(x) {
#     data.frame(var = x,
#                label = attr(data_set[[x]], "label"))
#   }) %>%
#     bind_rows()
#   
#   return(labels)
# } 

# Import round 4 and 5 Afrobarometer data ----
afpr <- read_dta("data_raw/AFPR_data.dta")

# Import merged AB round 7 data ----
ab7 <- read_sav("data_raw/r7_merged_data_34ctry.release.sav") 

# Get variable names and save as csv for easy look-up

# label_to_vec(ab7) %>%
#   write.csv("data_clean/ab7_variables.csv")

# Remove Mauritius and Uganda
ab7 <- ab7 %>%
  # These countries will be added later with country-specific data
  filter(!(COUNTRY %in% c(32, # UGANDA
                          18))) # MAURITIUS

# Import Mauritius AB round 7 data ----
ab7_mauritius <- read_sav("data_raw/mau_r7_data_eng.sav")

# label_to_vec(ab7_mauritius) %>%
#   write.csv("data_clean/ab7_mauritius_variables.csv")

# Import Uganda AB round 7 data ----
ab7_uganda <- read_sav("data_raw/uga_r7_data.sav")

# label_to_vec(ab7_uganda) %>%
#   write.csv("data_clean/ab7_uganda_variables.csv")

# MATCHING MAIN OUTCOME AND CONTROL VARIABLES IN AB7 TO THOSE IN AFPR ----

# Create a vector where the values are the original variable names
# and the names are the names we need to match afpr

variable_vector <- c(crime = "Q10B",
                     noincome = "Q8E",
                     nocleanwater = "Q8B",
                     notenoughfood = "Q8A",
                     ec_conditions_self = "Q4B",
                     gov_manage_economy = "Q56A",
                     ec_conditions_ctry = "Q4A",
                     performance = "Q58A",
                     meeting = "Q21A",
                     demosupp = "Q28",
                     trust_opposition = "Q43F",
                     trust_rulingparty = "Q43E",
                     treatedunfairly_group = "Q85A",
                     idrank = "Q85B",
                     uncooperative = "Q110C",
                     suspicious = "Q110E",
                     hostile = "Q110A",
                     impatient = "Q110D",
                     youth_needs = "Q56R",
                     youth_employment = "Q82A_UGA",
                     youth_employment = "Q82A_MAU",
                     youth_drugabuse = "Q82C_MAU",
                     youth_smoking = "Q82E_MAU",
                     homelanguage = "Q2B",
                     interviewlanguage = "Q103",
                     enumeth = "Q117",
                     tribe = "Q84",
                     dateintr = "DATEINTR",
                     age = "Q1",
                     intage = "Q113",
                     urban = "URBRUR",
                     gender = "Q101",
                     region = "REGION",
                     country = "COUNTRY", 
                     edu = "Q97")


# Rename variables to match afpr ----
names(ab7) <- plyr::mapvalues(names(ab7), variable_vector, names(variable_vector))
names(ab7_mauritius) <- plyr::mapvalues(names(ab7_mauritius), variable_vector, names(variable_vector))
names(ab7_uganda) <- plyr::mapvalues(names(ab7_uganda), variable_vector, names(variable_vector))

# CONVERT REGION, COUNTRY, AND TRIBE TO CHARACTER ----
ab7$country <- as.character(as_factor(ab7$country))
ab7_mauritius$country <- "Mauritius"
ab7_uganda$country <- "Uganda"

ab7$region <- as.character(as_factor(ab7$region))
ab7_mauritius$region <- as.character(as_factor(ab7_mauritius$region))
ab7_uganda$region <- as.character(as_factor(ab7_uganda$region))

ab7$tribe <- as.character(as_factor(ab7$tribe))
ab7_mauritius$tribe <- as.character(as_factor(ab7_mauritius$tribe))
ab7_uganda$tribe <- as.character(as_factor(ab7_uganda$tribe))

# Keep only the variables we need
ab7 <- dplyr::select(ab7, one_of(names(variable_vector)))
ab7_mauritius <- dplyr::select(ab7_mauritius, one_of(names(variable_vector)))
ab7_uganda <- dplyr::select(ab7_uganda, one_of(names(variable_vector)))

# Combine full data, Mauritius, and Uganda dataframes ----
ab7 <- bind_rows(ab7, ab7_mauritius, ab7_uganda)

ab7 <- ab7 %>%
  mutate_at(vars(one_of("crime", "noincome", "notenoughfood", "nocleanwater", "edu", 
                        "ec_conditions_self", "gov_manage_economy", "ec_conditions_ctry", "meeting",
                        "demosupp", "trust_opposition", "trust_rulingparty", "uncooperative",
                        "suspicious", "hostile", "impatient", "youth_needs", "youth_employment",
                        "youth_drugabuse", "youth_smoking")),
            list(~ifelse(. %in% c(-1, 8, 9, 99, 98), NA, .))) %>%
  mutate(performance = case_when(performance %in% c(1, 2) ~ 0,
                                 performance %in% c(3, 4) ~ 1,
                                 performance %in% c(99, 9, 8, -1) ~ NA_real_)) %>%
  mutate(demosupp = case_when(demosupp %in% c(1, 2) ~ 0,
                              demosupp == 3 ~ 1,
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

ab7$region <- tolower(ab7$region)
afpr$region <- as.character(as_factor(afpr$region))
afpr$region[grepl("ou\x8em\x8e", afpr$region)] <- "oueme"

afpr$tribe <- as.character(as_factor(afpr$tribe))

afpr$country <- as.character(as_factor(afpr$country))
ab7$country <- as.character(as_factor(ab7$country))

ab7 <- ab7 %>%
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

# KEEP ONLY THE VARIABLES WE NEED ----

ab7 <- ab7 %>%
  mutate(inhomelang = ifelse(homelanguage == interviewlanguage, 1, 0))

# STANDARDIZE AB7 VARIABLES ----

afpr <- afpr %>%
  dplyr::select(one_of(unique(c(names(ab7), all_outcomes))), minority)

afpr <- bind_rows(afpr, ab7)

afpr <- afpr %>%
  mutate_at(vars(one_of(all_outcomes)),
            list(z = ~as.vector(scale(.)))) %>%
  rename_at(vars(ends_with("_z")),
            list(~paste0("z_", gsub("_z", "", .))))

write_rds(afpr, "data_clean/afpr_append.rds")
