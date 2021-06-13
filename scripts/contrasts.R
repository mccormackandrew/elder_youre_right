# MAIN RESULTS OF PAPER ----

# LOAD NECESSARY PACKAGES ----

library(tidyverse)
library(glue)
library(lfe)
library(lmtest)
library(sandwich)

# READ IN DATA AND FUNCTIONS ----

# Load Adida et al. Afrobarometer data with age difference appended
# age differences appended in scripts/adida_append_age_differences.R
afpr <- readRDS("./data_clean/afpr_ages.rds")

# Get list of outcomes and dataframe with outcome descriptions
source("scripts/variable_labels.R")

# Convenience function, opposite of %in%
'%!in%' <- function(x,y)!('%in%'(x,y))



# Reverse code present living conditions so direction is consistent with other variables
afpr$z_ec_conditions_self <- as.vector(scale(6 - afpr$ec_conditions_self))

# According to Adida et al.:
# "The estimates reported in Figures 1 to 4 correspond with Model 4, 
# the model that includes the full set of controls."
# This being the case, we will only use the 4th model here.

# DEFINE CONTRASTS ----

# To define contrasts, our coarsened age variables need to be factors
# with pre-defined levels. 
if(!is.factor(afpr$coarsened_age_10) |
   !is.factor(afpr$coarsened_age_35) |
   !is.factor(afpr$coarsened_age_40) ) {
  stop("Make sure that coarsened age variables are factors.")
}

# SETTING UP CONTRASTS ----

# contrast() function takes the inverse of the matrix of
# desired contrast weights. So although there are only
# two contrasts we care about, we need a square matrix,
# so we specify three contrasts, plus a row 1/4s for the constant
not_relevant <- c(0 , 0, 1,  0)
younger_int <- c( -1,   1,  0,    0 )
older_int <- c( 0,   0,   -1,   1 )
# Get inverse of contrast matrix and remove constant
contrasts_matrix <- solve(rbind(constant=1/4, not_relevant, younger_int, older_int))[ , -1]



# DEFINE MODEL FORMULA ----
# Outcomes and coarsened age variables will be glued in in the map2() function below
form_base  <- paste0("{outcome_variable} ~ {age_variable} + ",
                     "noncoeth +", 
                     "age + gender + edu + ",
                     "urban + minority +",
                     "round + inhomelang | region + tribe + enumeth")

# Generate all outcome-coarsened age combinations
outcome_age_combos_3_4 <- 
  expand.grid(outcome = c(pro_outcomes, pol_outcomes, stat_outcomes, eth_outcomes), 
              age_variable = c("coarsened_age_10", 
                               "coarsened_age_35", 
                               "coarsened_age_40"),
                stringsAsFactors = FALSE) %>%
  mutate(round = "3 and 4")

outcome_age_combos_7 <- 
  expand.grid(outcome = youth_outcomes, 
              age_variable = c("coarsened_age_10", 
                               "coarsened_age_35", 
                               "coarsened_age_40"),
              stringsAsFactors = FALSE) %>%
  mutate(round = "7")

outcome_age_combos <- bind_rows(outcome_age_combos_3_4, outcome_age_combos_7)

# RUN MODELS FOR ROUNDS 3 AND 4 (POL, PRO, STAT, AND ETH OUTCOMES) ----
# This map2() function runs a model for each outcome (y), 
# with each age difference coarsening (x)

age_diff_models <- 
  pmap_dfr(outcome_age_combos, function(outcome, age_variable, round) {
    
    if(round == "7") include_round <- 7 else include_round <- 3:4
    
    outcome_variable <- paste0("z_", outcome) # "z_" is standardized variables
         
         # Run model
         if(age_variable == "coarsened_age_10") {
           # Note: no contrasts for 10-year age difference model
           # because we only have one baseline of interest: same age.
           model <- felm(as.formula(glue(form_base)), data = afpr[afpr$round %in% include_round, ])
         } else {
           
           contrasts_matrix_list <- list(x = contrasts_matrix)
           names(contrasts_matrix_list) <- age_variable
           
           model <- felm(as.formula(glue(form_base)), data = afpr[afpr$round %in% include_round, ], 
                         contrasts = contrasts_matrix_list)
         }
         
         n_obs <- nrow(model$response)
         
         model <- model %>%
           # Extract estimates with robust standard errors
           summary(., robust = TRUE) %>%
           "$"(coefficients) %>%
           # Clean up into tidy dataframe
           data.frame() %>%
           rownames_to_column() %>%
           "names<-"(c("term", "estimate", "std.error", "statistic", "p.value")) %>%
           # Get just coefficients of interest
           filter(grepl("older_int|younger_int|Interviewer|noncoeth", term)) %>%
           # Add identifiers for outcome and age difference variable
           mutate(n_obs = n_obs,
                  outcome_variable = outcome_variable,
                  age_variable = age_variable) %>%
           # Calculate upper and lower confidence bands
           mutate(upper = estimate + qnorm(.975)*std.error,
                  lower = estimate - qnorm(.975)*std.error)
         
         return(model)
       }) %>%
  # Create variable labels column
  mutate(label = plyr::mapvalues(outcome_variable, 
                                 paste0("z_", variable_labels$var),
                                 as.character(variable_labels$label))) %>%
  # Make sure factor levels are correct for order of variables in plots
  mutate(label = factor(label, levels = variable_labels$label)) %>%
  # Create variable grouping column (i.e. stat_outcomes, pol_outcomes, etc.)
  mutate(group = plyr::mapvalues(outcome_variable, 
                                 paste0("z_", variable_labels$var),
                                 variable_labels$group))

# RUN ROUND 7 MODELS FOR JUST UGANDA, MAURITIUS, AND BOTH ----
# DOESN'T WORK RIGHT BECAUSE NO OLDER INTERVIEWERS IN UGANDA
# outcome_age_combos_uganda_mauritius <- 
#   expand.grid(outcome = youth_outcomes, 
#               age_variable = c("coarsened_age_10", 
#                                "coarsened_age_35", 
#                                "coarsened_age_40"),
#               country = c("Uganda", "Mauritius", "Both"),
#               stringsAsFactors = FALSE) 
# 
# # Not all variables are in both countries, so filter out models that are not possible
# filter_uganda_mauritius_grid <- pmap_lgl(outcome_age_combos_uganda_mauritius, function(outcome, age_variable, country) {
#   
#   if(country == "Both") country <- c("Uganda", "Mauritius")
#   
#   country_round_df <- afpr[afpr$round == 7 & afpr$country %in% country, ]
#   
#   na_rows <- country_round_df %>%
#     filter(is.na(!!sym(outcome))) %>%
#     nrow()
#   
#   !(na_rows == nrow(country_round_df))
#   
# })

# outcome_age_combos_uganda_mauritius <- outcome_age_combos_uganda_mauritius[filter_uganda_mauritius_grid, ]
# 
# age_diff_models_uganda_mauritius <- 
#   pmap_dfr(outcome_age_combos_uganda_mauritius[1:3, ], function(outcome, age_variable, country) {
#     
#     if(country == "Both") country <- c("Uganda", "Mauritius")
#     
#     outcome_variable <- paste0("z_", outcome) # "z_" is standardized variables
#     
#     # Run model
#     if(age_variable == "coarsened_age_10") {
#       # Note: no contrasts for 10-year age difference model
#       # because we only have one baseline of interest: same age.
#       model <- felm(as.formula(glue(form_base)), data = afpr[afpr$round == 7 & afpr$country %in% country, ])
#     } else {
#       
#       contrasts_matrix_list <- list(x = contrasts_matrix)
#       names(contrasts_matrix_list) <- age_variable
#       
#       model <- felm(as.formula(glue(form_base)), data = afpr[afpr$round == 7 & afpr$country %in% country, ], 
#                     contrasts = contrasts_matrix_list)
#     }
#     
#     n_obs <- nrow(model$response)
#     
#     model <- model %>%
#       # Extract estimates with robust standard errors
#       summary(., robust = TRUE) %>%
#       "$"(coefficients) %>%
#       # Clean up into tidy dataframe
#       data.frame() %>%
#       rownames_to_column() %>%
#       "names<-"(c("term", "estimate", "std.error", "statistic", "p.value")) %>%
#       # Get just coefficients of interest
#       filter(grepl("older_int|younger_int|Interviewer|noncoeth", term)) %>%
#       # Add identifiers for outcome and age difference variable
#       mutate(n_obs = n_obs,
#              outcome_variable = outcome_variable,
#              age_variable = age_variable) %>%
#       # Calculate upper and lower confidence bands
#       mutate(upper = estimate + qnorm(.975)*std.error,
#              lower = estimate - qnorm(.975)*std.error)
#     
#     return(model)
#   }) %>%
#   # Create variable labels column
#   mutate(label = plyr::mapvalues(outcome_variable, 
#                                  paste0("z_", variable_labels$var),
#                                  as.character(variable_labels$label))) %>%
#   # Make sure factor levels are correct for order of variables in plots
#   mutate(label = factor(label, levels = variable_labels$label)) %>%
#   # Create variable grouping column (i.e. stat_outcomes, pol_outcomes, etc.)
#   mutate(group = plyr::mapvalues(outcome_variable, 
#                                  paste0("z_", variable_labels$var),
#                                  variable_labels$group))

# RUN MODELS FOR OVER/UNDER 35-YEAR WITH UNSTANDARIZED OUTCOME ----
# i.e. outcomes on original scale

age_diff_models_original_scale <- outcome_age_combos %>%
  filter(age_variable == "coarsened_age_35") %>%
  pmap_dfr(function(outcome, age_variable, round) {
    
    if(round == "7") include_round <- 7 else include_round <- 3:4
    outcome_variable <- outcome
    
    # Run model
    contrasts_matrix_list <- list(x = contrasts_matrix)
    names(contrasts_matrix_list) <- age_variable
    
    model <- felm(as.formula(glue(form_base)), data = afpr[afpr$round %in% include_round, ], 
                  contrasts = contrasts_matrix_list)
    
    n_obs <- nrow(model$response)
    
    model <- model %>%
      # Extract estimates with robust standard errors
      summary(., robust = TRUE) %>%
      "$"(coefficients) %>%
      # Clean up into tidy dataframe
      data.frame() %>%
      rownames_to_column() %>%
      "names<-"(c("term", "estimate", "std.error", "statistic", "p.value")) %>%
      # Get just coefficients of interest
      filter(grepl("older_int|younger_int|Interviewer|noncoeth", term)) %>%
      # Add identifiers for outcome and age difference variable
      mutate(n_obs = n_obs,
             outcome_variable = outcome_variable,
             age_variable = paste0(age_variable, "_originalscale")) %>%
      # Calculate upper and lower confidence bands
      mutate(upper = estimate + qnorm(.975)*std.error,
             lower = estimate - qnorm(.975)*std.error)
    
    return(model)
  }) %>%
  # Create variable labels column
  mutate(label = plyr::mapvalues(outcome_variable, 
                                 variable_labels$var,
                                 as.character(variable_labels$label))) %>%
  # Make sure factor levels are correct for order of variables in plots
  mutate(label = factor(label, levels = variable_labels$label)) %>%
  # Create variable grouping column (i.e. stat_outcomes, pol_outcomes, etc.)
  mutate(group = plyr::mapvalues(outcome_variable, 
                                 variable_labels$var,
                                 variable_labels$group))

rbind(age_diff_models, age_diff_models_original_scale) %>%
  saveRDS(., "data_clean/age_diff_models.rds")


table(afpr$country)
