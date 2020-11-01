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

# RUN MODELS FOR +/- 10 YEAR AGE DIFFERENCE, OVER/UNDER 35-YEAR & 40-YEAR ----

# Define formula
# Outcomes and coarsened age variables will be glued in in the map2() function below
form_base  <- paste0("{outcome_variable} ~ {age_variable} + ",
                     #noncoeth
                     "age + gender + edu + ",
                     "urban + minority +",
                     "round + inhomelang | region + tribe + enumeth")
                     #+ tribe + enumtribes")




# Generate all outcome-coarsened age combinations
outcome_age_combos <- 
  expand.grid(outcome_variable = all_outcomes, 
              age_variable = c("coarsened_age_10", 
                               "coarsened_age_35", 
                               "coarsened_age_40"))


# This map2() function runs a model for each outcome (y), 
# with each age difference coarsening (x)
age_diff_models <- 
  map2(outcome_age_combos$age_variable,
       outcome_age_combos$outcome_variable,
       function(x, y) {
         
         age_variable <- x
         outcome_variable <- paste0("z_", y) # "z_" is standardized variables
         
         # Run model
         if(x == "coarsened_age_10") {
           # Note: no contrasts for 10-year age difference model
           # because we only have one baseline of interest: same age.
           model <- felm(as.formula(glue(form_base)), data = afpr)
         } else {
           
           contrasts_matrix_list <- list(x = contrasts_matrix)
           names(contrasts_matrix_list) <- age_variable
           
           model <- felm(as.formula(glue(form_base)), data = afpr, 
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
  do.call("rbind", .) %>%
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

# RUN MODELS FOR OVER/UNDER 35-YEAR WITH UNSTANDARIZED OUTCOME ----
# i.e. outcomes on original scale

outcome_age_combos <- 
  expand.grid(outcome_variable = all_outcomes, 
              age_variable = c("coarsened_age_10", 
                               "coarsened_age_35", 
                               "coarsened_age_40"))

age_diff_models_original_scale <- 
  map2(filter(outcome_age_combos, age_variable == "coarsened_age_35")$age_variable,
       filter(outcome_age_combos, age_variable == "coarsened_age_35")$outcome_variable,
       function(x, y) {
         
         age_variable <- x
         outcome_variable <- y # no "z_" like above because now original scale
         
         # Run model
         
         contrasts_matrix_list <- list(x = contrasts_matrix)
         names(contrasts_matrix_list) <- age_variable
           
         model <- felm(as.formula(glue(form_base)), data = afpr, 
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
  do.call("rbind", .) %>%
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
