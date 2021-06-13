# Balance on demographic variables (Rounds 3/4 and Round 7) ----

# Load packages
library(tidyverse)
library(xtable)
library(gridExtra)
library(fastDummies)

# Load data 
afpr <- readRDS("./data_clean/afpr_ages.rds")

# Recode variables ----

# Make gender binary
afpr$female <- ifelse(afpr$gender == 0, 1, 0)

# Recode education into 3 categories
afpr <- afpr %>%
  mutate(education = case_when(edu %in% c(0, 1, 2, 3) ~ "Less than high school",
                               edu %in% c(4, 5) ~ "High school",
                               edu %in% c(6, 7, 8, 9) ~ "Post-secondary"))

# Make dummy variables so easy to plug into rank sum test
afpr <- cbind(afpr, fastDummies::dummy_cols(afpr$education) %>%
                     `colnames<-`(gsub(".data_", "", colnames(.))))


# Define function that outputs means and p-values for all age combination categories ----

# Define t-test function ----
t_test_group_fun <- function(variable, data) {
  
  # Older respondents subset
  older_t_df <- data %>%
    dplyr::select(coarsened_age_35, !!dplyr::sym(variable)) %>%
    dplyr::rename(variable = !!dplyr::sym(variable)) %>%
    filter(coarsened_age_35 %in% c("Both older (age 35 cutoff)", "Interviewer younger (age 35 cutoff)"))
  
  # Younger respondents subset 
  younger_t_df <- data %>%
    dplyr::select(coarsened_age_35, !!dplyr::sym(variable)) %>%
    dplyr::rename(variable = !!dplyr::sym(variable)) %>%
    filter(coarsened_age_35 %in% c("Both younger (age 35 cutoff)", "Interviewer older (age 35 cutoff)"))
  
  # Run the t-tests
  t_older <- t.test(older_t_df$variable ~ older_t_df$coarsened_age_35)
  t_younger <- t.test(younger_t_df$variable ~ younger_t_df$coarsened_age_35)
  
  # Return the name of the variable and the p-values for the results
  return(data.frame(Variable = variable, 
                    p_older = t_older$p.value, 
                    p_younger = t_younger$p.value))
}

# Define rank sum  function ----
ranksum_group_fun <- function(variable, data) {
  
  # Older respondents subset
  older_ranksum_df <- data %>%
    dplyr::select(coarsened_age_35, !!dplyr::sym(variable)) %>%
    dplyr::rename(variable = !!dplyr::sym(variable)) %>%
    filter(coarsened_age_35 %in% c("Both older (age 35 cutoff)", "Interviewer younger (age 35 cutoff)"))
  
  # Younger respondents subset 
  younger_ranksum_df <- data %>%
    dplyr::select(coarsened_age_35, !!dplyr::sym(variable)) %>%
    dplyr::rename(variable = !!dplyr::sym(variable)) %>%
    filter(coarsened_age_35 %in% c("Both younger (age 35 cutoff)", "Interviewer older (age 35 cutoff)"))
  
  # Run the rank sum tests
  ranksum_older <- wilcox.test(older_ranksum_df$variable ~ older_ranksum_df$coarsened_age_35, exact = FALSE)
  ranksum_younger <- wilcox.test(younger_ranksum_df$variable ~ younger_ranksum_df$coarsened_age_35, exact = FALSE)
  
  return(data.frame(Variable  = variable, 
                    p_older = ranksum_older$p.value, 
                    p_younger = ranksum_younger$p.value))
}

mean_group_fun <- function(variable, data ) {
  
  sum_table <- data %>%
    filter(!is.na(coarsened_age_35)) %>%
    group_by(coarsened_age_35) %>%
    summarise(mean = mean(!!sym(variable), na.rm = TRUE)) %>%
    spread(coarsened_age_35, mean) %>%
    mutate(Variable = variable) %>%
    dplyr::select(Variable, everything())
  return(sum_table)
}

demographic_groups <- c("age", "female", "Less than high school",
  "High school", "Post-secondary", "urban",
  "noncoeth", "minority", "inhomelang")

# Balance table for rounds 3 and 4 ----

statistical_tests_3_4 <- map(demographic_groups, function(x) {
  if(x == "age") {
    t_test_group_fun(x, data = afpr[afpr$round %in% 3:4, ])
  } else {
    ranksum_group_fun(x, data = afpr[afpr$round %in% 3:4, ])
  }
}) %>%
  do.call("rbind", .) 

means_3_4 <- map(demographic_groups, ~mean_group_fun(., data = afpr[afpr$round %in% 3:4, ])) %>%
  do.call("rbind", .)

# Calculate mean differences
means_table_3_4 <- means_3_4 %>%
  `colnames<-`(trimws(gsub("\\(|\\)|\\`|\\`|age 35 cutoff", "", colnames(.)))) %>%
  mutate(difference_older = `Both older` - `Interviewer younger`,
         differnece_younger = `Both younger` - `Interviewer older`) %>%
  dplyr::select(Variable, 
                `Both older`, `Interviewer younger`, difference_older,
                `Both younger`, `Interviewer older`, differnece_younger)

# Format means table to have significance stars if p < 0.05
balance_table_3_4 <- means_table_3_4 %>%
  left_join(statistical_tests_3_4) %>%
  mutate_at(vars(-one_of("Variable", "p_older", "p_younger", "difference_older", "differnece_younger")), list(~round(., 3))) %>%
  mutate_at(vars(one_of("difference_older", "differnece_younger")), list(~round(., 2))) %>%
  mutate(difference_older = case_when(p_older >= .05 ~ as.character(difference_older),
                                      p_older < 0.05 ~ paste0(difference_older, "*")),
         differnece_younger = case_when(p_younger >= .05 ~ as.character(differnece_younger),
                                        p_younger < .05 ~ paste0(differnece_younger, "*"))) %>%
  dplyr::select(-p_older, -p_younger)

# Write balance table as CSV to be imported into MICROSOFT WORD ----
write.csv(as.data.frame(balance_table_3_4), "tables/balance_table_rounds_3_and_4.csv")

# Balance table for rounds 7 ----

statistical_tests_7 <- map(demographic_groups, function(x) {
  if(x == "age") {
    t_test_group_fun(x, data = afpr[afpr$round %in% 7, ])
  } else {
    ranksum_group_fun(x, data = afpr[afpr$round %in% 7, ])
  }
}) %>%
  do.call("rbind", .) 

means_7 <- map(demographic_groups, ~mean_group_fun(., data = afpr[afpr$round %in% 7, ])) %>%
  do.call("rbind", .)

# Calculate mean differences
means_table_7 <- means_7 %>%
  `colnames<-`(trimws(gsub("\\(|\\)|\\`|\\`|age 35 cutoff", "", colnames(.)))) %>%
  mutate(difference_older = `Both older` - `Interviewer younger`,
         differnece_younger = `Both younger` - `Interviewer older`) %>%
  dplyr::select(Variable, 
                `Both older`, `Interviewer younger`, difference_older,
                `Both younger`, `Interviewer older`, differnece_younger)

# Format means table to have significance stars if p < 0.05
balance_table_7 <- means_table_7 %>%
  left_join(statistical_tests_7) %>%
  mutate_at(vars(-one_of("Variable", "p_older", "p_younger", "difference_older", "differnece_younger")), list(~round(., 3))) %>%
  mutate_at(vars(one_of("difference_older", "differnece_younger")), list(~round(., 2))) %>%
  mutate(difference_older = case_when(p_older >= .05 ~ as.character(difference_older),
                                      p_older < 0.05 ~ paste0(difference_older, "*")),
         differnece_younger = case_when(p_younger >= .05 ~ as.character(differnece_younger),
                                        p_younger < .05 ~ paste0(differnece_younger, "*"))) %>%
  dplyr::select(-p_older, -p_younger)

# Write balance table as CSV to be imported into MICROSOFT WORD ----
write.csv(as.data.frame(balance_table), "tables/balance_table_round_7.csv")