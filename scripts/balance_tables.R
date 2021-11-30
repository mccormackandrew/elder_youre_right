# Balance on demographic variables (Rounds 3/4 and Round 7) ----

# Load packages
library(tidyverse)
library(xtable)
library(gridExtra)
library(fastDummies)
library(devtools)
library(TOSTER)

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


# %%%% BALANCE TABLES %%%% ----

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
write.csv(as.data.frame(balance_table_7), "tables/balance_table_round_7.csv")

# Balance table for Mauritius Round 7 ----

statistical_tests_7_mauritius <- map(demographic_groups, function(x) {
  if(x == "age") {
    t_test_group_fun(x, data = afpr[afpr$round %in% 7 & afpr$country == "Mauritius", ])
  } else {
    ranksum_group_fun(x, data = afpr[afpr$round %in% 7 & afpr$country == "Mauritius", ])
  }
}) %>%
  do.call("rbind", .) 

means_7_mauritius <- map(demographic_groups, ~mean_group_fun(., data = afpr[afpr$round %in% 7 & afpr$country == "Mauritius", ])) %>%
  do.call("rbind", .)

# Calculate mean differences
means_table_7_mauritius <- means_7_mauritius %>%
  `colnames<-`(trimws(gsub("\\(|\\)|\\`|\\`|age 35 cutoff", "", colnames(.)))) %>%
  mutate(difference_older = `Both older` - `Interviewer younger`,
         differnece_younger = `Both younger` - `Interviewer older`) %>%
  dplyr::select(Variable, 
                `Both older`, `Interviewer younger`, difference_older,
                `Both younger`, `Interviewer older`, differnece_younger)

# Format means table to have significance stars if p < 0.05
balance_table_7_mauritius <- means_table_7_mauritius %>%
  left_join(statistical_tests_7_mauritius) %>%
  mutate_at(vars(-one_of("Variable", "p_older", "p_younger", "difference_older", "differnece_younger")), list(~round(., 3))) %>%
  mutate_at(vars(one_of("difference_older", "differnece_younger")), list(~round(., 2))) %>%
  mutate(difference_older = case_when(p_older >= .05 ~ as.character(difference_older),
                                      p_older < 0.05 ~ paste0(difference_older, "*")),
         differnece_younger = case_when(p_younger >= .05 ~ as.character(differnece_younger),
                                        p_younger < .05 ~ paste0(differnece_younger, "*"))) %>%
  dplyr::select(-p_older, -p_younger)

# Write balance table as CSV to be imported into MICROSOFT WORD ----
write.csv(as.data.frame(balance_table_7_mauritius), "tables/balance_table_round_7_mauritius.csv")



# %%%% EQUIVALENCE TESTS %%%% ----

equivalence_intervals <- function(data, var_name, level = 1,
                                  group = "coarsened_age_35",
                                  alpha = 0.05,
                                  lower_equivalence_bound = -0.25,
                                  upper_equivalence_bound = 0.25,
                                  type = "prop") {
  
  if(type == "t") { # Relevant only for age
    
    tost_est <- dataTOSTtwo(data, var_name, group, alpha = alpha,
                            low_eqbound = lower_equivalence_bound, 
                            high_eqbound = upper_equivalence_bound)
    std_dev <- sd(data[, var_name], na.rm = TRUE)
    est <- as.numeric(diff(t(tost_est$desc$asDF[, c("m[2]", "m[1]")])))
    lower <- as.numeric(tost_est$eqb$asDF["cil[raw]"])
    upper <- as.numeric(tost_est$eqb$asDF["ciu[raw]"])
    p_upper <- as.numeric(tost_est$tost$asDF["p[1]"])
    p_lower <- as.numeric(tost_est$tost$asDF["p[2]"])
    
    } else {
      
      tost_est <- datatosttwoprop(data, var_name, level, group, alpha, 
                                  low_eqbound = lower_equivalence_bound, 
                                  high_eqbound = upper_equivalence_bound)
      std_dev <- sd(data[, var_name] == level, na.rm = TRUE)
      est <- as.numeric(diff(t(tost_est$desc$asDF[, c("prop[2]", "prop[1]")])))
      lower <- as.numeric(tost_est$eqb$asDF["cil"])
      upper <- as.numeric(tost_est$eqb$asDF["ciu"])
      p_upper <- as.numeric(tost_est$tost$asDF["p[1]"])
      p_lower <- as.numeric(tost_est$tost$asDF["p[2]"])
    
    }
  
  out <- data.frame(est = est, lower = lower, upper = upper,
                    std_dev = std_dev, est_sd = est / std_dev,
                    lower_sd = lower / std_dev, upper_sd = upper / std_dev,
                    p_upper = p_upper, p_lower = p_lower)
  row.names(out) <- 1:nrow(out)
  
  return(out)
  
}




equivalence_tests <- expand_grid(demo_var = demographic_groups,
            age_group = c("younger", "older")) %>%
  pmap_dfr(function(demo_var, age_group) {
    
    if(demo_var == "age") type <- "t" else type <- "prop"
    
    # Filter the data by young and old to assess age-diff 
    # balance within the two groups
    if(age_group == "older") { # Older respondents subset
      afpr <- filter(afpr, coarsened_age_35 %in% 
               c("Both older (age 35 cutoff)", 
                 "Interviewer younger (age 35 cutoff)"))
    } else { # Younger respondents subset
      afpr <- filter(afpr, coarsened_age_35 %in% 
                       c("Both younger (age 35 cutoff)", 
                         "Interviewer older (age 35 cutoff)"))
    }
    
    # Estimate equivalence interval
    equivalence_intervals(data = afpr, 
                          var_name = demo_var,
                          group = "coarsened_age_35",
                          lower_equivalence_bound = -0.25,
                          upper_equivalence_bound = 0.25,
                          alpha = 0.05,
                          type = type) %>%
      mutate(name = demo_var,
             title = paste0(str_to_sentence(age_group),
                            " respondents"))
  })
            
ggplot(equivalence_tests, aes(x = est_sd, y = name)) +
  coord_cartesian(xlim = c(-0.36, 0.36)) +
  scale_x_continuous(breaks = c(-0.36, -0.3, -0.25, -0.2, -0.1, 0,
                                0.1, 0.2, 0.25, 0.3, 0.36)) +
  labs(x = "Equivalence range (in std. dev.)", y = "") +
  geom_vline(xintercept = c(-0.36, -0.25, 0.25, 0.36), 
             linetype = 3, size = 0.5) +
  geom_vline(xintercept = seq(-0.3, 0.3, 0.1),
             linetype = 1, size = 0.5,
             colour = "grey90") +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.6) +
  geom_segment(aes(x = lower_sd, xend = upper_sd, yend = name),
               size = 4, color = "grey70") +
  geom_point() +
  facet_wrap(~title) +
  theme_linedraw() +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90,
                                   hjust = 1, vjust = 0.5),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        legend.key.width=unit(3,"line"),
        text = element_text(family = "Roboto")) 


ggsave("figs/equivalence_test.png", width = 7, height = 3)
