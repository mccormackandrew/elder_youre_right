# DESCRIPTIVE STATISTICS ----

# Load packages
library(kableExtra)
library(tidyverse)
library(showtext)
library(sysfonts)
library(ggtext)

# Load data  
afpr <- readRDS("data_clean/afpr_ages.rds")
source("scripts/variable_labels.R")


# Function to convert Stata labelled variables to character
l2f <- function(x) as.character(haven::as_factor(x))

# TABLE 2: Descriptive statistics on the age of interviewers and respondents. ----

ages_df <- map_dfr(c("All", 3, 4, 7), function(x) {
  
  if(x != "All") {
    afpr <- afpr %>%
      filter(round == x)
    round_label = paste0("Round ", x)
  } else {
    round_label = x
  }
  
  # Interviewer ages
  int_ages <- afpr %>%
    dplyr::select(int_id, intage) %>%
    # Filter to include only distinct interviewer, no repeats
    distinct(int_id, .keep_all = TRUE) %>%
    dplyr::select(-int_id) %>%
    summarise_all(list(int_mean = ~mean(., na.rm = T), 
                       int_sd = ~sd(., na.rm = T), 
                       int_n = ~n())) %>%
    mutate(round = round_label)
  
  # Respondent ages and age differences
  resp_ages <- afpr %>%
    dplyr::select(age, age_difference) %>%
    # Can safelyy na_omit at this point
    na.omit() %>%
    summarise(across(everything(), 
                     list(mean = ~mean(., na.rm = T), 
                          sd = ~sd(., na.rm = T), 
                          n = ~n()),
                     .names = "{.col}_{.fn}")) %>%
    # Reverse so that table describes respondent age - interviewer age
    mutate(age_difference_mean = age_difference_mean*-1) %>%
    mutate(round = round_label)
  
  left_join(resp_ages, int_ages)
  
})

ages_df <- ages_df %>%
  mutate(round = fct_relevel(round, "Round 3", "Round 4", "Round 7", "All")) %>%
  arrange(round) %>%
  mutate(across(c(ends_with("_sd"), ends_with("_mean")),
                ~round(., 2))) %>%
  mutate(across(ends_with("_n"),
                ~prettyNum(., big.mark = ","))) %>%
  dplyr::select(round, 
                age_mean, age_sd, age_n, 
                int_mean, int_sd, int_n,
                age_difference_mean, age_difference_sd, age_difference_n)

write.csv(ages_df, "tables/age_difference_distributions.csv")

# kable(ages_df, format = "latex", booktabs = TRUE, 
#       col.names = c("AB round", 
#                     "Mean", "SD", "N",
#                     "Mean", "SD", "N",
#                     "Mean", "SD", "N")) %>%
#   add_header_above(c(" ", 
#                      "Respondent age" = 3, 
#                      "Interviewer age" = 3,
#                      "Age difference" = 3)) %>%
#   row_spec(3, extra_latex_after = "\\cline{2-10} \\\\[-5pt]") %>%
#   writeLines("tables/age_difference_distributions.tex")
  



# Figure 1: Descriptive plots of our four age group categories. ----

coarsened_35_desc_rounds <- afpr %>%
  dplyr::select(coarsened_age_35, round) %>%
  mutate(round = paste0("Round ", round))

coarsened_35_desc <- afpr %>%
  dplyr::select(coarsened_age_35) %>% 
  mutate(round = "All") %>%
  rbind(coarsened_35_desc_rounds)

coarsened_35_desc <- coarsened_35_desc %>%
  group_by(round) %>%
  count(coarsened_age_35) %>%
  na.omit() %>%
  mutate(percent = round((n/sum(n)*100), 2)) %>%
  ungroup() %>%
  mutate(round = factor(round, levels = c("Round 3", "Round 4", "Round 7", "All")))

# Remove "age 35 cutoff" from variable names
coarsened_35_desc$coarsened_age_35 <- gsub(" \\(age 35 cutoff)", "", coarsened_35_desc$coarsened_age_35)

# Rename coarsened age variable values 
coarsened_35_desc <- coarsened_35_desc %>%
  mutate(coarsened_age_35 = case_when(coarsened_age_35 == "Interviewer younger" ~ "Interviewer 35 and younger, Respondent over 35",
                                      coarsened_age_35 == "Interviewer older" ~ "Interviewer over 35, Respondent 35 and younger",
                                      coarsened_age_35 == "Both younger" ~ "Both 35 and under",
                                      coarsened_age_35 == "Both older" ~ "Both over 35",
                                      TRUE ~ coarsened_age_35))


coarsened_35_desc %>%
  mutate(coarsened_age_35 = str_wrap(coarsened_age_35, 30)) %>%
  ggplot(aes(coarsened_age_35, percent)) +
  theme_linedraw() +
  geom_col() +
  facet_wrap(~round,ncol = 4) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()) +
  geom_richtext(aes(coarsened_age_35, percent + 1.5, 
                label = paste0("**", percent, "%<br>", "**(N = ", n, ")")), 
                fill = NA, label.color = NA, # remove background and outline
                label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                hjust = 0, size = 3) +
  #ggtitle("Distribution of age differences") +
  ylim(0, 120) +
  coord_flip() +
  ggsave("figs/descriptives_age_coarsened_35_plot.png", width = 8, height = 2)

# Figure XX: Descriptive plots of our four age group categories for Uganda and Mauritius ----

coarsened_35_round_7 <- 
  map_dfr(c("Mauritius", "All Round 7"), function(x) {
    
    if(x != "All Round 7") {
      afpr <- afpr %>%
        filter(country == x)
    }
    
    afpr %>%
      count(coarsened_age_35) %>%
      na.omit() %>%
      mutate(percent = round((n/sum(n)*100), 2)) %>%
      ungroup() %>%
      mutate(country = x)
  })

# Remove "age 35 cutoff" from variable names
coarsened_35_round_7$coarsened_age_35 <- gsub(" \\(age 35 cutoff)", "", coarsened_35_round_7$coarsened_age_35)

# Rename coarsened age variable values 
coarsened_35_round_7 <- coarsened_35_round_7 %>%
  mutate(coarsened_age_35 = case_when(coarsened_age_35 == "Interviewer younger" ~ "Interviewer 35 and younger, Respondent over 35",
                                      coarsened_age_35 == "Interviewer older" ~ "Interviewer over 35, Respondent 35 and younger",
                                      coarsened_age_35 == "Both younger" ~ "Both 35 and under",
                                      coarsened_age_35 == "Both older" ~ "Both over 35",
                                      TRUE ~ coarsened_age_35))


coarsened_35_round_7 %>%
  mutate(coarsened_age_35 = str_wrap(coarsened_age_35, 30)) %>%
  ggplot(aes(coarsened_age_35, percent)) +
  theme_linedraw() +
  geom_col() +
  facet_wrap(~country,ncol = 4) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()) +
  geom_richtext(aes(coarsened_age_35, percent + 1.5, 
                    label = paste0("**", percent, "%<br>", "**(N = ", n, ")")), 
                fill = NA, label.color = NA, # remove background and outline
                label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                hjust = 0, size = 3) +
  #ggtitle("Distribution of age differences") +
  ylim(0, 120) +
  coord_flip() +
  ggsave("figs/descriptives_age_coarsened_35_plot_round_7.png", width = 8, height = 2)


# Tables 4 and 5: Mean differences between younger and older respondents for outcomes ----

################################
## AFROBAROMETER DESCRIPTIVES ##
################################

afpr <- afpr %>%
  mutate(younger_older = ifelse(age <= 35, "Younger than 35", "Older than 35")) %>%
  mutate(younger_older = fct_relevel(younger_older, "Younger than 35"))

conditional_rescale <- function(x) {
  if(min(as.numeric(x), na.rm = T) == 1) {
    x <- as.numeric(x) - 1
  }
  return(x)
}

afpr <- afpr %>%
  mutate_at(vars(one_of(all_outcomes)),
            conditional_rescale)

# ROUND 3 and 4

# Determine whether variables are binary or ordinal
variable_type_3_4 <- afpr[ , c(pro_outcomes, pol_outcomes, stat_outcomes, eth_outcomes)] %>%
  sapply(function(x) length(na.omit(unique(x))))

variable_type_3_4 <- ifelse(variable_type_3_4 == 2, "binary", "ordinal")

# Get p-value for difference of means
older_younger_p_3_4 <- map2(names(variable_type_3_4), variable_type_3_4, function(x, y) {
  
  if(y == "binary") { # If binary, do a wilcoxon rank-sum test
    pval <- do(afpr[afpr$round %in% 3:4, ], 
               broom::tidy(wilcox.test(!!sym(x) ~ younger_older, data = .)))$p.value
  } else { # If ordinal, do a t.test
    pval <- do(afpr[afpr$round %in% 3:4, ], 
               broom::tidy(t.test(!!sym(x) ~ younger_older, data = .)))$p.value
  }
  data.frame(variable = x, pval = pval)
}) %>%
  bind_rows() 

# Construct table 
older_younger_desc_3_4 <- map(names(variable_type_3_4), function(x) {
  means <- afpr[afpr$round %in% 3:4, ] %>%
    dplyr::select(younger_older, one_of(x)) %>%
    na.omit() %>%
    group_by(younger_older) %>%
    dplyr::summarise(means = mean(!!sym(x), na.rm = TRUE)) %>%
    spread(younger_older, means) %>%
    mutate(difference = `Younger than 35` - `Older than 35`) %>%
    mutate(variable = x) 
  
  means$min <- range(afpr[[x]], na.rm = TRUE)[1]
  means$max <- range(afpr[[x]], na.rm = TRUE)[2]
  
  return(means)
}) %>%
  bind_rows() %>%
  dplyr::select(variable, everything()) %>%
  # Join p-values from mean difference tests
  left_join(older_younger_p_3_4, by = "variable") %>%
  # Clean up variable labels
  mutate(variable = plyr::mapvalues(variable, variable_labels$var, 
                                    as.character(variable_labels$label))) %>%
  # Add in variable grouping category
  mutate(group = plyr::mapvalues(variable, variable_labels$label, variable_labels$group)) %>%
  # Round numbers
  mutate_if(is.numeric, list(~round(., 3))) %>%
  # Append a star * to the difference if it's significant
  mutate(difference = ifelse(pval < 0.05, paste0(difference, "*"), difference)) %>%
  # Remove p-value column
  dplyr::select(-pval)

# Make ordering of variables correct
older_younger_desc_3_4$variable <- fct_rev(factor(older_younger_desc_3_4$variable, variable_labels$label))

older_younger_desc_3_4 <- older_younger_desc_3_4 %>%
  arrange(variable)

map(c("stat_outcomes","pol_outcomes","eth_outcomes","pro_outcomes"), function(x) {
  older_younger_desc_3_4 %>%
    filter(group == x) %>%
    dplyr::select(-group) %>%
    write.csv(., file = paste0("tables/older_younger_means_3_4", x, ".csv"))
})

# ROUND 7 ====

# Determine whether variables are binary or ordinal
variable_type_7 <- afpr[ , youth_outcomes] %>%
  sapply(function(x) length(na.omit(unique(x))))

variable_type_7 <- ifelse(variable_type_7 == 2, "binary", "ordinal")

# Get p-value for difference of means
older_younger_p_7 <- map2(names(variable_type_7), variable_type_7, function(x, y) {
  
  if(y == "binary") { # If binary, do a wilcoxon rank-sum test
    pval <- do(afpr[afpr$round %in% 7, ], 
               broom::tidy(wilcox.test(!!sym(x) ~ younger_older, data = .)))$p.value
  } else { # If ordinal, do a t.test
    pval <- do(afpr[afpr$round %in% 7, ], 
               broom::tidy(t.test(!!sym(x) ~ younger_older, data = .)))$p.value
  }
  data.frame(variable = x, pval = pval)
}) %>%
  bind_rows() 

# Construct table 
older_younger_desc_7 <- map(names(variable_type_7), function(x) {
  means <- afpr[afpr$round %in% 7, ] %>%
    dplyr::select(younger_older, one_of(x)) %>%
    na.omit() %>%
    group_by(younger_older) %>%
    dplyr::summarise(means = mean(!!sym(x), na.rm = TRUE)) %>%
    spread(younger_older, means) %>%
    mutate(difference = `Younger than 35` - `Older than 35`) %>%
    mutate(variable = x) 
  
  means$min <- range(afpr[[x]], na.rm = TRUE)[1]
  means$max <- range(afpr[[x]], na.rm = TRUE)[2]
  
  return(means)
}) %>%
  bind_rows() %>%
  dplyr::select(variable, everything()) %>%
  # Join p-values from mean difference tests
  left_join(older_younger_p_7, by = "variable") %>%
  # Clean up variable labels
  mutate(variable = plyr::mapvalues(variable, variable_labels$var, 
                                    as.character(variable_labels$label))) %>%
  # Add in variable grouping category
  mutate(group = plyr::mapvalues(variable, variable_labels$label, variable_labels$group)) %>%
  # Round numbers
  mutate_if(is.numeric, list(~round(., 3))) %>%
  # Append a star * to the difference if it's significant
  mutate(difference = ifelse(pval < 0.05, paste0(difference, "*"), difference)) %>%
  # Remove p-value column
  dplyr::select(-pval)

# Make ordering of variables correct
older_younger_desc_7$variable <- fct_rev(factor(older_younger_desc_7$variable, variable_labels$label))

older_younger_desc_7 <- older_younger_desc_7 %>%
  arrange(variable)

older_younger_desc_7 %>%
  filter(group == "youth_outcomes") %>%
  dplyr::select(-group) %>%
  write.csv(., file = paste0("tables/older_younger_means_7_youth_outcomes.csv"))


# ROUND 7 (UGANDA AND MAURITIUS) ====

# Get p-value for difference of means

uganda_mauritius <-
  expand.grid(outcome = youth_outcomes,
              country = c("Uganda", "Mauritius"),
              stringsAsFactors = FALSE)

# # Not all variables are in both countries, so filter out models that are not possible
filter_uganda_mauritius_grid <- pmap_lgl(uganda_mauritius, function(outcome, country) {
  
  country_round_df <- afpr[afpr$round == 7 & afpr$country %in% country, ]

  na_rows <- country_round_df %>%
    filter(is.na(!!sym(outcome))) %>%
    nrow()

  !(na_rows == nrow(country_round_df))

})

uganda_mauritius <- uganda_mauritius[filter_uganda_mauritius_grid, ]

uganda_mauritius_descriptives <- pmap_dfr(uganda_mauritius, function(outcome, country) {
  
  afpr <- afpr[afpr$round %in% 7 & afpr$country == country, ]
  
  # Get significance
  pval <- do(afpr, 
             broom::tidy(t.test(!!sym(outcome) ~ younger_older, data = .)))$p.value
    
  pval <- data.frame(variable = outcome, pval = pval)
  
  # Construct table 
  means <- afpr %>%
      dplyr::select(younger_older, one_of(outcome)) %>%
      na.omit() %>%
      group_by(younger_older) %>%
      dplyr::summarise(means = mean(!!sym(outcome), na.rm = TRUE)) %>%
      spread(younger_older, means) %>%
      mutate(difference = `Younger than 35` - `Older than 35`) %>%
      mutate(variable = outcome) 
  
  means$min <- range(afpr[[outcome]], na.rm = TRUE)[1]
  means$max <- range(afpr[[outcome]], na.rm = TRUE)[2]
  
  means %>%
    mutate(country = country) %>%
    select(country, variable, everything()) %>%
    left_join(pval, by = "variable") 
}) %>%
  mutate(variable = plyr::mapvalues(variable, variable_labels$var, 
                                    as.character(variable_labels$label))) %>%
  # Add in variable grouping category
  mutate(group = plyr::mapvalues(variable, variable_labels$label, variable_labels$group)) %>%
  # Round numbers
  mutate_if(is.numeric, list(~round(., 3))) %>%
  # Append a star * to the difference if it's significant
  mutate(difference = ifelse(pval < 0.05, paste0(difference, "*"), difference)) %>%
  # Remove p-value column
  dplyr::select(-pval)


# Make ordering of variables correct
uganda_mauritius_descriptives$variable <- fct_rev(factor(uganda_mauritius_descriptives$variable, variable_labels$label))

older_younger_desc_7 <- older_younger_desc_7 %>%
  arrange(variable)

uganda_mauritius_descriptives %>%
    write.csv(., file = paste0("tables/older_younger_means_7_youth_outcomes_uganda_maurituis.csv"))


            
            
            
            