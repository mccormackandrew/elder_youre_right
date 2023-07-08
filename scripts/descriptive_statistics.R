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

# TABLE XX: Descriptive statistics on the age of interviewers and respondents. ----

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

# Figure XX: Descriptive plots of our four age group categories. ----

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
  coord_flip() 

ggsave("figs/descriptives_age_coarsened_35_plot.png", width = 8, height = 2)

# Figure XX: Descriptive plots of our four age group categories Round 7 and Mauritius ----

# Mauritius is only in Round 7, so we can filter all the results to include only round 7
stopifnot(setequal(7, unique(afpr$round[afpr$country == "Mauritius"])))

coarsened_35_round_7 <- map_dfr(c("Mauritius", "All Round 7"), function(x) {
    
    if(x == "Mauritius") {
      afpr <- afpr %>%
        filter(country == "Mauritius")
    }
    
    afpr %>%
      filter(round == 7) %>%
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
  coord_flip() 

ggsave("figs/descriptives_age_coarsened_35_plot_round_7.png", width = 8, height = 2)


# Tables XX: Mean differences between younger and older respondents (Rounds 3 and 4) ----

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

# Tables XX: Mean differences between younger and older respondents (All round 7) ----
# It is just "youth_needs" that is in all Round 7


pval <- do(afpr[afpr$round %in% 7, ], 
               broom::tidy(t.test(youth_needs ~ younger_older, data = .)))$p.value

older_younger_p_7 <- data.frame(variable = "youth_needs", pval = pval)

# Construct table 
older_younger_desc_7 <- 
  means <- afpr[afpr$round %in% 7, ] %>%
  dplyr::select(younger_older, youth_needs) %>%
  na.omit() %>%
  group_by(younger_older) %>%
  dplyr::summarise(means = mean(youth_needs, na.rm = TRUE)) %>%
  spread(younger_older, means) %>%
  mutate(difference = `Younger than 35` - `Older than 35`) %>%
  mutate(variable = "youth_needs") %>%
  select(variable, everything())
  
older_younger_desc_7$min <- range(afpr[["youth_needs"]], na.rm = TRUE)[1]
older_younger_desc_7$max <- range(afpr[["youth_needs"]], na.rm = TRUE)[2]
  
  
older_younger_desc_7 <- older_younger_desc_7 %>%
  # Join p-values from mean difference tests
  left_join(older_younger_p_7, by = "variable") %>%
  # Clean up variable labels
  mutate(variable = plyr::mapvalues(variable, variable_labels$var, 
                                    as.character(variable_labels$label))) %>%
  # Round numbers
  mutate_if(is.numeric, list(~round(., 3))) %>%
  # Append a star * to the difference if it's significant
  mutate(difference = ifelse(pval < 0.05, paste0(difference, "*"), difference)) %>%
  # Remove p-value column
  dplyr::select(-pval)

older_younger_desc_7 %>%
  write.csv(., file = paste0("tables/older_younger_means_7_youth_outcomes.csv"))


# Tables XX: Mean differences between younger and older respondents (Mauritius only) ----

# Get p-value for difference of means
mauritius_descriptives <- map_dfr(youth_outcomes, function(outcome) {
  
  afpr <- afpr[afpr$round %in% 7 & afpr$country == "Mauritius", ]
  
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
    mutate(country = "Mauritius") %>%
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
mauritius_descriptives$variable <- fct_rev(factor(mauritius_descriptives$variable, variable_labels$label))

mauritius_descriptives <- mauritius_descriptives %>%
  arrange(variable)

uganda_mauritius_descriptives %>%
    write.csv(., file = paste0("tables/older_younger_means_7_youth_outcomes_maurituis.csv"))

# Distribution of interviewer-respondent dyads by country and wave ----

# Explanatory variables used in the models. If any of have NA values, those
# respondents will not be used in the analysis

model_vars <- c("coarsened_age_35", "noncoeth", "age", "gender", "edu", 
  "urban", "minority", "round", "inhomelang", 
  "region", "tribe", "enumeth")


afpr$omit_respondent <- afpr %>%
  select(one_of(model_vars)) %>%
  apply(1, function(x) any(is.na(x)))


afpr %>%
  filter(country == "Mozambique") %>%
  select(one_of(model_vars))

age_differnce_count <- afpr %>%
  group_by(round, country) %>%
  count(coarsened_age_35) %>%
  filter(!is.na(coarsened_age_35)) %>%
  pivot_wider(names_from = coarsened_age_35, values_from = n) %>%
  ungroup() %>%
  # NA values mean there are zero respondents in that category
  mutate(across(-one_of(c("country", "round")), ~ifelse(is.na(.), 0, .))) %>%
  arrange(round, country)

total_n <- afpr %>%
  group_by(round, country) %>%
  count() %>%
  rename(total_n = n)

n_used <- afpr %>%
  filter(!omit_respondent) %>%
  group_by(round, country) %>%
  count() %>%
  rename(n_used = n)
  
  
age_differnce_count %>%
  left_join(total_n) %>%
  left_join(n_used) %>%
  split(.$round) %>%
  map(function(x) {
    
    total_row <- colSums(select(x, -country, -round), na.rm = TRUE) %>%
      stack() %>%
      spread(ind, values) %>%
      mutate(country = "Total", round = unique(x$round))
    
    x <- bind_rows(x, total_row)
    
    write_csv(x, paste0("tables/respondent_interviewer_dyad_counts_round", unique(x$round), ".csv"))
    
    })




  
