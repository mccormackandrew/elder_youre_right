# DESCRIPTIVE STATISTICS ----

# Load packages
library(tidyverse)
library(showtext)
library(sysfonts)
library(extrafont)
# font_import() run once
loadfonts()

showtext_auto()

# Load data  
afpr <- readRDS("data_clean/afpr_ages.rds")
source("scripts/variable_labels.R")

# Function to convert Stata labelled variables to character
l2f <- function(x) as.character(haven::as_factor(x))

# TABLE 2: Descriptive statistics on the age of interviewers and respondents. ----

# Interviewer ages by round

# q110 is interviewer's number

int_ages <- afpr %>%
  dplyr::select(q110, round, intnum) %>%
  # Filter to include only distinct interviewer, no repeats
  distinct(q110, .keep_all = TRUE) %>%
  dplyr::select(-q110) %>%
  group_by(round) %>%
  summarise_all(list(int_mean = ~mean(., na.rm = T), 
                    int_sd = ~sd(., na.rm = T), 
                    int_n = ~n())) %>%
  ungroup() %>%
  mutate(round = paste0("Round ", round))

# Interviewer ages both rounds

int_ages_all <- afpr %>%
  dplyr::select(intage, q110) %>%
  # Filter to include only distinct interviewer, no repeats
  distinct(q110, .keep_all = TRUE) %>%
  dplyr::select(-q110) %>%
  summarise_all(list(int_mean = ~mean(., na.rm = T), 
                     int_sd = ~sd(., na.rm = T), 
                     int_n = ~n())) %>%
  mutate(round = "All")
 
# Respondent ages by round 

resp_ages <- 
  afpr %>%
  dplyr::select(age, round, age_difference) %>%
  # Can safelyy na_omit at this point
  na.omit() %>%
  mutate(age = as.numeric(l2f(age))) %>%
  group_by(round) %>%
  summarise_all(list(~mean(., na.rm = T), 
                     ~sd(., na.rm = T), 
                     ~n())) %>%
  # Reverse so that table describes respondent age - interviewer age
  mutate(age_difference_mean = age_difference_mean*-1) %>%
  ungroup() %>%
  mutate(round = paste0("Round ", round))

# Respondent ages both rounds

resp_ages_all <- 
  afpr %>%
  dplyr::select(age, age_difference) %>%
  # Can safelyy na_omit at this point
  na.omit() %>%
  mutate(age = as.numeric(l2f(age))) %>%
  summarise_all(list(~mean(., na.rm = T), 
                     ~sd(., na.rm = T), 
                     ~n())) %>%
  # Reverse so that table describes respondent age - interviewer age
  mutate(age_difference_mean = age_difference_mean*-1) %>%
  mutate(round = "All")


ages_df <- cbind(bind_rows(resp_ages, resp_ages_all),
      bind_rows(int_ages, int_ages_all)[ , -1]) %>%
  mutate_if(is.numeric, list(~round(., digits = 2))) %>%
  dplyr::select(round, 
                age_mean, age_sd, age_n, 
                int_mean, int_sd, int_n,
                age_difference_mean, age_difference_sd, age_difference_n)

write.csv(ages_df, "tables/age_difference_distributions.csv")

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
  mutate(round = factor(round, levels = c("Round 3", "Round 4", "All")))

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
  ggplot(aes(coarsened_age_35, percent)) +
  theme_linedraw() +
  geom_col() +
  facet_wrap(~round,ncol = 4) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()) +
  geom_text(aes(coarsened_age_35, percent + 1.5, label = paste0(percent, "%")), hjust = 0, size = 2.75) +
  #ggtitle("Distribution of age differences") +
  ylim(0, 100) +
  coord_flip() +
  theme(text = element_text(family = "Century Gothic")) +
  ggsave("figs/descriptives_age_coarsened_35_plot.png", width = 8, height = 2)


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

# Determine whether variables are binary or ordinal
variable_type <- afpr[ , all_outcomes] %>%
  sapply(function(x) length(na.omit(unique(x))))

variable_type <- ifelse(variable_type == 2, "binary", "ordinal")

# Get p-value for difference of means
older_younger_p <- map2(names(variable_type), variable_type, function(x, y) {
  
  if(y == "binary") { # If binary, do a wilcoxon rank-sum test
    pval <- do(afpr, broom::tidy(wilcox.test(!!sym(x) ~ younger_older, data = .)))$p.value
  } else { # If ordinal, do a t.test
    pval <- do(afpr, broom::tidy(t.test(!!sym(x) ~ younger_older, data = .)))$p.value
  }
  data.frame(variable = x, pval = pval)
}) %>%
  bind_rows() 

older_younger_desc <- map(names(variable_type), function(x) {
  means <- afpr %>%
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
  left_join(older_younger_p, by = "variable") %>%
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
older_younger_desc$variable <- fct_rev(factor(older_younger_desc$variable, variable_labels$label))

older_younger_desc <- older_younger_desc %>%
  arrange(variable)

map(unique(variable_labels$group), function(x) {
  older_younger_desc %>%
    filter(group == x) %>%
    dplyr::select(-group) %>%
    write.csv(., file = paste0("tables/older_younger_means_", x, ".csv"))
})
