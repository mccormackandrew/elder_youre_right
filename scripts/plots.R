# PLOTS FROM PAPER ----

library(cowplot)
library(tidyverse)
library(showtext)
library(sysfonts)


font_add_google("Roboto", "Roboto")
showtext_auto()
# library(extrafont)
# 
# #extrafont::font_import() # Run once
# loadfonts()
# Read in data ----

source("scripts/variable_labels.R")
age_diff_models <- readRDS("data_clean/age_diff_models.rds")

# Clean up coefficient estimates df ----

age_diff_models <- age_diff_models %>%
  filter(!(term %in% c("coarsened_age_35", "coarsened_age_40"))) %>%
  # Clean up coefficient estimate names
  mutate(term = 
           case_when(grepl("Interviewer younger", term) ~ 
                       "Interviewer under 10 years younger than respondent\n(relative to interviewer's age within 10 years of respondent)",
                     grepl("Interviewer older", term) ~
                       "Interviewer over 10 years older then respondents\n(relative to interviewer's age within 10 years of respondent)",
                     grepl("35.*older_int", term) ~
                       "Interviewer over 35 (relative to both 35 and under)",
                     grepl("35.*younger_int", term) ~
                       "Interviewer 35 and under (relative to both over 35)",
                     grepl("40.*older_int", term) ~
                       "Interviewer over 40 (relative to both 40 and under)",
                     grepl("40.*younger_int", term) ~
                       "Interviewer 40 and under (relative to both over 40)",
                     grepl("40.*younger_int", term) ~
                       "Interviewer 40 and under (relative to both over 40)",
                     grepl("noncoeth", term) ~
                       "Non-coethnic interviewer",
         TRUE ~ term))
                     
# PLOT FUNCTION ----
# This function plots coefficients from models 
# created in contrasts.Rmd along with error bars
plotfun <- function(data) {
  data %>%
    ggplot(aes(label, 
               estimate, 
               colour = term,
               linetype = term,
               shape = term)) +
    theme_linedraw() +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  position = position_dodge(width = 0.5),
                  width = 0.3) +
    scale_colour_manual(values = c("black", "gray50", "gray80")) +
    scale_shape_manual(values = c(17,15,19)) +
    scale_linetype_manual(values = c(rep("solid", 4))) +
    coord_flip() +
    scale_y_continuous(breaks = seq(-0.2, 0.2, 0.05),
                       limits = c(-0.2, 0.2)) +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 7),
          legend.key.width=unit(3,"line"),
          strip.background = element_blank(),
          strip.text = element_blank(),
          text = element_text(family = "Roboto")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    guides(colour = guide_legend(reverse = TRUE,
                                 ncol = 1),
           linetype = guide_legend(reverse = TRUE,
                                   ncol = 1),
           shape = guide_legend(reverse = TRUE,
                                ncol = 1)) +
    ylab("\nEstimated effect (in SDs) of non-coethnic interviewer \nand age difference, with 95% confidence intervals")
}



# 10 year age difference plots ----


  
plot10yr <- lapply(unique(age_diff_models$group), function(x) {
  plot <- age_diff_models %>%
    filter(age_variable == "coarsened_age_10") %>%
    filter(group == x) %>%
    plotfun()
  
  if(x == "pol_outcomes") {
    plot <- plot +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1),
                         limits = c(-0.4, 0.4)) +
      geom_vline(xintercept = 2.5, size = 0.3) +
      geom_vline(xintercept = 4.5, size = 0.3) 
  } else if (x == "stat_outcomes") {
    plot <- plot +
      geom_vline(xintercept = 1.5, size = 0.3) 
  } else if (x == "pro_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.2, 0.2)) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.05))
  } else if (x == "youth_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.2, 0.2)) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1))
  }
  return(plot)
}) %>%
  "names<-"(unique(age_diff_models$group))

### Align x-axes of plots
## This constrains the plot rectangles to all be 
## the same size and makes it easier to compare 
## effect sizes. Without this, varying variable 
## label length makes all the plots different sizes

plot10yr_align <- align_plots(plotlist = plot10yr, 
                              align = "hv",
                              axis = "tblr") %>%
  lapply(ggdraw) 

map2(names(plot10yr_align), c(5, 8, 5, 3.5, 5), function(x, y) {
  save_plot(paste0("figs/", x, "10yr.png"),
            plot10yr_align[[x]],
            base_width = 7,
            base_height = y)
})
  
    
# 35 year age difference plots ----

plot35yr <- lapply(unique(age_diff_models$group), function(x) {
  plot <- age_diff_models %>%
    filter(age_variable == "coarsened_age_35") %>%
    filter(group == x) %>%
    plotfun()
  
  if(x == "pol_outcomes") {
    plot <- plot +
      geom_vline(xintercept = 2.5, size = 0.3) +
      geom_vline(xintercept = 4.5, size = 0.3)  
  } else if (x == "stat_outcomes") {
    plot <- plot +
      geom_vline(xintercept = 1.5, size = 0.3) 
  } else if (x == "pro_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.2, 0.2)) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.05))
  } else if (x == "youth_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.4, 0.4)) +
      scale_y_continuous(breaks = seq(-0.8, 0.8, 0.1))
  }
  return(plot)
}) %>%
  "names<-"(unique(age_diff_models$group))

plot35yr_align <- align_plots(plotlist = plot35yr, 
                              align = "hv",
                              axis = "tblr") %>%
  lapply(ggdraw) 

map2(names(plot35yr_align), c(5, 8, 5, 3.5, 5), function(x, y) {
  save_plot(paste0("figs/", x, "35yr.png"),
            plot35yr_align[[x]],
            base_width = 7,
            base_height = y)
})
 
# 40 year age difference plots ----

plot40yr <- lapply(unique(age_diff_models$group), function(x) {
  plot <- age_diff_models %>%
    filter(age_variable == "coarsened_age_40") %>%
    filter(group == x) %>%
    plotfun()
  
  if(x == "pol_outcomes") {
    plot <- plot +
      geom_vline(xintercept = 2.5, size = 0.3) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.05)) +
      geom_vline(xintercept = 4.5, size = 0.3)  
  } else if (x == "stat_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.2, 0.2)) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.05)) +
      geom_vline(xintercept = 1.5, size = 0.3) 
  } else if (x == "pro_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.2, 0.2)) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.05))
  }
  return(plot)
}) %>%
  "names<-"(unique(age_diff_models$group))

plot40yr_align <- align_plots(plotlist = plot40yr, 
                              align = "hv",
                              axis = "tblr") %>%
  lapply(ggdraw) 

map2(names(plot40yr_align), c(5, 8, 5, 3.5, 5), function(x, y) {
  save_plot(paste0("figs/", x, "40yr.png"),
            plot40yr_align[[x]],
            base_width = 7,
            base_height = y)
})

# 35 year age difference results (original scale) table ----

age_diff_models %>%
  filter(age_variable == "coarsened_age_35_originalscale") %>%
  dplyr::select(term, label, estimate, std.error, upper, lower) %>%
  mutate_if(is.numeric, list(~round(., 4))) %>%
  write.csv("tables/effects_original_scales.csv")


table(afpr$round)

