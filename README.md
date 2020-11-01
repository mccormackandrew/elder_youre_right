# "Elder you're right!: Age Group Differences in Social and Political Interactions in Africa" replication materials

## Abstract

Differences in age play an important role in social interaction across the African continent. However, these differences remain understudied. Using Afrobarometer data, we investigate how age differences between interviewers and respondents may shape how respondents answer questions across Africa. We further explore three mechanisms through which age differences may induce response pattern variation. The first is *social acquiescence*?where younger respondents say what they think the socially dominant older interviewer wants to hear because they are socially inferior, but the socially dominant group does not change their response pattern. The second is *in-group esteem*?where both younger and older respondents are more likely to present themselves in ways that reinforce their social standing when talking to interviewers of different ages. The third is *social distance*, where all respondents, regardless of social status say what they impute the interviewer wants to hear, which we assume will vary by age group.  We find relatively large and statistically significant effects for age differences across a variety of questions. While more research remains to be done, we believe these finding generally support *social acquiescence* and *in-group esteem*, rather than *social distance*. Additionally, we show preliminary evidence that age differences induce larger response pattern variation than does coethnicity. Our findings speak to the importance of age in social interaction in Africa and provide important lessons for the survey research community.

## Data

The data for this paper is Afrobarometer (rounds 3 and 4) data from Adida et al. 2016 (`AFPR_data.dta`) and Afrobarometer Round 7 (). [Replication files.](https://journals.sagepub.com/doi/suppl/10.1177/0010414016633487)

## `scripts`

1) `clean_and_combine_data.R` combines and cleans Afrobarometer rounds 3, 4, and 7 and outputs `afpr_append.rds`. For round 7, there are three datasets to combine: Mauritius, Uganda, and the full Round 7 Afrobaromter dataset. These three datasets are combined before merging into rounds 3 and 4.

1) `append_age_differences.R` appends age difference variables to the Afrobarometer data based on the ages of interviewers and respondents and saves this as a new dataset: `afpr_ages.rds`.

2) `variable_labels.R` generates a dataframe of variable names, variable descriptions, and variable groupings. This dataframe is used in subsequent files to group and relabel outcomes.

3) `balance_tables.R` uses `afpr_ages.rds` to generate the covariate balance table found in Table 3 of the paper. 

4) `descriptive_statistics.R` uses `afpr_ages.rds` to create tables and figures that compare the age distributions of respondents and interviewers in the Afrobarometer data (Table 2 and Figure 1 in the paper). This file also creates tables that compare the responses of older and younger interviewers (Table 4 and Table 5 in the paper). 

4) `contrasts.R` runs the main models (with 35-years old age cutoff) as well as the appendix models (with 40-years old age cutoff and 10-year age difference) using `afpr_ages.rds`. Model estimates are tidied into a dataframe and saved as `age_diff_models.rds`.

5) `plots.R` creates plots of our main findings (Figures 2 to 5) and the plots found in the appendix of the paper (Figures 6 to 13).

## `tables`

Contains csv files of all the main tables found in the paper.

## `figs`

Contains all of the figures found in the paper. 





