# DEFINE VECTORS OF OUTCOMES AND GENERATE VARIABLE/OUTCOME/OUTCOME GROUP DATAFRAME ----
eth_outcomes <- c("idrank", "patronage", "etrust", 
                  "netrust", "ecconditions_group", 
                  "polinfluence_group", "treatedunfairly_group") 

pol_outcomes <- c("demosupp", "performance", "bribe1", 
                  "ec_conditions_ctry", "trust_rulingparty", "trust_opposition",  
                  "gov_manage_economy", "voted", "pubaffairs", 
                  "meeting", "knows_MP")  

stat_outcomes <- c("ec_conditions_self", "notenoughfood", "noincome", 
                   "nocleanwater", "crime", "aids")

pro_outcomes <- c("hostile", "uncooperative", 
                  "impatient", "suspicious")

youth_outcomes <- c("youth_needs", 
                    "youth_employment", 
                    "youth_pregnancy",
                    "youth_drugabuse", 
                    "youth_alcohol",
                    "youth_delinquency",
                    "youth_smoking")

all_outcomes <- c(eth_outcomes, pol_outcomes, stat_outcomes, pro_outcomes, youth_outcomes)



variable_labels <- 
  data.frame(label = as.factor(
    # STAT OUTCOMES
    c("Knows someone who died of AIDS",
    "Has feared crime in own home",
    "Has gone without cash income",
    "Has gone without clean water",
    "Has gone without enough food",
    "Own present living conditions bad",
    # POL OUTCOMES
    "Exposure to vote buying",
    "Knows MP's name",
    "Government's handling of the economy",
    "Country's economic conditions",
    "Approval of president's performance",
    "Attendance at community meetings",
    "Interest in public affairs",
    "Preference for democracy",
    "Trust in opposition parties",
    "Trust in ruling party",
    "Voted in last national election",
    # ETH OUTCOMES
    "Ethnic group is treated unfairly",
    "Ethnic group's economic conditions",
    "Ethnic group's political influence",
    "Leaders should help home community",
    "National vs. ethnic identification",
    "Trust for coethnics",
    "Trust for non-coethnics",
    # PRO OUTCOMES
    "Respondent was uncooperative",
    "Respondent was suspicious",
    "Respondent was hostile",
    "Respondent was impatient", 
    # YOUTH OUTCOMES
    "Government's handling of addressing needs of youth",
    "Government's handling of youth employment",
    "Government's handling teenage pregnancy",
    "Government's handling of drug abuse among youth",
    "Government's handling of underage consumption of alcohol",
    "Government's handling of youth delinquency",
    "Government's handling of smoking among youth")),
    
    var = c(# STAT OUTCOMES
            "aids",
            "crime",
            "noincome",
            "nocleanwater",
            "notenoughfood",
            "ec_conditions_self",
            # POL OUTCOMES
            "bribe1",
            "knows_MP",
            "gov_manage_economy",
            "ec_conditions_ctry",
            "performance",
            "meeting",
            "pubaffairs",
            "demosupp",
            "trust_opposition",
            "trust_rulingparty",
            "voted",
            # ETH OUTCOMES
            "treatedunfairly_group",
            "ecconditions_group",
            "polinfluence_group",
            "patronage",
            "idrank",
            "etrust",
            "netrust",
            # PRO OUTCOMES
            "uncooperative",
            "suspicious",
            "hostile",
            "impatient",
            # YOUTH OUTCOMES
            "youth_needs", 
            "youth_employment", 
            "youth_pregnancy",
            "youth_drugabuse", 
            "youth_alcohol",
            "youth_delinquency",
            "youth_smoking")) %>%
  mutate(group = case_when(var %in% eth_outcomes ~ "eth_outcomes",
                           var %in% pol_outcomes ~ "pol_outcomes",
                           var %in% stat_outcomes ~ "stat_outcomes",
                           var %in% pro_outcomes ~ "pro_outcomes",
                           var %in% youth_outcomes ~ "youth_outcomes"))
  