rm(list=ls())
setwd("~/Dropbox/Gender differences")

library(ggplot2)
library(foreign)
library(plyr)
library(dplyr)
library(xtable)
library(TeachingDemos)
library(reporttools)
library(gridExtra)
library(vcd)
###############################################################################
# Bring-in data
###############################################################################
AB45 <- read.dta("ANALYSES/AB/GGR_FinalData.dta", convert.factors = F)
str(AB45)
names(AB45)

female <- AB45[AB45$female==1,]
male <- AB45[AB45$female==0,]

###############################################################################
# subset data
###############################################################################
countries <- AB45 %>%
  filter(ctag==1) %>%
  select(ROUND, total_gap, femD_Economy, femD_Poverty, femD_Infrastructure, femD_Health, femD_Water, femD_Education, femD_Agriculture, 
         femD_Violence, femD_Rights, femD_Services, femD_polindex, gap_polindex, femD_polinformA, gap_polinformA, 
         Country, ccodewb, wempower, WMPshare, vulA, muslimshare, femploy, avg_edgap)

countries$ROUND=factor(countries$ROUND)
countries$WMPshare=as.numeric(countries$WMPshare)
countries$wempower=as.numeric(countries$wempower)
head(countries)

# reshape into long format
dfl <- reshape(countries, 
               varying = c("femD_Economy", "femD_Poverty", "femD_Infrastructure", "femD_Health", "femD_Agriculture",
                           "femD_Water", "femD_Education", "femD_Violence", "femD_Rights", "femD_Services"), 
               v.names = "Gap",
               timevar = "Policy", 
               times = c("Economy", "Poverty", "Infrastructure", "Health", "Agriculture",
                         "Water", "Education", "Violence", "Rights", "Services"), 
               new.row.names = 1:1000,
               direction = "long")

dfl$Policy_f = factor(dfl$Policy, levels=c("Economy", "Poverty", "Infrastructure", "Health", "Agriculture",
                                           "Water", "Education", "Violence", "Rights", "Services"))

######################################
# Manuscript Figure 5 
######################################
head(dfl)
table(dfl$Policy)
mean(dfl$Gap)

PolicyMean = ddply(dfl,~Policy,summarise,mean=mean(Gap),sd=sd(Gap))

## Compute the quantile
qrcp.df = ddply(.data = dfl, .variables = .(Policy), transform,
                lQntl = quantile(Gap, probs = 0.25, na.rm = TRUE),
                uQntl = quantile(Gap, probs = 0.75, na.rm = TRUE))

## Compute the lower and upper bound which defines the outlier
brcp.df = ddply(.data = qrcp.df, .variables = .(Policy), transform,
                lBound = lQntl - 1.5 * (uQntl - lQntl),
                uBound = uQntl + 1.5 * (uQntl - lQntl))

## Remove the country names if it is within the bounds
with(brcp.df, {
  brcp.df[Gap <= uBound &
            Gap >= lBound, "ccodewb"] <<- NA
})

## Plot the data
set.seed(587)
pdf("Drafts/Figures/PolicyDomainDistributionALL.pdf", width=12, height=7)
ggplot(data = brcp.df, aes(x = Policy_f, y = Gap)) +
  geom_boxplot(outlier.colour = NA) +
  geom_text(aes(label = ccodewb), size = 3,
            position = position_jitter(width = 0.1)) + theme_bw() +
  labs(x = NULL, y = "Female share - Male share", title = "") +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="black") +
  geom_hline(aes(yintercept=0), colour="red", linetype="solid") +
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 1.5),
        axis.title.y = element_text(colour="grey20",size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,face="bold"),
        axis.text.y = element_text(colour="grey20",size=12,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=14,face="bold"), 
        axis.ticks.y = element_blank())
dev.off()  

###############################################################################
# Figure 9: relationship between total gap and Women MP Share in parliament (2008) 
###############################################################################

# drop Guinea that has missing data
countriesG<- filter(countries, ccodewb!="GIN")
(MPcor4=round(cor(countriesG$total_gap[countriesG$ROUND==4], countriesG$WMPshare[countriesG$ROUND==4]), 3))
(MPcor5=round(cor(countriesG$total_gap[countriesG$ROUND==5], countriesG$WMPshare[countriesG$ROUND==5]), 3))

# Total Gap (fig.9)
pdf("Drafts/Figures/PoliticalRepresentationGray.pdf", width=12, height=7)
(MPShare = ggplot(countriesG, aes(x=WMPshare, y=total_gap, fill=ROUND, colour=ROUND)) +geom_point(aes(colour = ROUND),  size=3.5)+ 
  stat_smooth(method=lm, se=F) + scale_colour_grey() +
  scale_y_continuous(name="Total Policy Prioriies Gender Gap", breaks = round(seq(min(0.1), max(0.7), by = 0.1),1), limits = c(0.1, 0.75)) +
  scale_x_continuous(name="Women MP Share in parliament", limits=c(0.05,0.45), breaks = seq(0.05, 0.45, 0.10)) + theme_bw()+
  geom_text(aes(x=.27, y=0.37, label=paste("r^2==", MPcor4), group=NULL), size = 4, color = "grey50", data=countriesG, parse = T) +
  geom_text(aes(x=.27, y=0.25, label=paste("r^2==", MPcor5), group=NULL), size = 4, color = "black", data=countriesG, parse = T) +
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 1.5),
        axis.title.y = element_text(colour="grey20",size=15,face="bold"),
        axis.text.x = element_text(colour="grey20",size=13,face="bold"),
        axis.text.y = element_text(colour="grey20",size=13,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=15,face="bold"), 
        axis.ticks.y = element_blank()))
dev.off() 

# Disaggregated policy prioritization 
# Figure A.24 (online appendix)
pdf (file="Drafts/Figures/PoliticalRepresentation_All.pdf", height=10, width=10)
ggplot(dfl, aes(x=WMPshare, y=Gap)) + geom_point(aes(colour = ROUND)) + stat_smooth(method=lm, se=T) +
  scale_y_continuous(name="Policy Priority Gender Gap") +
  scale_x_continuous(name="Women MP Share in parliament") + theme_bw() + facet_wrap(~Policy_f)+
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 1.5),
        axis.title.y = element_text(colour="grey20",size=15,face="bold"),
        axis.text.x = element_text(colour="grey20",size=13,face="bold"),
        axis.text.y = element_text(colour="grey20",size=13,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=15,face="bold"), 
        axis.ticks.y = element_blank())
dev.off()

###############################################################################
# Manuscript figure 10
# correlation with political participation
###############################################################################
# relationship between total gap and political participation gender gap
(Partcor=round(cor(countries$total_gap, countries$gap_polindex), 3))
(Partcor5=round(cor(countries$total_gap[countries$ROUND==4], countries$gap_polindex[countries$ROUND==4]), 3))
(Partcor4=round(cor(countries$total_gap[countries$ROUND==5], countries$gap_polindex[countries$ROUND==5]), 3))

pdf("Drafts/Figures/ParticipationIndexGray.pdf", width=12, height=7)
ggplot(countries, aes(x=gap_polindex, y=total_gap, fill=ROUND, colour=ROUND)) + geom_point(aes(colour = ROUND), size=3.5) +
  stat_smooth(method=lm, se=F) + annotate(geom="text", x=0.2, y=0.68, label="MALI",color="black") + scale_colour_grey() +
  scale_y_continuous(name="Total Policy Prioriies Gender Gap", breaks = round(seq(min(0.1), max(0.7), by = 0.1),1), limits = c(0.1, 0.75)) +
  scale_x_continuous(name="Political Participation Gender Gap") + theme_bw()+
  geom_text(aes(x=0.1, y=0.34, label=paste("r^2==", Partcor4), group=NULL), size = 4, 
            color = "grey50", data=countries, parse = T) +
  geom_text(aes(x=0.1, y=0.24, label=paste("r^2==", Partcor5), group=NULL), size = 4, 
            color = "black", data=countries, parse = T) +
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 1.5),
        axis.title.y = element_text(colour="grey20",size=15,face="bold"),
        axis.text.x = element_text(colour="grey20",size=13,face="bold"),
        axis.text.y = element_text(colour="grey20",size=13,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=15,face="bold"), 
        axis.ticks.y = element_blank())
dev.off() 

# dissagrgated by policy domain (Fig. A.23 online appendix)
pdf (file="Drafts/Figures/ParticipationIndex_All.pdf", height=10, width=10)
ggplot(dfl, aes(x=gap_polindex, y=Gap)) + geom_point(aes(colour = ROUND)) + stat_smooth(method=lm, se=T) +
  scale_y_continuous(name="Policy Priority Gender Gap (Female Share - Male Share)") +
  scale_x_continuous(name="Political Participation Gender Gap") + theme_bw() + facet_wrap(~Policy_f)+
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 1.5),
        axis.title.y = element_text(colour="grey20",size=15,face="bold"),
        axis.text.x = element_text(colour="grey20",size=13,face="bold"),
        axis.text.y = element_text(colour="grey20",size=13,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=15,face="bold"), 
        axis.ticks.y = element_blank())
dev.off()

#################################################################################
# figure A.1 (SI)

dfl$Policy_f = with(dfl, factor(Policy_f, levels = rev(levels(Policy_f))))

pdf("Drafts/Figures/CountryDiff.pdf", width=12, height=10)
ggplot(dfl, aes(y=Policy_f, x=Gap, shape=ROUND)) +geom_point(aes(colour = ROUND)) +
  facet_wrap(~Country, ncol = 5)+ theme_bw() + scale_y_discrete(name="") +
  geom_vline(xintercept =  0) + scale_x_continuous(name="Female share - Male share")+
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 1.5),
        axis.text.x = element_text(colour="grey20",size=12,face="bold"),
        axis.title.x = element_text(colour="grey20",size=14,face="bold"), 
        axis.ticks.y = element_blank())
dev.off()

####################################################
# relationship between vars at the individual level
# Fig. A.2 (SI)
####################################################
key_vars =read.dta("DATA/CountryLevel/KeyCorrelated.dta", convert.factors = F)
head(key_vars)

key_vars$edgap = factor(key_vars$edgap, labels=c("Not below", "Below male edu"))
key_vars$female = factor(key_vars$female, labels=c("Male", "Female"))
key_vars$employment = factor(key_vars$employment, labels=c("Not employed", "Employed"))

vnames <- list(set_varnames = c(edgap="Edu-gap", female="Sex", employment="Employed"))

pdf("Drafts/Figures/KeyCorrelatedIndC.pdf", width=12, height=7)
CAT= cotabplot(~edgap + employment | female, data = key_vars, labeling_args=vnames, panel = cotab_coindep)
dev.off()

###############################################################################
# Relationship between social vulnerability and share of female employment
# Fig. A.3 (SI)
###############################################################################
KeyCorrelate <- AB45 %>%
  select(Country, COUNTRY, ROUND,  vulA, femploy, muslimshare, gdp, ccodewb, CABBR)  %>%
  filter (ROUND==5, CABBR!="BDI") %>%
  group_by(CABBR) %>%
  summarize( vulA = mean(vulA, na.rm=T), 
             muslimshare = mean(muslimshare, na.rm=T),
             GDP = mean(gdp, na.rm=T),
             femploy = mean(femploy, na.rm=T))

head(KeyCorrelate)

pdf("Drafts/Figures/KeyCorrelated.pdf", width=12, height=7)
ggplot(KeyCorrelate, aes(x=femploy, y=vulA)) + geom_point(colour="red", size=3) +
  stat_smooth(method=loess, se=F)  +  ggtitle("Correlation between Vulnrability and Share female employment") +
  geom_text(aes(label=CABBR),hjust=0, vjust=0) +
  scale_y_continuous(name="Vulnrability index", breaks = round(seq(min(-1.75), max(1.75), by = 0.25),2), limits = c(-1.75, 1.75)) +
  scale_x_continuous(name="Share female employment") + theme_bw()+
  theme(plot.title = element_text(size = 18, face = "bold", vjust = 1.5),
        axis.title.y = element_text(colour="grey20",size=15,face="bold"),
        axis.text.x = element_text(colour="grey20",size=13,face="bold"),
        axis.text.y = element_text(colour="grey20",size=13,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=15,face="bold"), 
        axis.ticks.y = element_blank())
dev.off()

###############################################################################
# elationship between female employmentm female vulnerability and policy prioritization
# Fig. A.4 (SI)
###############################################################################

# subset data - using levels 
###############################################################################
CountriesGender <- AB45 %>%
  select(Country, COUNTRY, ROUND, female, Economy, Poverty, Infrastructure, Water, Agriculture, ccodewb, 
         vulA, femploy, muslimshare, WECON, WOPOL, gdp, Withinwt)  %>%
  group_by(Country, ROUND, female) %>%
  summarize(Economy = weighted.mean(Economy, Withinwt, na.rm=T), 
            Poverty = weighted.mean(Poverty, Withinwt, na.rm=T), 
            Infrastructure = weighted.mean(Infrastructure, Withinwt, na.rm=T), 
            Water = weighted.mean(Water, Withinwt, na.rm=T), 
            vulA = mean(vulA, na.rm=T), 
            muslimshare = mean(muslimshare, na.rm=T),
            WECON = mean(WECON, na.rm=T),
            WOPOL = mean(WOPOL, na.rm=T),
            GDP = mean(gdp, na.rm=T),
            femploy = mean(femploy, na.rm=T))

CountriesGender$ROUND=factor(CountriesGender$ROUND)
CountriesGender$Gender=factor(CountriesGender$female, labels=c("Male", "Female"))
CountriesGender$lgdp=log(CountriesGender$GDP)
head(CountriesGender)

(INF1 = ggplot(CountriesGender, aes(x=femploy, y=Infrastructure, shape=Gender, colour=Gender)) + geom_point(aes(colour = Gender), size=3) +
  stat_smooth(method=loess, se=F)  +  ggtitle("Infrastructure") +
  scale_y_continuous(name="Population Share",  breaks = round(seq(min(0), max(0.7), by = 0.1),2), limits = c(0, 0.7)) +
  scale_x_continuous(name="Share female employment") + theme_bw()+
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 1.5),
        axis.title.y = element_text(colour="grey20",size=15,face="bold"),
        axis.text.x = element_text(colour="grey20",size=13,face="bold"),
        axis.text.y = element_text(colour="grey20",size=13,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=15,face="bold"), 
        axis.ticks.y = element_blank()))

(INF2 = ggplot(CountriesGender, aes(x=vulA, y=Infrastructure, shape=Gender, colour=Gender)) + geom_point(aes(colour = Gender), size=3) +
  stat_smooth(method=loess, se=F)  + ggtitle("Infrastructure") +
  scale_y_continuous(name="",  breaks = round(seq(min(0), max(0.7), by = 0.1),2), limits = c(0, 0.7)) +
  scale_x_continuous(name="Vulnerability index") + theme_bw()+
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 1.5),
        axis.title.y = element_text(colour="grey20",size=15,face="bold"),
        axis.text.x = element_text(colour="grey20",size=13,face="bold"),
        axis.text.y = element_text(colour="grey20",size=13,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=15,face="bold"), 
        axis.ticks.y = element_blank()))

(WAT1 = ggplot(CountriesGender, aes(x=femploy, y=Water, shape=Gender, colour=Gender)) + geom_point(aes(colour = Gender), size=3) +
  stat_smooth(method=loess, se=F)  +  ggtitle("Water") +
  scale_y_continuous(name="Population Share",  breaks = round(seq(min(0), max(0.7), by = 0.1),2), limits = c(0, 0.7)) +
  scale_x_continuous(name="Share female employment") + theme_bw()+
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 1.5),
        axis.title.y = element_text(colour="grey20",size=15,face="bold"),
        axis.text.x = element_text(colour="grey20",size=13,face="bold"),
        axis.text.y = element_text(colour="grey20",size=13,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=15,face="bold"), 
        axis.ticks.y = element_blank()))

(WAT2 = ggplot(CountriesGender, aes(x=vulA, y=Water, shape=Gender, colour=Gender)) + geom_point(aes(colour = Gender), size=3) +
  stat_smooth(method=loess, se=F)  + ggtitle("Water") +
  scale_y_continuous(name="",  breaks = round(seq(min(0), max(0.7), by = 0.1),2), limits = c(0, 0.7)) +
  scale_x_continuous(name="Vulnerability index") + theme_bw()+
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 1.5),
        axis.title.y = element_text(colour="grey20",size=15,face="bold"),
        axis.text.x = element_text(colour="grey20",size=13,face="bold"),
        axis.text.y = element_text(colour="grey20",size=13,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=15,face="bold"), 
        axis.ticks.y = element_blank()))

(POV1 = ggplot(CountriesGender, aes(x=femploy, y=Poverty, shape=Gender, colour=Gender)) + geom_point(aes(colour = Gender), size=3) +
  stat_smooth(method=loess, se=F)  +  ggtitle("Poverty") +
  scale_y_continuous(name="Population Share",  breaks = round(seq(min(0), max(0.9), by = 0.1),2), limits = c(0, 0.9)) +
  scale_x_continuous(name="Share female employment") + theme_bw()+
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 1.5),
        axis.title.y = element_text(colour="grey20",size=15,face="bold"),
        axis.text.x = element_text(colour="grey20",size=13,face="bold"),
        axis.text.y = element_text(colour="grey20",size=13,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=15,face="bold"), 
        axis.ticks.y = element_blank()))

(POV2 = ggplot(CountriesGender, aes(x=vulA, y=Poverty, shape=Gender, colour=Gender)) + geom_point(aes(colour = Gender), size=3) +
  stat_smooth(method=loess, se=F)  + ggtitle("Poverty") +
  scale_y_continuous(name="",  breaks = round(seq(min(0), max(0.9), by = 0.1),2), limits = c(0, 0.9)) +
  scale_x_continuous(name="Vulnerability index") + theme_bw()+
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 1.5),
        axis.title.y = element_text(colour="grey20",size=15,face="bold"),
        axis.text.x = element_text(colour="grey20",size=13,face="bold"),
        axis.text.y = element_text(colour="grey20",size=13,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=15,face="bold"), 
        axis.ticks.y = element_blank()))

##################################################
# combine the six figures into a single graph (A.4)
##################################################
pdf("Drafts/Figures/ALLGenderLevels.pdf", width=12, height=15)
grid.arrange(INF1, INF2, WAT1, WAT2, POV1, POV2, ncol=2, top="")
dev.off()

###############################################################################
# Additional correlates at the individual-level
# A.5 (SI)
###############################################################################
KeyCorrelateInd <- AB45 %>%
  select(Water, Economy, Poverty, Infrastructure, edgap, employment, female, Withinwt) %>% 
  filter(!is.na(edgap), !is.na(employment))  %>%
  group_by(female, edgap, employment) %>%
  summarize(Economy = weighted.mean(Economy, Withinwt, na.rm=T), 
            Poverty = weighted.mean(Poverty, Withinwt, na.rm=T), 
            Infrastructure = weighted.mean(Infrastructure, Withinwt, na.rm=T), 
            Water = weighted.mean(Water, Withinwt, na.rm=T))

KeyCorrelateInd$female=factor(KeyCorrelateInd$female, labels=c("Male", "Female"))
KeyCorrelateInd$employment=factor(KeyCorrelateInd$employment, labels=c("Not employed", "Employed"))
KeyCorrelateInd$edgap=factor(KeyCorrelateInd$edgap, labels=c("Not vul", "Vul"))
head(KeyCorrelateInd)

ECONIND = ggplot(data=KeyCorrelateInd, aes(y=Economy, x=female, fill=female)) + 
  geom_bar(stat="identity") + facet_wrap(~ employment + edgap, nrow=1) +
  ggtitle("Economy") + scale_y_continuous(name="") + scale_x_discrete(name="") + theme_bw()+
  theme(plot.title = element_text(size = 18, face = "bold", vjust = 1.5),
        axis.title.y = element_text(colour="grey20",size=15,face="bold"),
        axis.text.x = element_text(colour="grey20",size=13,face="bold"),
        axis.text.y = element_text(colour="grey20",size=13,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=15,face="bold"), 
        legend.position="none", axis.ticks.y = element_blank())

INFIND = ggplot(data=KeyCorrelateInd, aes(y=Infrastructure, x=female, fill=female)) + 
  geom_bar(stat="identity") + facet_wrap(~ employment + edgap, nrow=1) +
  ggtitle("Infrastructure") + scale_y_continuous(name="") + scale_x_discrete(name="") + theme_bw()+
  theme(plot.title = element_text(size = 18, face = "bold", vjust = 1.5),
        axis.title.y = element_text(colour="grey20",size=15,face="bold"),
        axis.text.x = element_text(colour="grey20",size=13,face="bold"),
        axis.text.y = element_text(colour="grey20",size=13,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=15,face="bold"), 
        legend.position="none", axis.ticks.y = element_blank())

WATERIND = ggplot(data=KeyCorrelateInd, aes(y=Water, x=female, fill=female)) + 
  geom_bar(stat="identity") + facet_wrap(~ employment + edgap, nrow=1) +
  ggtitle("Water") + scale_y_continuous(name="") + scale_x_discrete(name="") + theme_bw()+
  theme(plot.title = element_text(size = 18, face = "bold", vjust = 1.5),
        axis.title.y = element_text(colour="grey20",size=15,face="bold"),
        axis.text.x = element_text(colour="grey20",size=13,face="bold"),
        axis.text.y = element_text(colour="grey20",size=13,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=15,face="bold"),
        legend.position="none",axis.ticks.y = element_blank())

POVIND = ggplot(data=KeyCorrelateInd, aes(y=Poverty, x=female, fill=female)) + 
  geom_bar(stat="identity") + facet_wrap(~ employment + edgap, nrow=1) +
  ggtitle("Poverty") + scale_y_continuous(name="") + scale_x_discrete(name="") + theme_bw()+
  theme(plot.title = element_text(size = 18, face = "bold", vjust = 1.5),
        axis.title.y = element_text(colour="grey20",size=15,face="bold"),
        axis.text.x = element_text(colour="grey20",size=13,face="bold"),
        axis.text.y = element_text(colour="grey20",size=13,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=15,face="bold"),
        legend.position="none",axis.ticks.y = element_blank())

pdf("Drafts/Figures/RawDataIND.pdf", width=12, height=15)
grid.arrange(INFIND, WATERIND, POVIND, ncol=1, top="")
dev.off()

