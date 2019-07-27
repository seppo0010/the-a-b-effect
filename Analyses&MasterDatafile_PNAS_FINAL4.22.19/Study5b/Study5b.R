#The A/B Effect: Objecting to experiments that compare two unobjectionable policies or treatments
#Study 5b: Drug Effectiveness Walk-In: replication using Pollfish

rm(list = ls()) #clear workspace

#Load packages
library(tidyverse) #for ggplot and filter
library(readxl) #for read
library(effsize) #for cohens d
library(gridExtra) #for multi plots
# library(psy) #for alpha
# library(psych) #for anova #comment in for ANOVA #can interfere with cohen's d function, so we load it later in this script
library(scales) #for colors
library(Rmisc) #for summary stats
library(knitr) #for tabling in R
library(kableExtra) #for tabling in R
library(broom) #for tabling in R

#read in the desired worksheet 
df <- read_excel("ABI_MasterDatafile.xlsx", sheet = "FullExperimentsOnly")
df <- filter(df,RepeatTurkerLabelCode != "1") #Exclude those with 1: participants anywhere in the dataset who took our very first survey AND another survey

#For workspace, keep only useful columns (grouping variables, dv, and dv_binary)
# df <- df[,c(1:14)] #only first 14 columns in data sheet
df.DrugTeach <- filter(df, df$Scenario == "DrugTeach_pf") #pull out Study 5b only
df.DrugTeach$condFinal <- factor(df.DrugTeach$condFinal, levels = c("Drug A", "Drug B", "A/B Learn")) #Reorder the factor for reporting in this order

# print(levels(df.DrugTeach$condFinal)) #to see factor levels if necessary

#Summary statistics of responses to each vignette
df.DrugTeach.summary <- summarySE(df.DrugTeach, measurevar="dv", groupvars="condFinal") #Summarize mean, sd, se, ci

df.SummaryPercent = summarySE(df.DrugTeach, measurevar = "dv_binary", groupvars = c("Scenario", "condFinal")) #Summarize proportion objecting
df.SummaryPercent <- df.SummaryPercent %>% mutate(dv_binary = dv_binary*100) #turn into percentages

df.DrugTeach.Summary <- add_column(df.DrugTeach.summary, percentObj = df.SummaryPercent[,4], .after = "N") #Add columns of percentages to table of summary stats
df.DrugTeach.Summary <- rename(df.DrugTeach.Summary,c("condFinal"="Condition","percentObj"="% Objecting", "dv"="M Appropriateness","sd"="SD","se"="SEM","ci"="95% CI +/-")) #rename table headers for export
write_excel_csv(df.DrugTeach.Summary,"Output/Study5bDescriptives.csv") #export to csv

# Histogram of all ratings for each condition
facet_names <- c(
  `Drug A` = "Drug A",
  `Drug B` = "Drug B",
  `A/B Learn` = "A/B learn"
)

ggplot(df.DrugTeach) +
  geom_bar(mapping = aes(x = dv, y = ..prop..), fill = 'light blue', color = 'black', width = .75) +
  facet_grid(. ~ condFinal,labeller = as_labeller(facet_names)) +
  theme(text = element_text(size=20)) +
  theme(axis.text = element_text(colour = 'black')) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  coord_cartesian(ylim=c(0,.6), expand = TRUE) +
  scale_y_continuous(breaks=seq(0, .6, .1), expand = c(0,0)) +
  ylab('Proportion Choosing') +
  xlab('Appropriateness')

# ggsave("Study5bHisto.png",height = 3) #save png file

#t-tests
#Create dataframes of each set of t-test after tidying with broom::tidy
#No longer an A/B Short condition, so we comment out tests we can no longer run.
AvB <- tidy((t.test(filter(df.DrugTeach, condFinal == 'Drug A')$dv,filter(df.DrugTeach, condFinal == 'Drug B')$dv, var.equal = TRUE))) #A vs. B
# ABSvABL <- tidy((t.test(filter(df.DrugTeach, condFinal == 'A/B')$dv,filter(df.DrugTeach, condFinal == 'A/B Learn')$dv, var.equal = TRUE))) #A/B Short vs. A/B Learn
AvAB <- tidy((t.test(filter(df.DrugTeach, condFinal == 'Drug A')$dv,filter(df.DrugTeach, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #A vs. A/B (collapsed together)
BvAB <- tidy((t.test(filter(df.DrugTeach, condFinal == 'Drug B')$dv,filter(df.DrugTeach, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #B vs. A/B (collapsed together)
PvE <- tidy((t.test(filter(df.DrugTeach, policyOrAB == 'Policy')$dv,filter(df.DrugTeach, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #Policy vs. Experiment: A & B (collapsed) vs. A/B (collapsed)

#Bind all t-tests into a single dataframe; transpose with t() function to right of <-
df.DrugTeach.ttests <- t(do.call(rbind, Map(data.frame, AvB=AvB, AvAB=AvAB, BvAB=BvAB, PolicyVsExperiment=PvE)))

#Include only first five columns
df.DrugTeach.ttests <- df.DrugTeach.ttests[ , c(1:5) ]

#Add cohen's d for the comparison to the end of this table
effSizes <- data.frame(unname(c(
  cohen.d(filter(df.DrugTeach, condFinal == 'Drug A')$dv,filter(df.DrugTeach, condFinal == 'Drug B')$dv)[[3]],      #A vs. B
  # cohen.d(filter(df.DrugTeach, condFinal == 'A/B')$dv,filter(df.DrugTeach, condFinal == 'A/B Learn')$dv)[[3]],    #A/B Short vs. A/B Learn
  cohen.d(filter(df.DrugTeach, condFinal == 'Drug A')$dv,filter(df.DrugTeach, policyOrAB == 'AB Test')$dv)[[3]],    #A vs. A/B (collapsed together)
  cohen.d(filter(df.DrugTeach, condFinal == 'Drug B')$dv,filter(df.DrugTeach, policyOrAB == 'AB Test')$dv)[[3]],    #B vs. A/B (collapsed together)
  cohen.d(filter(df.DrugTeach, policyOrAB == 'Policy')$dv,filter(df.DrugTeach, policyOrAB == 'AB Test')$dv)[[3]]))) #Policy vs. Experiment: A & B (collapsed) vs. A/B (collapsed)

Group1 <- c("A","A","B","A or B")
Group2 <- c("B","A/B Learn","A/B Learn","A/B Learn")

df.DrugTeach.ttests <- cbind(df.DrugTeach.ttests,effSizes,Group1,Group2) #bind column of cohen's d to the t-test table

#Rename columns
colnames(df.DrugTeach.ttests) <- c("Mean 1", "Mean 2", "t", "p", "df","d","Group 1","Group 2")
df.DrugTeach.ttests <- df.DrugTeach.ttests[, c("Group 1","Group 2","Mean 1","Mean 2","t","df","p","d")] #Reorder columns so that df comes right after t
write.csv(df.DrugTeach.ttests,"Output/Study5bTests.csv",row.names = TRUE) #export to csv

##Analyze Order Effect

df.DrugTeach.orderSummary <- summarySE(df.DrugTeach, measurevar="dv", groupvars="poleOrderPollfish") #Summarize mean, sd, se, ci 
t.test(filter(df.DrugTeach, poleOrderPollfish == 'HiToLo')$dv,filter(df.DrugTeach, poleOrderPollfish == 'LoToHi')$dv, var.equal = TRUE) #t-test for order effect
cohen.d(filter(df.DrugTeach, poleOrderPollfish == 'HiToLo')$dv,filter(df.DrugTeach, poleOrderPollfish == 'LoToHi')$dv)[[3]] #d for order effect

library(psych) #load in psych package for ANOVA; have to load it here because it conflicts with cohens.d function earlier.

fit <- aov(dv ~ policyOrAB*poleOrderPollfish, data=df.DrugTeach)
model.DrugTeach <- summary(fit)

anovaSummary = describeBy(df.DrugTeach$dv,list(df.DrugTeach$policyOrAB,df.DrugTeach$poleOrderPollfish), mat=TRUE,digits=2)
# anovaSummary$se = anovaSummary$sd/sqrt(anovaSummary$n)

limits = aes(ymax = mean + (1.96*se), ymin=mean - (1.96*se)) #95% CI limits

dodge = position_dodge(width=0.9)

apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'))

ggplot(anovaSummary, aes(x = group2, y = mean, fill = group1))+
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  theme_set(theme_classic(base_size = 24))+
  ylab('Appropriateness') + xlab('Scale Order') + guides(fill=guide_legend(title=NULL),size=12) +
  scale_x_discrete(labels=c("Descending","Ascending")) +
  coord_cartesian(ylim=c(1,5), expand = TRUE) +
  scale_y_continuous(breaks=seq(1, 5, 1), expand = c(0,0)) +
  scale_fill_grey() +
  theme(text = element_text(size=24)) +
  theme(axis.text = element_text(colour = 'black')) +
  theme(legend.position = c(0.8, 0.9))

