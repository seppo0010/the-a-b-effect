#The A/B Effect: Objecting to experiments that compare two unobjectionable policies or treatments
#Science Literacy Analyses: All Experiments (with science lit data)

rm(list = ls())

# #Load packages
library(tidyverse) #for ggplot and filter
library(readxl) #for read
library(effsize) #for cohens d
# library(gridExtra) #for multi plots
# library(psy) 
library(psych) #for reliability analysis
# library(scales) #for colors
# library(Rmisc) #for summary stats
library(knitr)
# library(kableExtra)
library(broom)
library(plotrix) #for standard error function
# detach(package:plyr) #have to detach to use Summarise properly
# options(digits=3) #set to 3 for table outputs
# 
#Read in the desired worksheet 
df.SciLit <- read_excel("ABI_MasterDatafile.xlsx", sheet = "FullExperimentsOnly")
df.SciLit <- filter(df.SciLit,RepeatTurkerLabelCode != "1") #Exclude those with 1: participants anywhere in the dataset who took our very first survey AND another survey
df.SciLit <- filter(df.SciLit, df.SciLit$StudyDescription == "Checklist:SmallEffectReplication" | df.SciLit$StudyDescription == "AV + GT" | df.SciLit$StudyDescription == "ManyDomains" ) #keep in only studies with sci lit data

#Keep only useful columns and only the full experiment data
df.SciLit <- df.SciLit[,c(1:59)] #remove hanging columns

df.SciLit$policyOrAB = factor(df.SciLit$policyOrAB, levels = c("Policy","AB Test"))

labels <- c(Policy = "Policy (A or B)", `AB Test` = "A/B Test") #Create labeller for strip text in histogram

#histogram of SciLit scores
ggplot(df.SciLit) +
  geom_bar(mapping = aes(x = ScienceLitSummed, y = ..prop..), fill = 'light blue', color = 'black', width = .75) +
  facet_wrap(~ policyOrAB, ncol = 1,labeller=labeller(policyOrAB = labels)) +
  theme(text = element_text(size=24)) +
  theme(axis.text = element_text(colour = 'black')) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  coord_cartesian(ylim=c(0,.5), expand = TRUE) +
  coord_cartesian(xlim=c(0,9), expand = TRUE) +
  scale_y_continuous(breaks=seq(0, .5, .1), expand = c(.1,0)) +
  scale_x_continuous(breaks=seq(0, 9, 1), expand = c(.07,0)) +
  ylab('Proportion of Participants') +
  xlab('Correct Answers')

#Summary stats, including appropriateness ratings, for science lit scores
df.SciLitSummary <- df.SciLit %>%
  group_by(policyOrAB) %>% 
  dplyr::summarise(N=n(), Mean=mean(ScienceLitSummed), Median=median(ScienceLitSummed), Std=sd(ScienceLitSummed),StdErr = std.error(ScienceLitSummed,na.rm = TRUE))

#Summary stats, including appropriateness ratings, grouped by STEM Degree or not
df.StemDegreeSummary <- df.SciLit %>%
  group_by(policyOrAB,STEMdegree) %>% 
  dplyr::summarise(N=n(), Mean=mean(dv), Std=sd(dv),StdErr = std.error(dv,na.rm = TRUE))

#Summary stats, including appropriateness ratings, grouped by STEM Degree or not AND by scenario
df.StemDegreeSummary.All <- df.SciLit %>%
  group_by(Scenario,policyOrAB,STEMdegree) %>% 
  dplyr::summarise(N=n(), Mean=mean(dv), Std=sd(dv),StdErr = std.error(dv,na.rm = TRUE))


#Preregistered analysis: correlation between SciLit and appropriateness rating grouped by condition.
sciCorrs <- df.SciLit %>% group_by(policyOrAB) %>% dplyr::summarise(Correlation = cor(dv,ScienceLitSummed, use = "complete"),n = n())
sciCorrs <- as_vector(sciCorrs[,2]) #Make into a vector to attach to table

# sciCorrs.All <- df.SciLit %>% group_by(Scenario,policyOrAB) %>% summarise(Correlation = cor(dv,ScienceLitSummed, use = "complete"),n = n()) #alternative grouped by scenario for a more detailed look.

# df.SciLitSummary <- add_column(df.SciLitSummary, corr = sciCorrs, .after = "StdErr") #Add columns of percentages to table of summary stats

#Write output table
write_excel_csv(df.SciLitSummary,"Output/SciLitDescriptives.csv") #export to csv
write_excel_csv(df.StemDegreeSummary,"Output/StemDegreeDescriptives.csv") #export to csv
write_excel_csv(df.StemDegreeSummary.All,"Output/StemDegreeDescriptivesOverAll.csv") #export to csv
# write_excel_csv(sciCorrs.All,"Output/SciLitCorrs.csv") #export to csv

#Check scale reliability
sciScale <- data.frame(df.SciLit$PlantFood_Correct, df.SciLit$GenesAnswer_Correct, df.SciLit$Earth_Correct, df.SciLit$Radioact_Correct, df.SciLit$Lasers_Correct, df.SciLit$Electrons_Correct, df.SciLit$Continents_Correct, df.SciLit$Gender_Correct, df.SciLit$Antibiotics_Correct) #make dataframe of the three variables
reliability <- alpha(sciScale) #check reliability

#t.tests for SciLit differences between conditions.
# t.test(filter(df.SciLit, genericCond == "A")$ScienceLitSummed,filter(df.SciLit, genericCond == "B")$ScienceLitSummed, var.equal = TRUE) #A against B #reported in results
# t.test(filter(df.SciLit, policyOrAB == "Policy")$ScienceLitSummed,filter(df.SciLit, policyOrAB == "AB Test")$ScienceLitSummed, var.equal = TRUE) #A or B against A/B
# t.test(filter(df.SciLit, genericCond == "A")$ScienceLitSummed,filter(df.SciLit, genericCond == "AB")$ScienceLitSummed, var.equal = TRUE) #A against A/B
# t.test(filter(df.SciLit, genericCond == "B")$ScienceLitSummed,filter(df.SciLit, genericCond == "AB")$ScienceLitSummed, var.equal = TRUE) #B against A/B

#############################################################
#Additional preregistered analyses (regressing appropriateness rating on science literacy score within each condition)

modelA <- lm(dv ~ ScienceLitSummed, data = filter(df.SciLit,df.SciLit$genericCond == "A")) #regress appropriateness on SciLit in all A conditions
modelAsummary <- summary(modelA)

modelB <- lm(dv ~ ScienceLitSummed, data = filter(df.SciLit,df.SciLit$genericCond == "B")) #regress appropriateness on SciLit in all B conditions
modelBsummary <- summary(modelB)

modelAB <- lm(dv ~ ScienceLitSummed, data = filter(df.SciLit,df.SciLit$genericCond == "AB")) #regress appropriateness on SciLit in all A/B conditions
modelABsummary <- summary(modelAB)