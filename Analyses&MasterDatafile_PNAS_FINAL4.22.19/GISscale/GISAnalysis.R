#The A/B Effect: Objecting to experiments that compare two unobjectionable policies or treatments
#God, Intuition, and Science Scale Analyses: All Experiments

rm(list = ls())

# #Load packages
library(tidyverse) #for ggplot and filter
library(readxl) #for read
library(effsize) #for cohens d
library(gridExtra) #for multi plots
library(psy) #for alpha
# library(psych) #for alpha
# library(scales) #for colors
library(Rmisc) #for summary stats
library(knitr)
# library(kableExtra)
library(broom)
library(plotrix) #for standard error function
# detach(package:plyr)
# options(digits=3) #set to 3 for table outputs
# 
#read in the desired worksheet 
df.Relig <- read_excel("ABI_MasterDatafile.xlsx", sheet = "FullExperimentsOnly")
df.Relig <- filter(df.Relig,RepeatTurkerLabelCode != "1") #Exclude those with 1: participants anywhere in the dataset who took our very first survey AND another survey

#don't need to exclude scenarios/studies because the code only uses complete observations.

#correlations between items
cor(df.Relig$God,df.Relig$Intuition,use = "complete")
cor(df.Relig$God,df.Relig$ScienceBelief,use = "complete")
cor(df.Relig$Intuition,df.Relig$ScienceBelief,use = "complete")

intScale <- data.frame(df.Relig$God,df.Relig$Intuition,df.Relig$ScienceBelief_Reverse) #make dataframe of the three variables, including the reverse-scored science question.

psych::alpha(intScale)

#Mean, Median, SD for Intuition Scale
df.Relig$genericCond = factor(df.Relig$genericCond, levels = c("A","B","AB"))
df.Relig$policyOrAB = factor(df.Relig$policyOrAB, levels = c("Policy","AB Test"))

df.Intuition <- df.Relig %>% group_by(policyOrAB) %>% dplyr::summarise(n = n(), Mean = mean(IntuitionScore_ByHand, na.rm = TRUE), Median = median(IntuitionScore_ByHand, na.rm = TRUE),sd=sd(IntuitionScore_ByHand,na.rm = TRUE),StdErr = std.error(IntuitionScore_ByHand,na.rm = TRUE))
write_excel_csv(df.Intuition,"Output/GISDescriptives.csv") #export to csv for formatting in Excel

# To see results by A, B, and AB, comment in line below:
# df.Intuition <- df.Relig %>% group_by(genericCond) %>% dplyr::summarise(n = n(), Mean = mean(IntuitionScore_ByHand, na.rm = TRUE), Median = median(IntuitionScore_ByHand, na.rm = TRUE),sd=sd(IntuitionScore_ByHand,na.rm = TRUE),StdErr = std.error(IntuitionScore_ByHand,na.rm = TRUE))

t.test(filter(df.Relig, policyOrAB == "Policy")$IntuitionScore_ByHand,filter(df.Relig, policyOrAB == "AB Test")$IntuitionScore_ByHand, var.equal = TRUE) #test for difference in scale scores
cohen.d(filter(df.Relig, policyOrAB == "Policy")$IntuitionScore_ByHand,filter(df.Relig, policyOrAB == "AB Test")$IntuitionScore_ByHand, var.equal = TRUE) #effect size for difference in scale scores

t.test(filter(df.Relig, policyOrAB == "Policy")$God,filter(df.Relig, policyOrAB == "AB Test")$God, var.equal = TRUE) #test for difference in scale scores
t.test(filter(df.Relig, policyOrAB == "Policy")$Intuition,filter(df.Relig, policyOrAB == "AB Test")$Intuition, var.equal = TRUE) #test for difference in scale scores
t.test(filter(df.Relig, policyOrAB == "Policy")$ScienceBelief_Reverse,filter(df.Relig, policyOrAB == "AB Test")$ScienceBelief_Reverse, var.equal = TRUE) #test for difference in scale scores

# #Correlations between scale and appropriateness ratings
# ScaleCorrs <- df %>% group_by(policyOrAB) %>% summarise(Correlation = cor(dv,IntuitionScore_ByHand, use = "complete"),n = n())
# GodCorrs <- df %>% group_by(policyOrAB) %>% summarise(Correlation = cor(dv,God, use = "complete"),n = n())
# IntCorrs <- df %>% group_by(policyOrAB) %>% summarise(Correlation = cor(dv,Intuition, use = "complete"),n = n())
# ScienceCorrs <- df %>% group_by(policyOrAB) %>% summarise(Correlation = cor(dv,ScienceBelief, use = "complete"),n = n())

#Preregistered analyses
modelA <- lm(dv ~ IntuitionScore_ByHand, data = filter(df.Relig,df.Relig$genericCond == "A")) #regress appropriateness on GIS Scale in all A conditions
modelAsummary <- summary(modelA)

modelB <- lm(dv ~ IntuitionScore_ByHand, data = filter(df.Relig,df.Relig$genericCond == "B")) #regress appropriateness on GIS Scale in all B conditions
modelBsummary <- summary(modelB)

modelAB <- lm(dv ~ IntuitionScore_ByHand, data = filter(df.Relig,df.Relig$genericCond == "AB")) #regress appropriateness on GIS Scale in all A/B conditions
modelABsummary <- summary(modelAB)

modelPooledPolicy <- lm(dv ~ IntuitionScore_ByHand, data = filter(df.Relig,df.Relig$policyOrAB == "Policy")) #regress appropriateness on GIS Scale in all Policy conditions
modelPooledPolicysummary <- summary(modelPooledPolicy)