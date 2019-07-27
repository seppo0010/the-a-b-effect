#The A/B Effect: Objecting to experiments that compare two unobjectionable policies or treatments
#Omnibus Analyses: Experiments 1-5

rm(list = ls()) #clear workspace

#Load packages
library(tidyverse) #for ggplot and filter
library(readxl) #for read
library(effsize) #for cohens d
library(gridExtra) #for multi plots
library(psy) #for alpha
library(scales) #for colors
library(Rmisc) #for summary stats
library(knitr) #for tabling in R
library(kableExtra) #for tabling in R
library(broom) #for tabling in R

#read in the desired worksheet 
df.Omni <- read_excel("ABI_MasterDatafile.xlsx", sheet = "FullExperimentsOnly")
df.Omni <- filter(df.Omni,RepeatTurkerLabelCode != "1") #Exclude those with 1: participants anywhere in the dataset who took our very first survey AND another survey
df.Omni <- filter(df.Omni,StudyDescription != "ProviderSurvey") #exclude healthcare participants from this analysis as per SI p. 89

# Keep only useful columns (here, grouping variables and the dv)
df.Omni <- df.Omni[,c(1:14)]

df.Omni$genericCond = factor(df.Omni$policyOrAB, levels = c("Policy", "AB Test")) #order the factor levels

df.Omni.summary = summarySE(df.Omni, 'dv', groupvars = c("policyOrAB")) #summarize mean ratings of policies vs. AB tests

df.Omni.SummaryPercent = summarySE(df.Omni, measurevar = "dv_binary", groupvars = c("policyOrAB")) #Summarize proportion objecting
df.Omni.SummaryPercent <- df.Omni.SummaryPercent %>% mutate(dv_binary = dv_binary*100) #turn into percentages

df.Omni.Summary <- add_column(df.Omni.summary, percentObj = df.Omni.SummaryPercent[,3], .after = "N") #Add columns of percentages to table of summary stats
df.Omni.Summary <- rename(df.Omni.Summary,c("policyOrAB"="Condition","percentObj"="% Objecting", "dv"="M Appropriateness","sd"="SD","se"="SEM","ci"="95% CI +/-")) #rename table headers for export
write_excel_csv(df.Omni.Summary,"Output/OmnibusDescriptives.csv") #export to csv

cohen.d(filter(df.Omni, policyOrAB == 'Policy')$dv,filter(df.Omni, policyOrAB == 'AB Test')$dv) #Overall effect size estimate