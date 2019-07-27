#The A/B Effect: Objecting to experiments that compare two unobjectionable policies or treatments
#Summary analyses on pretest and pilot studies

rm(list = ls()) #clear workspace

#Load packages
library(tidyverse) #for ggplot and filter
library(readxl) #for read
library(effsize) #for cohens d
library(gridExtra) #for multi plots
# library(psy) #for alpha
# library(scales) #for colors
library(Rmisc) #for summary stats
library(knitr) #for tabling in R
library(kableExtra) #for tabling in R
library(broom) #for tabling in R

#read in the desired worksheet 
df.Pilot <- read_excel("ABI_MasterDatafile.xlsx", sheet = "CompiledData")
df.Pilot <- filter(df.Pilot,StudyType == "Pilot") #Only keep participants from pilot studies
df.Pilot <- filter(df.Pilot,RepeatTurkerLabelCode != "1") #Exclude those with 1: participants anywhere in the dataset who took our very first survey AND another survey

# Keep only useful columns (here, grouping variables and the dv)
df.Pilot <- df.Pilot[,c(1:14)]

df.Pilot$genericCond = factor(df.Pilot$policyOrAB, levels = c("Policy", "AB Test"))

# MetaTable = summarySE(df.Pilot, 'dv', groupvars = c("StudyType","genericCond"))
df.Pilot.summary = summarySE(df.Pilot, 'dv', groupvars = c("policyOrAB"))

df.Pilot.SummaryPercent = summarySE(df.Pilot, measurevar = "dv_binary", groupvars = c("policyOrAB")) #Summarize proportion objecting
df.Pilot.SummaryPercent <- df.Pilot.SummaryPercent %>% mutate(dv_binary = dv_binary*100) #turn into percentages

df.Pilot.Summary <- add_column(df.Pilot.summary, percentObj = df.Pilot.SummaryPercent[,3], .after = "N") #Add columns of percentages to table of summary stats
df.Pilot.Summary <- rename(df.Pilot.Summary,c("policyOrAB"="Condition","percentObj"="% Objecting", "dv"="M Appropriateness","sd"="SD","se"="SEM","ci"="95% CI +/-")) #rename table headers for export
write_excel_csv(df.Pilot.Summary,"Output/PilotSummary.csv") #export to csv

cohen.d(filter(df.Pilot, policyOrAB == 'Policy')$dv,filter(df.Pilot, policyOrAB == 'AB Test')$dv) #Overall effect size estimate
t.test(filter(df.Pilot, policyOrAB == 'Policy')$dv,filter(df.Pilot, policyOrAB == 'AB Test')$dv, var.equal = TRUE) #t-test for significance