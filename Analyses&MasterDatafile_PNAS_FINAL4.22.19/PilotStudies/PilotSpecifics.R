#The A/B Effect: Objecting to experiments that compare two unobjectionable policies or treatments
#Specific analyses on pretest and pilot studies

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
df.Pilot <- read_excel("ABI_MasterDatafile.xlsx", sheet = "CompiledData")
df.Pilot <- filter(df.Pilot,StudyType == "Pilot") #Only keep participants from pilot studies
df.Pilot <- filter(df.Pilot,RepeatTurkerLabelCode != "1") #Exclude those with 1: participants anywhere in the dataset who took our very first survey AND another survey

# Keep only useful columns (here, grouping variables and the dv)
df.Pilot <- df.Pilot[,c(1:14)]

df.Pilot$genericCond = factor(df.Pilot$genericCond, levels = c("A", "B", "AB"))

df.Pilot.Summary = summarySE(df.Pilot, 'dv', groupvars = c("Scenario", "genericCond", "`R File`")) #groups into A, B, and AB
# df.Pilot.SummaryLabeled = summarySE(df.Pilot, 'dv', groupvars = c("Scenario", "scenarioCond")) #Includes shorthand keywords

write_excel_csv(df.Pilot.Summary,"Output/PilotSpecifics.csv") #export to csv