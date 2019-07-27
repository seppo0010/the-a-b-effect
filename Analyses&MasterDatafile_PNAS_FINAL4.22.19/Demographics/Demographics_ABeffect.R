#The A/B Effect: Objecting to experiments that compare two unobjectionable policies or treatments
#Demographic Analyses Grouped by Policy or Experiment Conditions: All Experiments

rm(list = ls())

# #Load packages
library(tidyverse) #for ggplot and filter
library(readxl) #for read
library(effsize) #for cohens d
library(psy) #for alpha
library(psych) 
library(Rmisc) #for summary stats
library(knitr)
library(kableExtra)
library(broom)
library(plotrix) #for standard error function

#read in the desired worksheet 
df.Demos <- read_excel("ABI_MasterDatafile.xlsx", sheet = "FullExperimentsOnly")
df.Demos <- filter(df.Demos,RepeatTurkerLabelCode != "1") #Exclude those with 1: participants anywhere in the dataset who took our very first survey AND another survey
df.Demos <- filter(df.Demos,StudyDescription != "ProviderSurvey") #exclude healthcare participants from this analysis (we did not collect demographics)

#Keep only useful columns and only the full experiment data
df.Demos <- df.Demos[,c(1:59)] #remove hanging columns

#get differences between conditions split by demographics.                                                                                    
df.Sex <-       df.Demos %>% group_by(policyOrAB,SexBinary1Male) %>% dplyr::summarise(Average = mean(dv,na.rm = TRUE), n = n(),StdDev = sd(dv,na.rm = TRUE), StdErr = std.error(dv,na.rm = TRUE)) #over experiments
df.Race <-      df.Demos %>% group_by(policyOrAB,RaceBinary1White) %>% dplyr::summarise(Average = mean(dv,na.rm = TRUE), n = n(),StdDev = sd(dv,na.rm = TRUE), StdErr = std.error(dv,na.rm = TRUE))
df.Education <- df.Demos %>% group_by(policyOrAB,EducationBinary1Degree) %>% dplyr::summarise(Average = mean(dv,na.rm = TRUE), n = n(),StdDev = sd(dv,na.rm = TRUE), StdErr = std.error(dv,na.rm = TRUE))
df.Age <-       df.Demos %>% group_by(policyOrAB,AgeBinary1Old) %>% dplyr::summarise(Average = mean(dv,na.rm = TRUE), n = n(),StdDev = sd(dv,na.rm = TRUE), StdErr = std.error(dv,na.rm = TRUE))
df.Income <-    df.Demos %>% group_by(policyOrAB,IncomeBinary1HighInc) %>% dplyr::summarise(Average = mean(dv,na.rm = TRUE), n = n(),StdDev = sd(dv,na.rm = TRUE), StdErr = std.error(dv,na.rm = TRUE))

#can comment in to see demographic differences split by experiment.
# df.Sex <-       df.Demos %>% group_by(Scenario,policyOrAB,SexBinary1Male) %>% dplyr::summarise(Average = mean(dv,na.rm = TRUE), n = n(),StdDev = sd(dv,na.rm = TRUE), StdErr = std.error(dv,na.rm = TRUE)) #by experiment
# df.Race <-      df.Demos %>% group_by(Scenario,policyOrAB,RaceBinary1White) %>% dplyr::summarise(Average = mean(dv,na.rm = TRUE), n = n(),StdDev = sd(dv,na.rm = TRUE), StdErr = std.error(dv,na.rm = TRUE))
# df.Education <- df.Demos %>% group_by(Scenario,policyOrAB,EducationBinary1Degree) %>% dplyr::summarise(Average = mean(dv,na.rm = TRUE), n = n(),StdDev = sd(dv,na.rm = TRUE), StdErr = std.error(dv,na.rm = TRUE))
# df.Age <-       df.Demos %>% group_by(Scenario,policyOrAB,AgeBinary1Old) %>% dplyr::summarise(Average = mean(dv,na.rm = TRUE), n = n(),StdDev = sd(dv,na.rm = TRUE), StdErr = std.error(dv,na.rm = TRUE))
# df.Income <-    df.Demos %>% group_by(Scenario,policyOrAB,IncomeBinary1HighInc) %>% dplyr::summarise(Average = mean(dv,na.rm = TRUE), n = n(),StdDev = sd(dv,na.rm = TRUE), StdErr = std.error(dv,na.rm = TRUE))
