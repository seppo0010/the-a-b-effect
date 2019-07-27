#The A/B Effect: Objecting to experiments that compare two unobjectionable policies or treatments
#Demographic Analyses: All Experiments

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

#Keep only useful columns
df.Demos <- df.Demos[,c(1:59)] #remove hanging columns

df.Demos$policyOrAB = factor(df.Demos$policyOrAB, levels = c("Policy","AB Test"))
df.Demos$Scenario = factor(df.Demos$Scenario, levels = c("Checklist",
                                                         "Checklist_exact",
                                                         "Checklist_vary",
                                                         "Checklist_pf",
                                                         "Genetic Testing",
                                                         "Autonomous Vehicles",
                                                         "Retirement",
                                                         "Recruit",
                                                         "Charity",
                                                         "Education",
                                                         "Basic Income",
                                                         "Drug",
                                                         "DrugTeach",
                                                         "DrugTeach_pf"))

#get table of demographics split by condition and by sample. Multiply binary variables by 100 to convert to percentage.
df.Samples <-   df.Demos %>% group_by(Scenario,policyOrAB) %>% dplyr::summarise(n = n(),
                                                                                Sex = mean(SexBinary1Male,na.rm = TRUE)*100, 
                                                                                White = mean(RaceBinary1White,na.rm = TRUE)*100, 
                                                                                Degree = mean(EducationBinary1Degree,na.rm = TRUE)*100,
                                                                                Income = mean(IncomeBinary1HighInc,na.rm = TRUE)*100,
                                                                                MeanAge = mean(Age,na.rm = TRUE),
                                                                                StdAge = sd(Age,na.rm = TRUE))

write_excel_csv(df.Samples,"Output/DemographicDescriptives.csv") #export to csv