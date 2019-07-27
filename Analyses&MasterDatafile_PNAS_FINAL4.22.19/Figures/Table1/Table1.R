#Objecting to experiments that compare two unobjectionable policies
rm(list = ls())

#Load packages used in the AB Illusion project
library(tidyverse) #for ggplot and filter
library(readxl) #for read
library(effsize) #for cohens d
library(gridExtra) #for multi plots
library(psy) #for alpha
library(scales) #for colors
library(Rmisc) #for summary stats
library(knitr)
library(kableExtra)
library(broom)

#read in the desired worksheet 
df <- read_excel("ABI_MasterDatafile.xlsx", sheet = "FullExperimentsOnly")
df <- filter(df,RepeatTurkerLabelCode != "1") #Exclude those with 1: participants anywhere in the dataset who took our very first survey AND another survey
#Keep only useful columns (here, grouping variables and the dv)
df <- df[,c(1:14)]

df.Domains <- filter(df, df$Scenario == "Autonomous Vehicles"
                       | df$Scenario == "Genetic Testing"
                       | df$Scenario == "Basic Income" 
                       | df$Scenario == "Charity"
                       | df$Scenario == "Education"
                       | df$Scenario == "Recruit"
                       | df$Scenario == "Retirement") 

df.Domains$genericCond = factor(df.Domains$genericCond, levels = c("A", "B", "AB"))

summaryTable = summarySE(df.Domains, 'dv', groupvars = c("Scenario", "genericCond"))
summaryTable$Scenario = factor(summaryTable$Scenario, levels = c("Genetic Testing", "Autonomous Vehicles", "Retirement", "Recruit","Charity","Education","Basic Income"))
summaryTable = arrange(summaryTable,summaryTable$Scenario)

df.SummaryPercent = summarySE(df.Domains, measurevar = "dv_binary", groupvars = c("Scenario", "genericCond")) #Summarize proportion objecting
df.SummaryPercent$Scenario = factor(df.SummaryPercent$Scenario, levels = c("Genetic Testing", "Autonomous Vehicles", "Retirement", "Recruit","Charity","Education","Basic Income"))
df.SummaryPercent = arrange(df.SummaryPercent,df.SummaryPercent$Scenario)
df.SummaryPercent <- df.SummaryPercent %>% mutate(dv_binary = dv_binary*100) #turn into percentages

summaryTable <- add_column(summaryTable, percentObj = df.SummaryPercent[,4], .after = "N") #Add columns of percentages to table of summary stats
summaryTable <- rename(summaryTable,c("genericCond"="Condition","percentObj"="% Objecting", "dv"="M Appropriateness","sd"="SD","se"="SEM","ci"="95% CI +/-")) #rename table headers for export
write_excel_csv(summaryTable,"Output/Figure2Means.csv") #export to csv for formatting in Excel

PvE.AV  <- tidy((t.test(filter(df.Domains, Scenario == "Genetic Testing" & policyOrAB == 'Policy')$dv,    filter(df.Domains, Scenario == "Genetic Testing" &  policyOrAB == 'AB Test')$dv, var.equal = TRUE))) 
PvE.GT  <- tidy((t.test(filter(df.Domains, Scenario == "Autonomous Vehicles" & policyOrAB == 'Policy')$dv,filter(df.Domains, Scenario == "Autonomous Vehicles" &  policyOrAB == 'AB Test')$dv, var.equal = TRUE))) 
PvE.Ret <- tidy((t.test(filter(df.Domains, Scenario == "Retirement" & policyOrAB == 'Policy')$dv,         filter(df.Domains, Scenario == "Retirement" &  policyOrAB == 'AB Test')$dv, var.equal = TRUE))) 
PvE.Rec <- tidy((t.test(filter(df.Domains, Scenario == "Recruit" & policyOrAB == 'Policy')$dv,            filter(df.Domains, Scenario == "Recruit" &  policyOrAB == 'AB Test')$dv, var.equal = TRUE))) 
PvE.Cha <- tidy((t.test(filter(df.Domains, Scenario == "Charity" & policyOrAB == 'Policy')$dv,            filter(df.Domains, Scenario == "Charity" &  policyOrAB == 'AB Test')$dv, var.equal = TRUE))) 
PvE.Edu <- tidy((t.test(filter(df.Domains, Scenario == "Education" & policyOrAB == 'Policy')$dv,          filter(df.Domains, Scenario == "Education" &  policyOrAB == 'AB Test')$dv, var.equal = TRUE))) 
PvE.Inc <- tidy((t.test(filter(df.Domains, Scenario == "Basic Income" & policyOrAB == 'Policy')$dv,       filter(df.Domains, Scenario == "Basic Income" &  policyOrAB == 'AB Test')$dv, var.equal = TRUE)))

df.Domains.ttests <- t(do.call(rbind, Map(data.frame, PvE.AV=PvE.AV, PvE.GT=PvE.GT, PvE.Ret=PvE.Ret, PvE.Rec=PvE.Rec, PvE.Cha=PvE.Cha,PvE.Edu=PvE.Edu,PvE.Inc=PvE.Inc)))

#Include only first five columns
df.Domains.ttests <- df.Domains.ttests[ , c(3:5) ]

#Add cohen's d for the comparison to the end of this table
effSizes <- data.frame(unname(c(
  cohen.d(filter(df.Domains, Scenario == "Genetic Testing" & policyOrAB == 'Policy')$dv,                  filter(df.Domains, Scenario == "Genetic Testing" &  policyOrAB == 'AB Test')$dv)[[3]],  
  cohen.d(filter(df.Domains, Scenario == "Autonomous Vehicles" & policyOrAB == 'Policy')$dv,              filter(df.Domains, Scenario == "Autonomous Vehicles" &  policyOrAB == 'AB Test')$dv)[[3]],  
  cohen.d(filter(df.Domains, Scenario == "Retirement" & policyOrAB == 'Policy')$dv,                       filter(df.Domains, Scenario == "Retirement" &  policyOrAB == 'AB Test')$dv)[[3]],  
  cohen.d(filter(df.Domains, Scenario == "Recruit" & policyOrAB == 'Policy')$dv,                          filter(df.Domains, Scenario == "Recruit" &  policyOrAB == 'AB Test')$dv)[[3]],  
  cohen.d(filter(df.Domains, Scenario == "Charity" & policyOrAB == 'Policy')$dv,                          filter(df.Domains, Scenario == "Charity" &  policyOrAB == 'AB Test')$dv)[[3]],  
  cohen.d(filter(df.Domains, Scenario == "Education" & policyOrAB == 'Policy')$dv,                        filter(df.Domains, Scenario == "Education" &  policyOrAB == 'AB Test')$dv)[[3]],  
  cohen.d(filter(df.Domains, Scenario == "Basic Income" & policyOrAB == 'Policy')$dv,                     filter(df.Domains, Scenario == "Basic Income" &  policyOrAB == 'AB Test')$dv)[[3]]))) 
  
df.Domains.ttests <- cbind(df.Domains.ttests,effSizes) #bind column of cohen's d to the t-test table

colnames(df.Domains.ttests) <- c("t", "p", "df","d")
df.Domains.ttests <- df.Domains.ttests[, c("t","df","p","d")] #Reorder columns so that df comes right after t
write_excel_csv(df.Domains.ttests,"Output/Figure2Tests.csv") #export to csv for formatting in Excel