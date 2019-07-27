#Objecting to experiments that compare two unobjectionable policies

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
df <- read_excel("ABI_MasterDatafile.xlsx", sheet = "FullExperimentsOnly")
df <- filter(df,RepeatTurkerLabelCode != "1") #Exclude those with 1: participants anywhere in the dataset who took our very first survey AND another survey

df.Length.summary <- summarySE(df, measurevar = "FreeResponseLength", groupvars = "policyOrAB")

t.test(filter(df,df$policyOrAB == "Policy")$FreeResponseLength, filter(df,df$policyOrAB == "AB Test")$FreeResponseLength, var.equal = TRUE)
cohen.d(filter(df,df$policyOrAB == "Policy")$FreeResponseLength, filter(df,df$policyOrAB == "AB Test")$FreeResponseLength)