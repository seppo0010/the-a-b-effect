rm(list = ls())
library(tidyverse)
library(readxl)
library(effsize)
library(gridExtra)
library(psy)
library(scales)
library(Rmisc)
library(knitr)
library(kableExtra)
library(broom)
library(reshape)
library(SDMTools)
library(xtable)
library(lm.beta)
library(psych)
library(irr)
setwd("~/Documents/Projects/ABI/Free Response Coding/Finalized Coding/Checklist/Exp 3")
ABE3 <- read.csv("Study 2a Free Response Codes.csv")

##Combine Inequality and No Equipoise
ABE3 <- within (ABE3, Inequality <- ifelse (InequalityFinal==1 | NoEquipoiseFinal==1, 1, 0))

##Codes by condition 
df.Inequality.summary <- summarySE(ABE3, measurevar = "Inequality", groupvars = "condLabel")

df.Consent.summary <- summarySE(ABE3, measurevar = "ConsentFinal", groupvars = "condLabel")

df.Experimentation.summary <- summarySE(ABE3, measurevar = "ExperimentationFinal", groupvars = "condLabel")

df.Randomization.summary <- summarySE(ABE3, measurevar = "RandomizationFinal", groupvars = "condLabel")

df.Codes.summary <- cbind (df.Inequality.summary, df.Consent.summary, df.Experimentation.summary, df.Randomization.summary)
df.Codes.summaryVertical <- t(df.Codes.summary)

df.Codes.summary <- df.Codes.summary[,c("condLabel", "Inequality", "ConsentFinal","ExperimentationFinal", "RandomizationFinal")]

##Change Proportions to Percentages 
df.Codes.summaryPercent <- df.Codes.summary[,2:5]*100

##Add condition column, Rename and Re-order Columns and Factor Levels 
colnames(df.Codes.summaryPercent) <- c("Inequality", "Consent", "Experimentation","Randomization")
df.Codes.summaryPercent$condLabel <- df.Codes.summary$condLabel
df.Codes.summaryPercent <- df.Codes.summaryPercent[, c(5,1,2,3,4)]
df.Codes.summaryPercent$condLabel <- mapvalues (df.Codes.summaryPercent$condLabel, from = c("badge", "poster", "bp_long", "bp_short"), to = c("Badge", "Poster", "AB Long", "AB Short"))
df.Codes.summaryPercent$condLabel <- factor(df.Codes.summaryPercent$condLabel, levels = c("Badge", "Poster", "AB Long", "AB Short"))
df.Codes.summaryPercent <- arrange(df.Codes.summaryPercent,df.Codes.summaryPercent$condLabel)

##Transpose Table
df.CodesVertical <- setNames(data.frame(t(df.Codes.summaryPercent[,-1])),df.Codes.summaryPercent[,1])
df.CodesVertical <- cbind ("Condition" = rownames(df.CodesVertical), df.CodesVertical)
rownames(df.CodesVertical) <- NULL

##Create CSV
write_excel_csv (df.CodesVertical, "Exp3Table.csv")