rm(list = ls())
library(tidyverse)
library(psych)
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
# library(SDMTools)
library(xtable)
library(lm.beta)
library(irr)
ABE1 <- read.csv("Study 1 Free Response Codes.csv")

##Filter exclusions
ABE1 <-ABE1[!(ABE1$Exclusion > 0),]

##Combine Inequality and No Equipoise
ABE1 <- within (ABE1, Inequality <- ifelse (InequalityFinal==1 | NoEquipoiseFinal==1, 1, 0))

##Codes by condition 
df.Inequality.summary <- summarySE(ABE1, measurevar = "Inequality", groupvars = "Condition")

df.Consent.summary <- summarySE(ABE1, measurevar = "ConsentFinal", groupvars = "Condition")

df.Experimentation.summary <- summarySE(ABE1, measurevar = "ExperimentationFinal", groupvars = "Condition")

df.Randomization.summary <- summarySE(ABE1, measurevar = "RandomizationFinal", groupvars = "Condition")

df.Codes.summary <- cbind (df.Inequality.summary, df.Consent.summary, df.Experimentation.summary, df.Randomization.summary)
df.Codes.summaryVertical <- t(df.Codes.summary)

df.Codes.summary <- df.Codes.summary[,c("Condition", "Inequality", "ConsentFinal","ExperimentationFinal", "RandomizationFinal")]

##Change Proportions to Percentages 
df.Codes.summaryPercent <- df.Codes.summary[,2:5]*100

##Add condition column, Rename and Re-order Columns and Factor Levels 
colnames(df.Codes.summaryPercent) <- c("Inequality", "Consent", "Experimentation","Randomization")
df.Codes.summaryPercent$Condition <- df.Codes.summary$Condition
df.Codes.summaryPercent <- df.Codes.summaryPercent[, c(5,1,2,3,4)]
df.Codes.summaryPercent$Condition <- mapvalues(df.Codes.summaryPercent$Condition, from = c("badge", "poster", "xbpl", "xbps"), to = c("Badge", "Poster", "AB Long", "AB Short"))

##Transpose Table
df.CodesVertical <- setNames(data.frame(t(df.Codes.summaryPercent[,-1])),df.Codes.summaryPercent[,1])
df.CodesVertical <- cbind ("Condition" = rownames(df.CodesVertical), df.CodesVertical)
rownames(df.CodesVertical) <- NULL

##Create CSV
# write_excel_csv(df.CodesVertical, "Exp1Table.csv") #comment in and run to create a csv with final table
