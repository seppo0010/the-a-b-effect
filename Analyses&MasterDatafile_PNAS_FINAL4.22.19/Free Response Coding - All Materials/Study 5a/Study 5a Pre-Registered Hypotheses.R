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
# library(SDMTools)
library(xtable)
library(lm.beta)
library(psych)
library(irr)
BDT <- read.csv("Study 5a Free Response Codes.csv") #BDT = Better Drug Teaching

##Create approval group 
BDT <- BDT %>%
  mutate(dv_approve = ifelse(dv > 3, 1, 0))

##Pre-Registered Hypothesis 1 - Benefit most common code among participants who approved of decision

##Reshape data
BDT <- BDT %>%
  gather(category, code, BenefitFinal:PatientAlreadyShouldKnowFinal)

##Rank codes
df.CodeByApproval <- summarySE(BDT, measurevar = "code", groupvars = c("category", "dv_approve"))
df.CodeByApproval <- arrange(df.CodeByApproval, desc(df.CodeByApproval$dv_approve), desc(df.CodeByApproval$code))
df.CodeByApproval <- df.CodeByApproval[,c("category","dv_approve","N", "code")]

##Pre-Registered Hypothesis 2 - Ineffective most common code among participants in Policy conditiosn who objected to decision

##Rank codes
df.CodeByObjection <- summarySE(BDT, measurevar = "code", groupvars = c("category", "Objection", "policyOrAB"))
df.CodeByObjection <- arrange(df.CodeByObjection, desc(df.CodeByObjection$Objection), df.CodeByObjection$policyOrAB, desc(df.CodeByObjection$code))
df.CodeByObjection <- df.CodeByObjection[,c("category","Objection", "policyOrAB", "N", "code")]

##Pre-Registered Hypothesis 3 - Research most common code among participants in AB conditions who objected to decision

##Reset data
BDT <- read.csv("Study 5a Free Response Codes.csv")

##Combine Benefit, Learning, Absence Harm, Equality
BDT <- within(BDT, Benefit <- ifelse (BenefitFinal==1 | LearningFinal==1 | AbsenceHarmFinal==1 | EqualityFinal==1, 1, 0)) 

##Combine Physical Risk, Other Risk, Ineffective
BDT <- within(BDT, Harm <- ifelse (PhysicalRiskFinal==1 | OtherRiskFinal==1 | IneffectiveFinal==1, 1, 0))

##Combine Randomization, Experimentation, Inequality, No Equipoise 

BDT <- within (BDT, Research <- ifelse (RandomizationFinal==1 | ExperimentationFinal==1 | InequalityFinal==1 | NoEquipoiseFinal==1, 1, 0))

##Combine Inequality and No Equipoise

BDT <- within (BDT, Inequality <- ifelse (InequalityFinal==1 | NoEquipoiseFinal==1, 1, 0))

##Combine Consent and Notice
BDT <- within (BDT, Consent <- ifelse (ConsentFinal==1 | NoticeFinal==1, 1, 0))

##Combine Pick a Policy and Infallibility
BDT <- within (BDT, AllInfallibility <- ifelse (PickAPolicyFinal==1 | PatientConfidenceFinal==1 | CoddledExpertsFinal==1 | AgentsAlreadyKnowFinal==1 | AgentsShouldAlreadyKnowFinal==1 | ExpertsAlreadyKnowFinal==1 | ExpertsShouldAlreadyKnowFinal==1 | ParticipantKnowsBestFinal==1 | PatientAlreadyShouldKnowFinal == 1 | WorkHasAlreadyBeenDoneFinal==1, 1, 0))

##Combine Pick A Policy Other Infallibility
BDT <- within (BDT, OtherInfallibility <- ifelse (PickAPolicyFinal==1 | PatientConfidenceFinal==1 | CoddledExpertsFinal==1 | AgentsAlreadyKnowFinal==1 | AgentsShouldAlreadyKnowFinal==1 | ExpertsAlreadyKnowFinal==1 | ExpertsShouldAlreadyKnowFinal==1, 1, 0))

##Reshape data
BDT <- BDT %>%
  gather(category, code, Benefit:OtherInfallibility)

##Rank codes
df.CombinedCodeByObjection <- summarySE(BDT, measurevar = "code", groupvars = c("category", "Objection", "policyOrAB"))
df.CombinedCodeByObjection <- arrange(df.CombinedCodeByObjection, desc(df.CombinedCodeByObjection$Objection), df.CombinedCodeByObjection$policyOrAB, desc(df.CombinedCodeByObjection$code))
df.CombinedCodeByObjection <- df.CombinedCodeByObjection[,c("category","Objection", "policyOrAB", "N", "code")]

##Pre-Registered Hypothesis 4 - Chi-Squared Test for Consent between Policy and AB conditions

##Reset data
BDT <- read.csv("Study 5a Free Response Codes.csv")

##Chi-Square Test
df.ConsentByCondition <- table(BDT$policyOrAB, BDT$ConsentFinal)
prop.test(x = c(df.ConsentByCondition[1,1],df.ConsentByCondition[1,2]),n = c((df.ConsentByCondition[1,1]+df.ConsentByCondition[2,1]), (df.ConsentByCondition[1,2]+df.ConsentByCondition[2,2])))

prop.test(x = c(df.ConsentByCondition[1,2],df.ConsentByCondition[2,2]),n = c((df.ConsentByCondition[1,1]+df.ConsentByCondition[1,2]), (df.ConsentByCondition[2,1]+df.ConsentByCondition[2,2])))
