#The A/B Effect: Objecting to experiments that compare two unobjectionable policies or treatments
#Study 2b: Safety Checklist  (MTurk sample on Qualtrics; replication of Study 1 with wording changes)

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
# df <- filter(df,MarkedForExclusion != "1") #Exclude those with 1: participants hand-coded for exclusion based on their free response #comment in to exclude

#For workspace, keep only useful columns (grouping variables, dv, and dv_binary)
df <- df[,c(1:14)] #only first 14 columns in data sheet
df.ChecklistVary <- filter(df, df$Scenario == "Checklist_vary") #pull out Study 2b only
df.ChecklistVary$condFinal <- factor(df.ChecklistVary$condFinal, levels = c("Badge (A)", "Poster (B)", "A/B", "A/B Learn")) #Reorder the factor for reporting in this order

# print(levels(df.Checklist$condFinal)) #to see factor levels if necessary

#Summary statistics of responses to each vignette
df.ChecklistVary.summary <- summarySE(df.ChecklistVary, measurevar="dv", groupvars="condFinal") #Summarize mean, sd, se, ci

df.SummaryPercent = summarySE(df.ChecklistVary, measurevar = "dv_binary", groupvars = c("Scenario", "condFinal")) #Summarize proportion objecting
df.SummaryPercent <- df.SummaryPercent %>% mutate(dv_binary = dv_binary*100) #turn into percentages

df.ChecklistVary.Summary <- add_column(df.ChecklistVary.summary, percentObj = df.SummaryPercent[,4], .after = "N") #Add columns of percentages to table of summary stats
df.ChecklistVary.Summary <- rename(df.ChecklistVary.Summary,c("condFinal"="Condition","percentObj"="% Objecting", "dv"="M Appropriateness","sd"="SD","se"="SEM","ci"="95% CI +/-")) #rename table headers for export
write_excel_csv(df.ChecklistVary.Summary,"Output/Study2bDescriptives.csv") #export to csv

# Histogram of all ratings for each condition
#create a labeller to give control over strip text in histogram
facet_names <- c(
  `Badge (A)` = "Badge (A)",
  `Poster (B)` = "Poster (B)",
  `A/B` = "A/B short",
  `A/B Learn` = "A/B learn"
)

ggplot(df.ChecklistVary) +
  geom_bar(mapping = aes(x = dv, y = ..prop..), fill = 'light blue', color = 'black', width = .75) +
  facet_wrap(~ condFinal,labeller = as_labeller(facet_names)) + #label strip text using the labeller
  theme(text = element_text(size=24)) +
  theme(axis.text = element_text(colour = 'black')) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  coord_cartesian(ylim=c(0,.6), expand = TRUE) +
  scale_y_continuous(breaks=seq(0, .6, .1), expand = c(0,0)) +
  ylab('Proportion of participants choosing each scale point') +
  xlab('Appropriateness')

# ggsave("Study2bHisto.png") #save png file

#t-tests
#Create dataframes of each set of t-test after tidying with broom::tidy
AvB <- tidy((t.test(filter(df.ChecklistVary, condFinal == 'Badge (A)')$dv,filter(df.ChecklistVary, condFinal == 'Poster (B)')$dv, var.equal = TRUE))) #A vs. B
ABSvABL <- tidy((t.test(filter(df.ChecklistVary, condFinal == 'A/B')$dv,filter(df.ChecklistVary, condFinal == 'A/B Learn')$dv, var.equal = TRUE))) #A/B Short vs. A/B Learn
AvAB <- tidy((t.test(filter(df.ChecklistVary, condFinal == 'Badge (A)')$dv,filter(df.ChecklistVary, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #A vs. A/B (collapsed together)
BvAB <- tidy((t.test(filter(df.ChecklistVary, condFinal == 'Poster (B)')$dv,filter(df.ChecklistVary, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #B vs. A/B (collapsed together)
PvE <- tidy((t.test(filter(df.ChecklistVary, policyOrAB == 'Policy')$dv,filter(df.ChecklistVary, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #Policy vs. Experiment: A & B (collapsed) vs. A/B (collapsed)

#Bind all t-tests into a single dataframe; transpose with t() function to right of <-
df.ChecklistVary.ttests <- t(do.call(rbind, Map(data.frame, AvB=AvB, ABshortVsABlearn=ABSvABL, AvAB=AvAB, BvAB=BvAB, PolicyVsExperiment=PvE)))

#Include only first five rows
df.ChecklistVary.ttests <- df.ChecklistVary.ttests[ , c(1:5) ]

#Add cohen's d for the comparison to the end of this table
effSizes <- data.frame(unname(c(
  cohen.d(filter(df.ChecklistVary, condFinal == 'Badge (A)')$dv,filter(df.ChecklistVary, condFinal == 'Poster (B)')$dv)[[3]],    #A vs. B
  cohen.d(filter(df.ChecklistVary, condFinal == 'A/B')$dv,filter(df.ChecklistVary, condFinal == 'A/B Learn')$dv)[[3]],           #A/B Short vs. A/B Learn
  cohen.d(filter(df.ChecklistVary, condFinal == 'Badge (A)')$dv,filter(df.ChecklistVary, policyOrAB == 'AB Test')$dv)[[3]],      #A vs. A/B (collapsed together)
  cohen.d(filter(df.ChecklistVary, condFinal == 'Poster (B)')$dv,filter(df.ChecklistVary, policyOrAB == 'AB Test')$dv)[[3]],     #B vs. A/B (collapsed together)
  cohen.d(filter(df.ChecklistVary, policyOrAB == 'Policy')$dv,filter(df.ChecklistVary, policyOrAB == 'AB Test')$dv)[[3]])))      #Policy vs. Experiment: A & B (collapsed) vs. A/B (collapsed)

Group1 <- c("A","A/B","A","B","A or B")
Group2 <- c("B","A/B Learn","A/B Test","A/B Test","A/B Test")

df.ChecklistVary.ttests <- cbind(df.ChecklistVary.ttests,effSizes,Group1,Group2) #bind column of cohen's d to the t-test table

#Rename columns
colnames(df.ChecklistVary.ttests) <- c("Mean 1", "Mean 2", "t", "p", "df","d","Group 1","Group 2")
df.ChecklistVary.ttests <- df.ChecklistVary.ttests[, c("Group 1","Group 2","Mean 1","Mean 2","t","df","p","d")] #Reorder columns so that df comes right after t
write.csv(df.ChecklistVary.ttests,"Output/Study2bTests.csv",row.names = TRUE) #export to csv

#############################################################
#Additional preregistered analyses (here, comparing each policy against each A/B condition separately)
AvABS <- tidy((t.test(filter(df.ChecklistVary, condFinal == 'Badge (A)')$dv,filter(df.ChecklistVary, condFinal == 'A/B')$dv, var.equal = TRUE))) #A vs. A/B
BvABS <- tidy((t.test(filter(df.ChecklistVary, condFinal == 'Poster (B)')$dv,filter(df.ChecklistVary, condFinal == 'A/B')$dv, var.equal = TRUE))) #B vs. A/B
AvABL <- tidy((t.test(filter(df.ChecklistVary, condFinal == 'Badge (A)')$dv,filter(df.ChecklistVary, condFinal == 'A/B Learn')$dv, var.equal = TRUE))) #A vs. A/B Learn 
BvABL <- tidy((t.test(filter(df.ChecklistVary, condFinal == 'Poster (B)')$dv,filter(df.ChecklistVary, condFinal == 'A/B Learn')$dv, var.equal = TRUE))) #B vs. A/B Learn

df.ChecklistVary.ttests.Prereg <- t(do.call(rbind, Map(data.frame, AvABS=AvABS, BvABS=BvABS, AvABL=AvABL, BvABL=BvABL)))

#Include only first five columns
df.ChecklistVary.ttests.Prereg <- df.ChecklistVary.ttests.Prereg[ , c(1:5) ]

effSizesPrereg <- data.frame(unname(c(
  cohen.d(filter(df.ChecklistVary, condFinal == 'Badge (A)')$dv,filter(df.ChecklistVary, condFinal == 'A/B')$dv)[[3]],             #A vs. A/B
  cohen.d(filter(df.ChecklistVary, condFinal == 'Poster (B)')$dv,filter(df.ChecklistVary, condFinal == 'A/B')$dv)[[3]],            #B vs. A/B
  cohen.d(filter(df.ChecklistVary, condFinal == 'Badge (A)')$dv,filter(df.ChecklistVary, condFinal == 'A/B Learn')$dv)[[3]],       #A vs. A/B Learn
  cohen.d(filter(df.ChecklistVary, condFinal == 'Poster (B)')$dv,filter(df.ChecklistVary, condFinal == 'A/B Learn')$dv)[[3]])))    #A vs. A/B Learn 

Group1Prereg <- c("A","B","A","B")
Group2Prereg <- c("A/B","A/B","A/B Learn","A/B Learn")

df.ChecklistVary.ttests.Prereg <- cbind(df.ChecklistVary.ttests.Prereg,effSizesPrereg,Group1Prereg,Group2Prereg) #bind column of cohen's d to the t-test table

#Rename columns
colnames(df.ChecklistVary.ttests.Prereg) <- c("Mean 1", "Mean 2", "t", "p", "df","d","Group 1","Group 2")
df.ChecklistVary.ttests.Prereg <- df.ChecklistVary.ttests.Prereg[, c("Group 1","Group 2","Mean 1","Mean 2","t","df","p","d")] #Reorder columns so that df comes right after t
write.csv(df.ChecklistVary.ttests.Prereg,"Output/Study2bPrereg.csv",row.names = TRUE) #export to csv