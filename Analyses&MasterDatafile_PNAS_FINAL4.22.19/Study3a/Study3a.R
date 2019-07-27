#The A/B Effect: Objecting to experiments that compare two unobjectionable policies or treatments
#Study 3a: Genetic Testing

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

#For workspace, keep only useful columns (grouping variables, dv, and dv_binary)
df <- df[,c(1:14)] #only first 14 columns in data sheet
df.GeneticTesting <- filter(df, df$Scenario == "Genetic Testing") #pull out Study 3a only
df.GeneticTesting$condFinal <- factor(df.GeneticTesting$condFinal, levels = c("Actionable (A)", "All (B)", "A/B", "A/B Learn")) #Reorder the factor for reporting in this order

# print(levels(df.GeneticTesting$condFinal)) #to see factor levels if necessary

#Summary statistics of responses to each vignette
df.GeneticTesting.summary <- summarySE(df.GeneticTesting, measurevar="dv", groupvars="condFinal") #Summarize mean, sd, se, ci

df.SummaryPercent = summarySE(df.GeneticTesting, measurevar = "dv_binary", groupvars = c("Scenario", "condFinal")) #Summarize proportion objecting
df.SummaryPercent <- df.SummaryPercent %>% mutate(dv_binary = dv_binary*100) #turn into percentages

df.GeneticTesting.Summary <- add_column(df.GeneticTesting.summary, percentObj = df.SummaryPercent[,4], .after = "N") #Add columns of percentages to table of summary stats
df.GeneticTesting.Summary <- rename(df.GeneticTesting.Summary,c("condFinal"="Condition","percentObj"="% Objecting", "dv"="M Appropriateness","sd"="SD","se"="SEM","ci"="95% CI +/-")) #rename table headers for export
write_excel_csv(df.GeneticTesting.Summary,"Output/Study3aDescriptives.csv") #export to csv

# Histogram of all ratings for each condition
facet_names <- c(
  `Actionable (A)` = "Actionable (A)",
  `All (B)` = "All (B)",
  `A/B` = "A/B short",
  `A/B Learn` = "A/B learn"
)

ggplot(df.GeneticTesting) +
  geom_bar(mapping = aes(x = dv, y = ..prop..), fill = 'light blue', color = 'black', width = .75) +
  facet_wrap(~ condFinal,labeller = as_labeller(facet_names)) + #label strip text using the labeller
  theme(text = element_text(size=24)) +
  theme(axis.text = element_text(colour = 'black')) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  coord_cartesian(ylim=c(0,.6), expand = TRUE) +
  scale_y_continuous(breaks=seq(0, .6, .1), expand = c(0,0)) +
  ylab('Proportion of participants choosing each scale point') +
  xlab('Appropriateness')

# ggsave("Study3aHisto.png") #save png file

#t-tests
#Create dataframes of each set of t-test after tidying with broom::tidy
AvB <- tidy((t.test(filter(df.GeneticTesting, condFinal == 'Actionable (A)')$dv,filter(df.GeneticTesting, condFinal == 'All (B)')$dv, var.equal = TRUE))) #A vs. B
ABSvABL <- tidy((t.test(filter(df.GeneticTesting, condFinal == 'A/B')$dv,filter(df.GeneticTesting, condFinal == 'A/B Learn')$dv, var.equal = TRUE))) #A/B Short vs. A/B Learn
AvAB <- tidy((t.test(filter(df.GeneticTesting, condFinal == 'Actionable (A)')$dv,filter(df.GeneticTesting, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #A vs. A/B (collapsed together)
BvAB <- tidy((t.test(filter(df.GeneticTesting, condFinal == 'All (B)')$dv,filter(df.GeneticTesting, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #B vs. A/B (collapsed together)
PvE <- tidy((t.test(filter(df.GeneticTesting, policyOrAB == 'Policy')$dv,filter(df.GeneticTesting, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #Policy vs. Experiment: A & B (collapsed) vs. A/B (collapsed)

#Bind all t-tests into a single dataframe; transpose with t() function to right of <-
df.GeneticTesting.ttests <- t(do.call(rbind, Map(data.frame, AvB=AvB, ABshortVsABlearn=ABSvABL, AvAB=AvAB, BvAB=BvAB, PolicyVsExperiment=PvE)))

#Include only first five rows
df.GeneticTesting.ttests <- df.GeneticTesting.ttests[ , c(1:5) ]

#Add cohen's d for the comparison to the end of this table
effSizes <- data.frame(unname(c(
  cohen.d(filter(df.GeneticTesting, condFinal == 'Actionable (A)')$dv,filter(df.GeneticTesting, condFinal == 'All (B)')$dv)[[3]],    #A vs. B
  cohen.d(filter(df.GeneticTesting, condFinal == 'A/B')$dv,filter(df.GeneticTesting, condFinal == 'A/B Learn')$dv)[[3]],           #A/B Short vs. A/B Learn
  cohen.d(filter(df.GeneticTesting, condFinal == 'Actionable (A)')$dv,filter(df.GeneticTesting, policyOrAB == 'AB Test')$dv)[[3]],      #A vs. A/B (collapsed together)
  cohen.d(filter(df.GeneticTesting, condFinal == 'All (B)')$dv,filter(df.GeneticTesting, policyOrAB == 'AB Test')$dv)[[3]],     #B vs. A/B (collapsed together)
  cohen.d(filter(df.GeneticTesting, policyOrAB == 'Policy')$dv,filter(df.GeneticTesting, policyOrAB == 'AB Test')$dv)[[3]])))      #Policy vs. Experiment: A & B (collapsed) vs. A/B (collapsed)

Group1 <- c("A","A/B","A","B","A or B")
Group2 <- c("B","A/B Learn","A/B Test","A/B Test","A/B Test")

df.GeneticTesting.ttests <- cbind(df.GeneticTesting.ttests,effSizes,Group1,Group2) #bind column of cohen's d to the t-test table

#Rename columns
colnames(df.GeneticTesting.ttests) <- c("Mean 1", "Mean 2", "t", "p", "df","d","Group 1","Group 2")
df.GeneticTesting.ttests <- df.GeneticTesting.ttests[, c("Group 1","Group 2","Mean 1","Mean 2","t","df","p","d")] #Reorder columns so that df comes right after t
write.csv(df.GeneticTesting.ttests,"Output/Study3aTests.csv",row.names = TRUE) #export to csv

#############################################################
#Additional preregistered analyses (here, comparing each policy against each A/B condition separately)
AvABS <- tidy((t.test(filter(df.GeneticTesting, condFinal == 'Actionable (A)')$dv,filter(df.GeneticTesting, condFinal == 'A/B')$dv, var.equal = TRUE)))       #A vs. A/B
BvABS <- tidy((t.test(filter(df.GeneticTesting, condFinal == 'All (B)')$dv,filter(df.GeneticTesting, condFinal == 'A/B')$dv, var.equal = TRUE)))              #B vs. A/B
AvABL <- tidy((t.test(filter(df.GeneticTesting, condFinal == 'Actionable (A)')$dv,filter(df.GeneticTesting, condFinal == 'A/B Learn')$dv, var.equal = TRUE))) #A vs. A/B Learn 
BvABL <- tidy((t.test(filter(df.GeneticTesting, condFinal == 'All (B)')$dv,filter(df.GeneticTesting, condFinal == 'A/B Learn')$dv, var.equal = TRUE)))        #B vs. A/B Learn

df.GeneticTesting.ttests.Prereg <- t(do.call(rbind, Map(data.frame, AvABS=AvABS, BvABS=BvABS, AvABL=AvABL, BvABL=BvABL)))

#Include only first five columns
df.GeneticTesting.ttests.Prereg <- df.GeneticTesting.ttests.Prereg[ , c(1:5) ]

effSizesPrereg <- data.frame(unname(c(
  cohen.d(filter(df.GeneticTesting, condFinal == 'Actionable (A)')$dv,filter(df.GeneticTesting, condFinal == 'A/B')$dv)[[3]],       #A vs. A/B
  cohen.d(filter(df.GeneticTesting, condFinal == 'All (B)')$dv,filter(df.GeneticTesting, condFinal == 'A/B')$dv)[[3]],              #B vs. A/B
  cohen.d(filter(df.GeneticTesting, condFinal == 'Actionable (A)')$dv,filter(df.GeneticTesting, condFinal == 'A/B Learn')$dv)[[3]], #A vs. A/B Learn
  cohen.d(filter(df.GeneticTesting, condFinal == 'All (B)')$dv,filter(df.GeneticTesting, condFinal == 'A/B Learn')$dv)[[3]])))      #A vs. A/B Learn 

Group1Prereg <- c("A","B","A","B")
Group2Prereg <- c("A/B","A/B","A/B Learn","A/B Learn")

df.GeneticTesting.ttests.Prereg <- cbind(df.GeneticTesting.ttests.Prereg,effSizesPrereg,Group1Prereg,Group2Prereg) #bind column of cohen's d to the t-test table

#Rename columns
colnames(df.GeneticTesting.ttests.Prereg) <- c("Mean 1", "Mean 2", "t", "p", "df","d","Group 1","Group 2")
df.GeneticTesting.ttests.Prereg <- df.GeneticTesting.ttests.Prereg[, c("Group 1","Group 2","Mean 1","Mean 2","t","df","p","d")] #Reorder columns so that df comes right after t
write.csv(df.GeneticTesting.ttests.Prereg,"Output/Study3aPrereg.csv",row.names = TRUE) #export to csv