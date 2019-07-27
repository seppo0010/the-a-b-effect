#The A/B Effect: Objecting to experiments that compare two unobjectionable policies or treatments
#Study 2a: Safety Checklist (MTurk sample on Qualtrics; Direct replication of Study 1)

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
# df <- filter(df,ResponseId != "R_3jYX07TDqLZWJXW" &
#     ResponseId != "R_1QgPGNJm45izLyg" &
#     ResponseId != "R_ysaM3XebYJOmIyl" &
#     ResponseId != "R_2TTtyHZSxP0YCBA" &
#     ResponseId != "R_1hAUg5Nb1WN3FXQ" &
#     ResponseId != "R_sStfZnj66LwL74l" &
#     ResponseId != "R_3NIIgfn7EMvnM29" &
#     ResponseId != "R_1pM5kAOoCerfgBO" &
#     ResponseId != "R_eLGew7rmON3KdRD" &
#     ResponseId != "R_2pYrhIa0luPaw1x" &
#     ResponseId != "R_2dRwpPwsO4z1eyT" &
#     ResponseId != "R_2y8YYdTl8US6X1z" &
#     ResponseId != "R_A1Bo1sy3ajQtHnr" &
#     ResponseId != "R_C9tmHEJu5BkT48F" &
#     ResponseId != "R_2yd3DXXzrWXxsJn" &
#     ResponseId != "R_3kIhTRyyqnZSrME" &
#     ResponseId != "R_Z7rXcWRrkTsH3C9" &
#     ResponseId != "R_25GRaq3c2GeEWbM" &
#     ResponseId != "R_2dK3DjRasQwUsxx" &
#     ResponseId != "R_1QyetJKf2BipWHk" &
#     ResponseId != "R_3eeIeHL2dj6nN8j" &
#     ResponseId != "R_12JOF0UZxqBQP6W" &
#     ResponseId != "R_1IoNRwSYyNtXTVb" &
#     ResponseId != "R_3EMves4XyXmfr5c" &
#     ResponseId != "R_OdoyhOXjhXdUmDn" &
#     ResponseId != "R_3j0pZFbQTsSA7y4" &
#     ResponseId != "R_2WIL44CzhDgejFp" &
#     ResponseId != "R_1K9oIwJcBCQjR8O" &
#     ResponseId != "R_2qrd6HesOH0jbiu" &
#     ResponseId != "R_3J4TTAlshPYB2Li" 
# ) #Reviewer requested analysis: exclude all participants who were coded as Inequality or No Equipoise #ID's pulled from Free Response Coding folder/worksheet #comment in to exclude


#For workspace, keep only useful columns (grouping variables, dv, and dv_binary)
# df <- df[,c(1:14)] #only first 14 columns in data sheet
df.ChecklistExact <- filter(df, df$Scenario == "Checklist_exact") #pull out Study 2a only
df.ChecklistExact$condFinal <- factor(df.ChecklistExact$condFinal, levels = c("Badge (A)", "Poster (B)", "A/B", "A/B Learn")) #Reorder the factor for reporting in this order

# print(levels(df.Checklist$condFinal)) #to see factor levels if necessary

#Summary statistics of responses to each vignette
df.ChecklistExact.summary <- summarySE(df.ChecklistExact, measurevar="dv", groupvars="condFinal") #Summarize mean, sd, se, ci

df.SummaryPercent = summarySE(df.ChecklistExact, measurevar = "dv_binary", groupvars = c("Scenario", "condFinal")) #Summarize proportion objecting
df.SummaryPercent <- df.SummaryPercent %>% mutate(dv_binary = dv_binary*100) #turn into percentages

df.ChecklistExact.Summary <- add_column(df.ChecklistExact.summary, percentObj = df.SummaryPercent[,4], .after = "N") #Add columns of percentages to table of summary stats
df.ChecklistExact.Summary <- rename(df.ChecklistExact.Summary,c("condFinal"="Condition","percentObj"="% Objecting", "dv"="M Appropriateness","sd"="SD","se"="SEM","ci"="95% CI +/-")) #rename table headers for export
write_excel_csv(df.ChecklistExact.Summary,"Output/Study2aDescriptives.csv") #export to csv

# Histogram of all ratings for each condition
#create a labeller to give control over strip text in histogram
facet_names <- c(
  `Badge (A)` = "Badge (A)",
  `Poster (B)` = "Poster (B)",
  `A/B` = "A/B short",
  `A/B Learn` = "A/B learn"
)

ggplot(df.ChecklistExact) +
  geom_bar(mapping = aes(x = dv, y = ..prop..), fill = 'light blue', color = 'black', width = .75) +
  facet_wrap(~ condFinal,labeller = as_labeller(facet_names)) + #label strip text using the labeller
  theme(text = element_text(size=24)) +
  theme(axis.text = element_text(colour = 'black')) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  coord_cartesian(ylim=c(0,.6), expand = TRUE) +
  scale_y_continuous(breaks=seq(0, .6, .1), expand = c(0,0)) +
  ylab('Proportion of participants choosing each scale point') +
  xlab('Appropriateness')

# ggsave("Study2aHisto.png") #save png file


#t-tests
#Create dataframes of each set of t-test after tidying with broom::tidy
AvB <- tidy((t.test(filter(df.ChecklistExact, condFinal == 'Badge (A)')$dv,filter(df.ChecklistExact, condFinal == 'Poster (B)')$dv, var.equal = TRUE))) #A vs. B
ABSvABL <- tidy((t.test(filter(df.ChecklistExact, condFinal == 'A/B')$dv,filter(df.ChecklistExact, condFinal == 'A/B Learn')$dv, var.equal = TRUE))) #A/B Short vs. A/B Learn
AvAB <- tidy((t.test(filter(df.ChecklistExact, condFinal == 'Badge (A)')$dv,filter(df.ChecklistExact, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #A vs. A/B (collapsed together)
BvAB <- tidy((t.test(filter(df.ChecklistExact, condFinal == 'Poster (B)')$dv,filter(df.ChecklistExact, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #B vs. A/B (collapsed together)
PvE <- tidy((t.test(filter(df.ChecklistExact, policyOrAB == 'Policy')$dv,filter(df.ChecklistExact, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #Policy vs. Experiment: A & B (collapsed) vs. A/B (collapsed)

#Bind all t-tests into a single dataframe; transpose with t() function to right of <-
df.ChecklistExact.ttests <- t(do.call(rbind, Map(data.frame, AvB=AvB, ABshortVsABlearn=ABSvABL, AvAB=AvAB, BvAB=BvAB, PolicyVsExperiment=PvE)))

#Include only first five columns
df.ChecklistExact.ttests <- df.ChecklistExact.ttests[ , c(1:5) ]

#Add cohen's d for the comparison to the end of this table
effSizes <- data.frame(unname(c(
  cohen.d(filter(df.ChecklistExact, condFinal == 'Badge (A)')$dv,filter(df.ChecklistExact, condFinal == 'Poster (B)')$dv)[[3]],    #A vs. B
  cohen.d(filter(df.ChecklistExact, condFinal == 'A/B')$dv,filter(df.ChecklistExact, condFinal == 'A/B Learn')$dv)[[3]],           #A/B Short vs. A/B Learn
  cohen.d(filter(df.ChecklistExact, condFinal == 'Badge (A)')$dv,filter(df.ChecklistExact, policyOrAB == 'AB Test')$dv)[[3]],      #A vs. A/B (collapsed together)
  cohen.d(filter(df.ChecklistExact, condFinal == 'Poster (B)')$dv,filter(df.ChecklistExact, policyOrAB == 'AB Test')$dv)[[3]],     #B vs. A/B (collapsed together)
  cohen.d(filter(df.ChecklistExact, policyOrAB == 'Policy')$dv,filter(df.ChecklistExact, policyOrAB == 'AB Test')$dv)[[3]])))      #Policy vs. Experiment: A & B (collapsed) vs. A/B (collapsed)

Group1 <- c("A","A/B","A","B","A or B")
Group2 <- c("B","A/B Learn","A/B Test","A/B Test","A/B Test")

df.ChecklistExact.ttests <- cbind(df.ChecklistExact.ttests,effSizes,Group1,Group2) #bind column of cohen's d to the t-test table

#Rename columns
colnames(df.ChecklistExact.ttests) <- c("Mean 1", "Mean 2", "t", "p", "df","d","Group 1","Group 2")
df.ChecklistExact.ttests <- df.ChecklistExact.ttests[, c("Group 1","Group 2","Mean 1","Mean 2","t","df","p","d")] #Reorder columns so that df comes right after t
write.csv(df.ChecklistExact.ttests,"Output/Study2aTests.csv",row.names = TRUE) #export to csv

#############################################################
#Additional preregistered analyses (here, comparing each policy against each A/B condition separately)
AvABS <- tidy((t.test(filter(df.ChecklistExact, condFinal == 'Badge (A)')$dv,filter(df.ChecklistExact, condFinal == 'A/B')$dv, var.equal = TRUE))) #A vs. A/B
BvABS <- tidy((t.test(filter(df.ChecklistExact, condFinal == 'Poster (B)')$dv,filter(df.ChecklistExact, condFinal == 'A/B')$dv, var.equal = TRUE))) #B vs. A/B
AvABL <- tidy((t.test(filter(df.ChecklistExact, condFinal == 'Badge (A)')$dv,filter(df.ChecklistExact, condFinal == 'A/B Learn')$dv, var.equal = TRUE))) #A vs. A/B Learn 
BvABL <- tidy((t.test(filter(df.ChecklistExact, condFinal == 'Poster (B)')$dv,filter(df.ChecklistExact, condFinal == 'A/B Learn')$dv, var.equal = TRUE))) #B vs. A/B Learn

df.ChecklistExact.ttests.Prereg <- t(do.call(rbind, Map(data.frame, AvABS=AvABS, BvABS=BvABS, AvABL=AvABL, BvABL=BvABL)))

#Include only first five rows
df.ChecklistExact.ttests.Prereg <- df.ChecklistExact.ttests.Prereg[ , c(1:5) ]

effSizesPrereg <- data.frame(unname(c(
  cohen.d(filter(df.ChecklistExact, condFinal == 'Badge (A)')$dv,filter(df.ChecklistExact, condFinal == 'A/B')$dv)[[3]],            #A vs. A/B
  cohen.d(filter(df.ChecklistExact, condFinal == 'Poster (B)')$dv,filter(df.ChecklistExact, condFinal == 'A/B')$dv)[[3]],           #B vs. A/B
  cohen.d(filter(df.ChecklistExact, condFinal == 'Badge (A)')$dv,filter(df.ChecklistExact, condFinal == 'A/B Learn')$dv)[[3]],      #A vs. A/B Learn
  cohen.d(filter(df.ChecklistExact, condFinal == 'Poster (B)')$dv,filter(df.ChecklistExact, condFinal == 'A/B Learn')$dv)[[3]])))   #A vs. A/B Learn 

Group1Prereg <- c("A","B","A","B")
Group2Prereg <- c("A/B","A/B","A/B Learn","A/B Learn")

df.ChecklistExact.ttests.Prereg <- cbind(df.ChecklistExact.ttests.Prereg,effSizesPrereg,Group1Prereg,Group2Prereg) #bind column of cohen's d to the t-test table

#Rename columns
colnames(df.ChecklistExact.ttests.Prereg) <- c("Mean 1", "Mean 2", "t", "p", "df","d","Group 1","Group 2")
df.ChecklistExact.ttests.Prereg <- df.ChecklistExact.ttests.Prereg[, c("Group 1","Group 2","Mean 1","Mean 2","t","df","p","d")] #Reorder columns so that df comes right after t
write.csv(df.ChecklistExact.ttests.Prereg,"Output/Study2aPrereg.csv",row.names = TRUE) #export to csv