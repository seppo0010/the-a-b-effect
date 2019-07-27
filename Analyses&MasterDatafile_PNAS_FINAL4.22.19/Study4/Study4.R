#The A/B Effect: Objecting to experiments that compare two unobjectionable policies or treatments
#Study 4: Drug Effectiveness

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
df.Drug <- filter(df, df$Scenario == "Drug") #pull out Study 4 only
df.Drug$condFinal <- factor(df.Drug$condFinal, levels = c("Drug A", "Drug B", "A/B Learn")) #Reorder the factor for reporting in this order

# print(levels(df.Drug$condFinal)) #to see factor levels if necessary

#Summary statistics of responses to each vignette
df.Drug.summary <- summarySE(df.Drug, measurevar="dv", groupvars="condFinal") #Summarize mean, sd, se, ci

df.SummaryPercent = summarySE(df.Drug, measurevar = "dv_binary", groupvars = c("Scenario", "condFinal")) #Summarize proportion objecting
df.SummaryPercent <- df.SummaryPercent %>% mutate(dv_binary = dv_binary*100) #turn into percentages

df.Drug.Summary <- add_column(df.Drug.summary, percentObj = df.SummaryPercent[,4], .after = "N") #Add columns of percentages to table of summary stats
df.Drug.Summary <- rename(df.Drug.Summary,c("condFinal"="Condition","percentObj"="% Objecting", "dv"="M Appropriateness","sd"="SD","se"="SEM","ci"="95% CI +/-")) #rename table headers for export
write_excel_csv(df.Drug.Summary,"Output/Study4Descriptives.csv") #export to csv

# Histogram of all ratings for each condition
facet_names <- c(
  `Drug A` = "Drug A",
  `Drug B` = "Drug B",
  `A/B Learn` = "A/B learn"
)

ggplot(df.Drug) +
  geom_bar(mapping = aes(x = dv, y = ..prop..), fill = 'light blue', color = 'black', width = .75) +
  facet_grid(. ~ condFinal,labeller = as_labeller(facet_names)) +
  theme(text = element_text(size=20)) +
  theme(axis.text = element_text(colour = 'black')) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  coord_cartesian(ylim=c(0,.7), expand = TRUE) +
  scale_y_continuous(breaks=seq(0, .7, .1), expand = c(0,0)) +
  ylab('Proportion Choosing') +
  xlab('Appropriateness')

# ggsave("Study4Histo.png",height = 3) #save png file

#t-tests
#Create dataframes of each set of t-test after tidying with broom::tidy
#No longer an A/B Short condition, so we comment out tests we can no longer run.
AvB <- tidy((t.test(filter(df.Drug, condFinal == 'Drug A')$dv,filter(df.Drug, condFinal == 'Drug B')$dv, var.equal = TRUE))) #A vs. B
# ABSvABL <- tidy((t.test(filter(df.Drug, condFinal == 'A/B')$dv,filter(df.Drug, condFinal == 'A/B Learn')$dv, var.equal = TRUE))) #A/B Short vs. A/B Learn
AvAB <- tidy((t.test(filter(df.Drug, condFinal == 'Drug A')$dv,filter(df.Drug, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #A vs. A/B (collapsed together)
BvAB <- tidy((t.test(filter(df.Drug, condFinal == 'Drug B')$dv,filter(df.Drug, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #B vs. A/B (collapsed together)
PvE <- tidy((t.test(filter(df.Drug, policyOrAB == 'Policy')$dv,filter(df.Drug, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #Policy vs. Experiment: A & B (collapsed) vs. A/B (collapsed)

#Bind all t-tests into a single dataframe; transpose with t() function to right of <-
df.Drug.ttests <- t(do.call(rbind, Map(data.frame, AvB=AvB, AvAB=AvAB, BvAB=BvAB, PolicyVsExperiment=PvE)))

#Include only first five columns
df.Drug.ttests <- df.Drug.ttests[ , c(1:5) ]

#Add cohen's d for the comparison to the end of this table
effSizes <- data.frame(unname(c(
  cohen.d(filter(df.Drug, condFinal == 'Drug A')$dv,filter(df.Drug, condFinal == 'Drug B')$dv)[[3]],    #A vs. B
  # cohen.d(filter(df.Drug, condFinal == 'A/B')$dv,filter(df.Drug, condFinal == 'A/B Learn')$dv)[[3]],           #A/B Short vs. A/B Learn
  cohen.d(filter(df.Drug, condFinal == 'Drug A')$dv,filter(df.Drug, policyOrAB == 'AB Test')$dv)[[3]],      #A vs. A/B (collapsed together)
  cohen.d(filter(df.Drug, condFinal == 'Drug B')$dv,filter(df.Drug, policyOrAB == 'AB Test')$dv)[[3]],     #B vs. A/B (collapsed together)
  cohen.d(filter(df.Drug, policyOrAB == 'Policy')$dv,filter(df.Drug, policyOrAB == 'AB Test')$dv)[[3]])))      #Policy vs. Experiment: A & B (collapsed) vs. A/B (collapsed)

Group1 <- c("A","A","B","A or B")
Group2 <- c("B","A/B Learn","A/B Learn","A/B Learn")

df.Drug.ttests <- cbind(df.Drug.ttests,effSizes,Group1,Group2) #bind column of cohen's d to the t-test table

#Rename columns
colnames(df.Drug.ttests) <- c("Mean 1", "Mean 2", "t", "p", "df","d","Group 1","Group 2")
df.Drug.ttests <- df.Drug.ttests[, c("Group 1","Group 2","Mean 1","Mean 2","t","df","p","d")] #Reorder columns so that df comes right after t
write.csv(df.Drug.ttests,"Output/Study4Tests.csv",row.names = TRUE) #export to csv

#############################################################
#No additional preregistered analyses to run.