#The A/B Effect: Objecting to experiments that compare two unobjectionable policies or treatments
#Study 1: Safety Checklist 1 (MTurk sample on Surveymonkey)

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
# df <- filter(df,ResponseId != "6297134747" &
#     ResponseId != "6297286920" &
#     ResponseId != "6297092458" &
#     ResponseId != "6297095408" &
#     ResponseId != "6297136368" &
#     ResponseId != "6297143573" &
#     ResponseId != "6297199671" &
#     ResponseId != "6297200628" &
#     ResponseId != "6297200635" &
#     ResponseId != "6297202377" &
#     ResponseId != "6297218301" &
#     ResponseId != "6297384573" &
#     ResponseId != "6297095762" &
#     ResponseId != "6297099345" &
#     ResponseId != "6297119550" &
#     ResponseId != "6297097737" &
#     ResponseId != "6297087586" &
#     ResponseId != "6297109495" &
#     ResponseId != "6297122963" &
#     ResponseId != "6297199984" &
#     ResponseId != "6297604728" &
#     ResponseId != "6297084435" &
#     ResponseId != "6297086391"
# ) #Reviewer requested analysis: exclude all participants who were coded as Inequality or No Equipoise #ID's pulled from Free Response Coding folder/worksheet #comment in to exclude

#For workspace, keep only useful columns (grouping variables, dv, and dv_binary)
df <- df[,c(1:14)] #only first 14 columns in data sheet
df.Checklist <- filter(df, df$Scenario == "Checklist") #pull out Study 1 only
df.Checklist$condFinal <- factor(df.Checklist$condFinal, levels = c("Badge (A)", "Poster (B)", "A/B", "A/B Learn")) #Reorder the factor for reporting in this order

# print(levels(df.Checklist$condFinal)) #to see factor levels if necessary

#Summary statistics of responses to each vignette
df.Checklist.summary <- summarySE(df.Checklist, measurevar="dv", groupvars="condFinal") #Summarize mean, sd, se, ci

df.SummaryPercent = summarySE(df.Checklist, measurevar = "dv_binary", groupvars = c("Scenario", "condFinal")) #Summarize proportion objecting
df.SummaryPercent <- df.SummaryPercent %>% mutate(dv_binary = dv_binary*100) #turn into percentages

df.Checklist.Summary <- add_column(df.Checklist.summary, percentObj = df.SummaryPercent[,4], .after = "N") #Add columns of percentages to table of summary stats
df.Checklist.Summary <- rename(df.Checklist.Summary,c("condFinal"="Condition","percentObj"="% Objecting", "dv"="M Appropriateness","sd"="SD","se"="SEM","ci"="95% CI +/-")) #rename table headers for export
write_excel_csv(df.Checklist.Summary,"Output/Study1Descriptives.csv") #export to csv in the output folder

# Histogram of all ratings for each condition

#create a labeller to give control over strip text in histogram
facet_names <- c(
  `Badge (A)` = "Badge (A)",
  `Poster (B)` = "Poster (B)",
  `A/B` = "A/B short",
  `A/B Learn` = "A/B learn"
  )

ggplot(df.Checklist) +
  geom_bar(mapping = aes(x = dv, y = ..prop..), fill = 'light blue', color = 'black', width = .75) +
  facet_wrap(~ condFinal,labeller = as_labeller(facet_names)) + #label strip text using the labeller
  theme(text = element_text(size=24)) +
  theme(axis.text = element_text(colour = 'black')) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  coord_cartesian(ylim=c(0,.6), expand = TRUE) +
  scale_y_continuous(breaks=seq(0, .6, .1), expand = c(0,0)) +
  ylab('Proportion of participants choosing each scale point') +
  xlab('Appropriateness')

# ggsave("Study1Histo.png") #save png file

#t-tests
#Create dataframes of each set of t-test after tidying with broom::tidy
AvB <- tidy((t.test(filter(df.Checklist, condFinal == 'Badge (A)')$dv,filter(df.Checklist, condFinal == 'Poster (B)')$dv, var.equal = TRUE))) #A vs. B
ABSvABL <- tidy((t.test(filter(df.Checklist, condFinal == 'A/B')$dv,filter(df.Checklist, condFinal == 'A/B Learn')$dv, var.equal = TRUE))) #A/B Short vs. A/B Learn
AvAB <- tidy((t.test(filter(df.Checklist, condFinal == 'Badge (A)')$dv,filter(df.Checklist, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #A vs. A/B (collapsed together)
BvAB <- tidy((t.test(filter(df.Checklist, condFinal == 'Poster (B)')$dv,filter(df.Checklist, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #B vs. A/B (collapsed together)
PvE <- tidy((t.test(filter(df.Checklist, policyOrAB == 'Policy')$dv,filter(df.Checklist, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #Policy vs. Experiment: A & B (collapsed) vs. A/B (collapsed)

#Bind all t-tests into a single dataframe; transpose with t() function to right of <-
df.Checklist.ttests <- t(do.call(rbind, Map(data.frame, AvB=AvB, ABshortVsABlearn=ABSvABL, AvAB=AvAB, BvAB=BvAB, PolicyVsExperiment=PvE)))

#Include only first five columns
df.Checklist.ttests <- df.Checklist.ttests[ , c(1:5) ]

#Add cohen's d for the comparison to the end of this table
effSizes <- data.frame(unname(c(
  cohen.d(filter(df.Checklist, condFinal == 'Badge (A)')$dv,filter(df.Checklist, condFinal == 'Poster (B)')$dv)[[3]],    #A vs. B
  cohen.d(filter(df.Checklist, condFinal == 'A/B')$dv,filter(df.Checklist, condFinal == 'A/B Learn')$dv)[[3]],           #A/B Short vs. A/B Learn
  cohen.d(filter(df.Checklist, condFinal == 'Badge (A)')$dv,filter(df.Checklist, policyOrAB == 'AB Test')$dv)[[3]],      #A vs. A/B (collapsed together)
  cohen.d(filter(df.Checklist, condFinal == 'Poster (B)')$dv,filter(df.Checklist, policyOrAB == 'AB Test')$dv)[[3]],     #B vs. A/B (collapsed together)
  cohen.d(filter(df.Checklist, policyOrAB == 'Policy')$dv,filter(df.Checklist, policyOrAB == 'AB Test')$dv)[[3]])))      #Policy vs. Experiment: A & B (collapsed) vs. A/B (collapsed)

Group1 <- c("A","A/B","A","B","A or B")
Group2 <- c("B","A/B Learn","A/B Test","A/B Test","A/B Test")

df.Checklist.ttests <- cbind(df.Checklist.ttests,effSizes,Group1,Group2) #bind column of cohen's d to the t-test table

#Rename columns
colnames(df.Checklist.ttests) <- c("Mean 1", "Mean 2", "t", "p", "df","d","Group 1","Group 2")
df.Checklist.ttests <- df.Checklist.ttests[, c("Group 1","Group 2","Mean 1","Mean 2","t","df","p","d")] #Reorder columns so that df comes right after t
write_excel_csv(df.Checklist.ttests,"Output/Study1Tests.csv") #export to csv in the output folder