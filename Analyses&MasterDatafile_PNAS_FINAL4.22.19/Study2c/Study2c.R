#The A/B Effect: Objecting to experiments that compare two unobjectionable policies or treatments
#Study 2c: Safety Checklist  (Pollfish sample of mobile users; replication of Study 1)

rm(list = ls()) #clear workspace

#Load packages
library(tidyverse) #for ggplot and filter
library(readxl) #for read
library(effsize) #for cohens d
library(gridExtra) #for multi plots
library(psy) #for alpha
library(psych) #for ANOVA #can comment out if it conflicts with other packages.
library(scales) #for colors
library(Rmisc) #for summary stats
library(knitr) #for tabling in R
library(kableExtra) #for tabling in R
library(broom) #for tabling in R

#read in the desired worksheet 
df <- read_excel("ABI_MasterDatafile.xlsx", sheet = "FullExperimentsOnly")
df <- filter(df,RepeatTurkerLabelCode != "1") #Exclude those with 1: participants who took the pollfish survey more than once.

#Exclude those with 1: participants who reversed the scale order, gave a nonsense answer, or misunderstood the vignette
# df <- filter(df,MarkedForExclusion != "1") #uncomment to exclude hand-coded exclusions.

#For workspace, keep only useful columns (grouping variables, dv, and dv_binary)
# df <- df[,c(1:14)] #only first 14 columns in data sheet
df.ChecklistPF <- filter(df, df$Scenario == "Checklist_pf") #pull out Study 2c only

df.ChecklistPF$condFinal <- factor(df.ChecklistPF$condFinal, levels = c("Badge (A)", "Poster (B)", "A/B", "A/B Learn")) #Reorder the factor for reporting in this order

# print(levels(df.Checklist$condFinal)) #to see factor levels if necessary

#Summary statistics of responses to each vignette
df.ChecklistPF.summary <- summarySE(df.ChecklistPF, measurevar="dv", groupvars="condFinal") #Summarize mean, sd, se, ci

df.SummaryPercent = summarySE(df.ChecklistPF, measurevar = "dv_binary", groupvars = c("Scenario", "condFinal")) #Summarize proportion objecting
df.SummaryPercent <- df.SummaryPercent %>% mutate(dv_binary = dv_binary*100) #turn into percentages

df.ChecklistPF.Summary <- add_column(df.ChecklistPF.summary, percentObj = df.SummaryPercent[,4], .after = "N") #Add columns of percentages to table of summary stats
df.ChecklistPF.Summary <- rename(df.ChecklistPF.Summary,c("condFinal"="Condition","percentObj"="% Objecting", "dv"="M Appropriateness","sd"="SD","se"="SEM","ci"="95% CI +/-")) #rename table headers for export
write_excel_csv(df.ChecklistPF.Summary,"Output/Study2cDescriptives.csv") #export to csv

# Histogram of all ratings for each condition
facet_names <- c(
  `Badge (A)` = "Badge (A)",
  `Poster (B)` = "Poster (B)",
  `A/B` = "A/B short",
  `A/B Learn` = "A/B learn"
)

ggplot(df.ChecklistPF) +
  geom_bar(mapping = aes(x = dv, y = ..prop..), fill = 'light blue', color = 'black', width = .75) +
  facet_wrap(~ condFinal,labeller = as_labeller(facet_names)) + #label strip text using the labeller
  theme(text = element_text(size=24)) +
  theme(axis.text = element_text(colour = 'black')) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  coord_cartesian(ylim=c(0,.6), expand = TRUE) +
  scale_y_continuous(breaks=seq(0, .6, .1), expand = c(0,0)) +
  ylab('Proportion of participants choosing each scale point') +
  xlab('Appropriateness')

# ggsave("Study2cHisto.png") #save png file

#t-tests
#Create dataframes of each set of t-test after tidying with broom::tidy
AvB <- tidy((t.test(filter(df.ChecklistPF, condFinal == 'Badge (A)')$dv,filter(df.ChecklistPF, condFinal == 'Poster (B)')$dv, var.equal = TRUE))) #A vs. B
ABSvABL <- tidy((t.test(filter(df.ChecklistPF, condFinal == 'A/B')$dv,filter(df.ChecklistPF, condFinal == 'A/B Learn')$dv, var.equal = TRUE))) #A/B Short vs. A/B Learn
AvAB <- tidy((t.test(filter(df.ChecklistPF, condFinal == 'Badge (A)')$dv,filter(df.ChecklistPF, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #A vs. A/B (collapsed together)
BvAB <- tidy((t.test(filter(df.ChecklistPF, condFinal == 'Poster (B)')$dv,filter(df.ChecklistPF, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #B vs. A/B (collapsed together)
PvE <- tidy((t.test(filter(df.ChecklistPF, policyOrAB == 'Policy')$dv,filter(df.ChecklistPF, policyOrAB == 'AB Test')$dv, var.equal = TRUE))) #Policy vs. Experiment: A & B (collapsed) vs. A/B (collapsed)

#Bind all t-tests into a single dataframe; transpose with t() function to right of <-
df.ChecklistPF.ttests <- t(do.call(rbind, Map(data.frame, AvB=AvB, ABshortVsABlearn=ABSvABL, AvAB=AvAB, BvAB=BvAB, PolicyVsExperiment=PvE)))

#Include only first five rows
df.ChecklistPF.ttests <- df.ChecklistPF.ttests[ , c(1:5) ]

#Add cohen's d for the comparison to the end of this table
effSizes <- data.frame(unname(c(
  cohen.d(filter(df.ChecklistPF, condFinal == 'Badge (A)')$dv,filter(df.ChecklistPF, condFinal == 'Poster (B)')$dv)[[3]],    #A vs. B
  cohen.d(filter(df.ChecklistPF, condFinal == 'A/B')$dv,filter(df.ChecklistPF, condFinal == 'A/B Learn')$dv)[[3]],           #A/B Short vs. A/B Learn
  cohen.d(filter(df.ChecklistPF, condFinal == 'Badge (A)')$dv,filter(df.ChecklistPF, policyOrAB == 'AB Test')$dv)[[3]],      #A vs. A/B (collapsed together)
  cohen.d(filter(df.ChecklistPF, condFinal == 'Poster (B)')$dv,filter(df.ChecklistPF, policyOrAB == 'AB Test')$dv)[[3]],     #B vs. A/B (collapsed together)
  cohen.d(filter(df.ChecklistPF, policyOrAB == 'Policy')$dv,filter(df.ChecklistPF, policyOrAB == 'AB Test')$dv)[[3]])))      #Policy vs. Experiment: A & B (collapsed) vs. A/B (collapsed)

Group1 <- c("A","A/B","A","B","A or B")
Group2 <- c("B","A/B Learn","A/B Test","A/B Test","A/B Test")

df.ChecklistPF.ttests <- cbind(df.ChecklistPF.ttests,effSizes,Group1,Group2) #bind column of cohen's d to the t-test table

#Rename columns
colnames(df.ChecklistPF.ttests) <- c("Mean 1", "Mean 2", "t", "p", "df","d","Group 1","Group 2")
df.ChecklistPF.ttests <- df.ChecklistPF.ttests[, c("Group 1","Group 2","Mean 1","Mean 2","t","df","p","d")] #Reorder columns so that df comes right after t
write.csv(df.ChecklistPF.ttests,"Output/Study2cTests.csv",row.names = TRUE) #export to csv

#############################################################
#Additional preregistered analyses (here, comparing each policy against each A/B condition separately)
AvABS <- tidy((t.test(filter(df.ChecklistPF, condFinal == 'Badge (A)')$dv,filter(df.ChecklistPF, condFinal == 'A/B')$dv, var.equal = TRUE)))        #A vs. A/B
BvABS <- tidy((t.test(filter(df.ChecklistPF, condFinal == 'Poster (B)')$dv,filter(df.ChecklistPF, condFinal == 'A/B')$dv, var.equal = TRUE)))       #B vs. A/B
AvABL <- tidy((t.test(filter(df.ChecklistPF, condFinal == 'Badge (A)')$dv,filter(df.ChecklistPF, condFinal == 'A/B Learn')$dv, var.equal = TRUE)))  #A vs. A/B Learn 
BvABL <- tidy((t.test(filter(df.ChecklistPF, condFinal == 'Poster (B)')$dv,filter(df.ChecklistPF, condFinal == 'A/B Learn')$dv, var.equal = TRUE))) #B vs. A/B Learn

df.ChecklistPF.ttests.Prereg <- t(do.call(rbind, Map(data.frame, AvABS=AvABS, BvABS=BvABS, AvABL=AvABL, BvABL=BvABL)))

#Include only first five columns
df.ChecklistPF.ttests.Prereg <- df.ChecklistPF.ttests.Prereg[ , c(1:5) ]

effSizesPrereg <- data.frame(unname(c(
  cohen.d(filter(df.ChecklistPF, condFinal == 'Badge (A)')$dv,filter(df.ChecklistPF, condFinal == 'A/B')$dv)[[3]],           #A vs. A/B
  cohen.d(filter(df.ChecklistPF, condFinal == 'Poster (B)')$dv,filter(df.ChecklistPF, condFinal == 'A/B')$dv)[[3]],          #B vs. A/B
  cohen.d(filter(df.ChecklistPF, condFinal == 'Badge (A)')$dv,filter(df.ChecklistPF, condFinal == 'A/B Learn')$dv)[[3]],     #A vs. A/B Learn
  cohen.d(filter(df.ChecklistPF, condFinal == 'Poster (B)')$dv,filter(df.ChecklistPF, condFinal == 'A/B Learn')$dv)[[3]])))  #A vs. A/B Learn 

Group1Prereg <- c("A","B","A","B")
Group2Prereg <- c("A/B","A/B","A/B Learn","A/B Learn")

df.ChecklistPF.ttests.Prereg <- cbind(df.ChecklistPF.ttests.Prereg,effSizesPrereg,Group1Prereg,Group2Prereg) #bind column of cohen's d to the t-test table

#Rename columns
colnames(df.ChecklistPF.ttests.Prereg) <- c("Mean 1", "Mean 2", "t", "p", "df","d","Group 1","Group 2")
df.ChecklistPF.ttests.Prereg <- df.ChecklistPF.ttests.Prereg[, c("Group 1","Group 2","Mean 1","Mean 2","t","df","p","d")] #Reorder columns so that df comes right after t
write.csv(df.ChecklistPF.ttests.Prereg,"Output/Study2cPrereg.csv",row.names = TRUE) #export to csv

##Analyze Order Effect

df.ChecklistPF.orderSummary <- summarySE(df.ChecklistPF, measurevar="dv", groupvars="poleOrderPollfish") #Summarize mean, sd, se, ci 
t.test(filter(df.ChecklistPF, poleOrderPollfish == 'HiToLo')$dv,filter(df.ChecklistPF, poleOrderPollfish == 'LoToHi')$dv, var.equal = TRUE) #t-test for order effect
cohen.d(filter(df.ChecklistPF, poleOrderPollfish == 'HiToLo')$dv,filter(df.ChecklistPF, poleOrderPollfish == 'LoToHi')$dv)[[3]] #d for order effect

fit <- aov(dv ~ policyOrAB*poleOrderPollfish, data=df.ChecklistPF) #ANOVA on condition and order
model.Checklist <- summary(fit) #check ANOVA stats

anovaSummary = describeBy(df.ChecklistPF$dv,list(df.ChecklistPF$policyOrAB,df.ChecklistPF$poleOrderPollfish), mat=TRUE,digits=2) #see descriptives for each level of each condition

limits = aes(ymax = mean + (1.96*se), ymin=mean - (1.96*se)) #add 95% CI bounds

dodge = position_dodge(width=0.9)

apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'))

ggplot(anovaSummary, aes(x = group2, y = mean, fill = group1))+
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  theme_set(theme_classic(base_size = 24))+
  ylab('Appropriateness') + xlab('Scale Order') + guides(fill=guide_legend(title=NULL),size=12) +
  scale_x_discrete(labels=c("Descending","Ascending")) +
  coord_cartesian(ylim=c(1,5), expand = TRUE) +
  scale_y_continuous(breaks=seq(1, 5, 1), expand = c(0,0)) +
  scale_fill_grey() +
  theme(text = element_text(size=24)) +
  theme(axis.text = element_text(colour = 'black')) +
  theme(legend.position = c(0.8, 0.9))