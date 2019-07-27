#The A/B Effect: Objecting to experiments that compare two unobjectionable policies or treatments
#Figure 2
rm(list = ls())

#Load packages
library(tidyverse) #for ggplot and filter
library(readxl) #for read
library(effsize) #for cohens d
library(gridExtra) #for multi plots
library(psy) #for alpha
library(scales) #for colors
library(Rmisc) #for summary stats
library(knitr)
library(kableExtra)
library(broom)

#read in the desired worksheet 
df <- read_excel("ABI_MasterDatafile.xlsx", sheet = "FullExperimentsOnly")
df <- filter(df,RepeatTurkerLabelCode != "1") #Exclude those with 1: participants anywhere in the dataset who took our very first survey AND another survey

#For workspace, keep only useful columns (here, grouping variables, dv, and dv_binary)
df <- df[,c(1:14)]
df.Drug <- filter(df, df$Scenario == "Drug" | df$Scenario == "DrugTeach" | df$Scenario == "DrugTeach_pf") #pull out best drug 1.0 and 2.0 MTurk and Pollfish

#PANEL A: first, plot original drug experiment
df.DrugOrig <- filter(df.Drug, df.Drug$Scenario == "Drug") #pull out original Drug experiment
df.DrugOrig.summary <- summarySE(df.DrugOrig, measurevar = "dv_binary", groupvars = "genericCond") #summarize proportion dv
df.DrugOrig.summary <- df.DrugOrig.summary %>% mutate(dv_binary = dv_binary*100) #turn into percentages
df.DrugOrig.summary$genericCond <- factor(df.DrugOrig.summary$genericCond, levels = c("A", "B", "AB")) #order condLabels to be: A, B, AB

pDrugOrig <- ggplot(df.DrugOrig.summary, aes(x = genericCond, y = dv_binary)) +
  geom_bar(stat = 'identity', fill = 'grey38', color = 'grey38', width = .65) +
  theme_set(theme_classic(base_size = 16)) +
  ylab('% rating inappropriate') +
  xlab(NULL) +
  # ylim(0,50) +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = 'black', size = .5, linetype = 'solid')) +
  theme(axis.text = element_text(colour = 'black')) +
  scale_x_discrete(labels=c('A' = 'Drug\nA', 'B' = 'Drug\nB', 'AB' = 'A/B\nTest')) +
  ggtitle('A') + theme(plot.title = element_text(hjust = 0.05,size=30,margin = margin(t = 8, b = -30)))
  # ggsave("Checklist.png", width = 7.0, height = 5, units = "in", dpi = 500)

#PANEL B: next, plot best drug 2.0 MTurk
df.TeachDrug <- filter(df.Drug, df.Drug$Scenario == "DrugTeach") #pull out teaching Drug experiment
df.TeachDrug.summary <- summarySE(df.TeachDrug, measurevar = "dv_binary", groupvars = "genericCond") #summarize proportion dv
df.TeachDrug.summary <- df.TeachDrug.summary %>% mutate(dv_binary = dv_binary*100) #turn into percentages
df.TeachDrug.summary$genericCond <- factor(df.TeachDrug.summary$genericCond, levels = c("A", "B", "AB")) #order condLabels to be: A, B, AB

pTeachDrug <- ggplot(df.TeachDrug.summary, aes(x = genericCond, y = dv_binary)) +
  geom_bar(stat = 'identity', fill = 'grey38', color = 'grey38', width = .65) +
  theme_set(theme_classic(base_size = 16)) +
  ylab('% rating inappropriate') +
  xlab(NULL) +
  # ylim(0,50) +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = 'black', size = .5, linetype = 'solid')) +
  theme(axis.text = element_text(colour = 'black')) +
  scale_x_discrete(labels=c('A' = 'Drug\nA', 'B' = 'Drug\nB', 'AB' = 'A/B\nTest')) +
  ggtitle('B') + theme(plot.title = element_text(hjust = 0.05,size=30,margin = margin(t = 8, b = -30)))
# ggsave("Checklist.png", width = 7.0, height = 5, units = "in", dpi = 500)

#PANEL C: next, plot best drug 2.0 Pollfish
df.TeachDrug.pf <- filter(df.Drug, df.Drug$Scenario == "DrugTeach_pf") #pull out teaching Drug experiment
df.TeachDrug.pf.summary <- summarySE(df.TeachDrug.pf, measurevar = "dv_binary", groupvars = "genericCond") #summarize proportion dv
df.TeachDrug.pf.summary <- df.TeachDrug.pf.summary %>% mutate(dv_binary = dv_binary*100) #turn into percentages
df.TeachDrug.pf.summary$genericCond <- factor(df.TeachDrug.pf.summary$genericCond, levels = c("A", "B", "AB")) #order condLabels to be: A, B, AB

pTeachDrugPF <- ggplot(df.TeachDrug.pf.summary, aes(x = genericCond, y = dv_binary)) +
  geom_bar(stat = 'identity', fill = 'grey38', color = 'grey38', width = .65) +
  theme_set(theme_classic(base_size = 16)) +
  ylab('% rating inappropriate') +
  xlab(NULL) +
  # ylim(0,50) +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = 'black', size = .5, linetype = 'solid')) +
  theme(axis.text = element_text(colour = 'black')) +
  scale_x_discrete(labels=c('A' = 'Drug\nA', 'B' = 'Drug\nB', 'AB' = 'A/B\nTest')) +
  ggtitle('C') + theme(plot.title = element_text(hjust = 0.05,size=30,margin = margin(t = 8, b = -30)))
# ggsave("Checklist.png", width = 7.0, height = 5, units = "in", dpi = 500)

#Plot all panels in a grid
pChecklistGrid <- grid.arrange(pDrugOrig,pTeachDrug,pTeachDrugPF, ncol = 3)
# ggsave("Figure2.png", plot = pChecklistGrid, width = 14.0, height = 4, units = "in", dpi = 500, path = "Output/")
ggsave("Figure2.pdf", plot = pChecklistGrid, device = "pdf", width = 7, height = 3, units = "in", dpi = 320, path = "Output/") #HighRes PNAS Width
ggsave("Figure2.tif", plot = pChecklistGrid, device = "tiff", width = 7, height = 3, units = "in", dpi = 320, path = "Output/") #HighRes PNAS Width

