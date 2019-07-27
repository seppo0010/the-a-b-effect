#The A/B Effect: Objecting to experiments that compare two unobjectionable policies or treatments
#Figure 3
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
df.Checklist <- filter(df, df$Scenario == "Checklist_Providers" ) #pull out checklist studies only
df.Drug <- filter(df, df$Scenario == "DrugTeach_Providers" ) #pull out checklist studies only

#PANEL A: first, plot checklist study
# df.Checklist <- filter(df.Checklist, df.Checklist$Scenario == "Checklist_Providers") #pull out original checklist experiment
df.Checklist.summary <- summarySE(df.Checklist, measurevar = "dv_binary", groupvars = "genericCond") #summarize proportion dv
df.Checklist.summary <- df.Checklist.summary %>% mutate(dv_binary = dv_binary*100) #turn into percentages
df.Checklist.summary$genericCond <- factor(df.Checklist.summary$genericCond, levels = c("A", "B", "AB")) #order condLabels to be: A, B, AB

pChecklist <- ggplot(df.Checklist.summary, aes(x = genericCond, y = dv_binary)) +
  geom_bar(stat = 'identity', fill = 'grey38', color = 'grey38', width = .65) +
  theme_set(theme_classic(base_size = 15)) +
  ylab('% rating inappropriate') +
  xlab(NULL) +
  # ylim(0,50) +
  scale_y_continuous(limits = c(0,60), expand = c(0,0)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = 'black', size = .5, linetype = 'solid')) +
  theme(axis.text = element_text(colour = 'black')) + theme(axis.ticks = element_line(size = .5)) +
  scale_x_discrete(labels=c('A' = 'Badge\n(A)', 'B' = 'Poster\n(B)', 'AB' = 'A/B\nTest')) +
  # ggtitle('Checklist (N = 226)') + theme(plot.title = element_text(hjust = 0.5,size=24,margin = margin(t = 10, b = -35))) +
  ggtitle('A') + theme(plot.title = element_text(hjust = 0.05,size=30,margin = margin(t = 10, b = -30)))

#PANEL B: next, plot drug study
# df.Drug <- filter(df.Checklist, df.Checklist$Scenario == "Checklist_exact") #pull out replication checklist experiment
df.Drug.summary <- summarySE(df.Drug, measurevar = "dv_binary", groupvars = "genericCond") #summarize proportion dv
df.Drug.summary <- df.Drug.summary %>% mutate(dv_binary = dv_binary*100) #turn into percentages
df.Drug.summary$genericCond <- factor(df.Drug.summary$genericCond, levels = c("A", "B", "AB")) #order condLabels to be: A, B, AB

pDrug <- ggplot(df.Drug.summary, aes(x = genericCond, y = dv_binary)) +
  geom_bar(stat = 'identity', fill = 'grey38', color = 'grey38', width = .65) +
  theme_set(theme_classic(base_size = 15)) +
  ylab('% rating inappropriate') +
  xlab(NULL) +
  # ylim(0,50) +
  scale_y_continuous(limits = c(0,60), expand = c(0,0)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = 'black', size = .5, linetype = 'solid')) +
  theme(axis.text = element_text(colour = 'black')) + theme(axis.ticks = element_line(size = .5)) +
  scale_x_discrete(labels=c('A' = 'Drug\nA', 'B' = 'Drug\nB', 'AB' = 'A/B\nTest')) +
  # ggtitle('Best Drug Walk-In (N = 231)') + theme(plot.title = element_text(hjust = 0.5,size=24,margin = margin(t = 10, b = -35))) +
  ggtitle('B') + theme(plot.title = element_text(hjust = 0.05,size=30,margin = margin(t = 10, b = -30)))

#Plot all panels in a grid
pGrid <- grid.arrange(pChecklist,pDrug, nrow = 1)
# ggsave("Figure3.png", plot = pGrid, width = 9.0, height = 4, units = "in", dpi = 500, path = "Output/")
ggsave("Figure3.pdf", plot = pGrid, device = "pdf", width = 4.75, height = 3, units = "in", dpi = 320, path = "Output/") #HighRes PNAS Width
ggsave("Figure3.tif", plot = pGrid, device = "tiff", width = 4.75, height = 3, units = "in", dpi = 320, path = "Output/") #HighRes PNAS Width
