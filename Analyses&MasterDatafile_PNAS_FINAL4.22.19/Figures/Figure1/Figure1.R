#The A/B Effect: Objecting to experiments that compare two unobjectionable policies or treatments
#Figure 1
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
df.Checklist <- filter(df, df$Scenario == "Checklist" | df$Scenario == "Checklist_vary" | df$Scenario == "Checklist_exact" | df$Scenario == "Checklist_pf" ) #pull out checklist studies only

#PANEL A: first, plot original checklist experiment
df.ChecklistOrig <- filter(df.Checklist, df.Checklist$Scenario == "Checklist") #pull out original checklist experiment
df.ChecklistOrig.summary <- summarySE(df.ChecklistOrig, measurevar = "dv_binary", groupvars = "genericCond") #summarize proportion dv
df.ChecklistOrig.summary <- df.ChecklistOrig.summary %>% mutate(dv_binary = dv_binary*100) #turn into percentages
df.ChecklistOrig.summary$genericCond <- factor(df.ChecklistOrig.summary$genericCond, levels = c("A", "B", "AB")) #order condLabels to be: A, B, AB

pChecklistOrig <- ggplot(df.ChecklistOrig.summary, aes(x = genericCond, y = dv_binary)) +
  geom_bar(stat = 'identity', fill = 'grey38', color = 'grey38', width = .65) +
  theme_set(theme_classic(base_size = 18)) +
  ylab('% rating inappropriate') +
  xlab(NULL) +
  # ylim(0,50) +
  scale_y_continuous(limits = c(0,50), expand = c(0,0)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = 'black', size = .5, linetype = 'solid')) +
  theme(axis.text = element_text(colour = 'black')) +
  scale_x_discrete(labels=c('A' = 'Badge', 'B' = 'Poster', 'AB' = 'A/B Test')) +
  ggtitle('A') + theme(plot.title = element_text(hjust = 0.05,size=30,margin = margin(t = 10, b = -30)))
  # ggsave("Checklist.png", width = 7.0, height = 5, units = "in", dpi = 500)

#PANEL B: next, plot exact replication of checklist experiment
df.ChecklistRep <- filter(df.Checklist, df.Checklist$Scenario == "Checklist_exact") #pull out replication checklist experiment
df.ChecklistRep.summary <- summarySE(df.ChecklistRep, measurevar = "dv_binary", groupvars = "genericCond") #summarize proportion dv
df.ChecklistRep.summary <- df.ChecklistRep.summary %>% mutate(dv_binary = dv_binary*100) #turn into percentages
df.ChecklistRep.summary$genericCond <- factor(df.ChecklistRep.summary$genericCond, levels = c("A", "B", "AB")) #order condLabels to be: A, B, AB

pChecklistRep <- ggplot(df.ChecklistRep.summary, aes(x = genericCond, y = dv_binary)) +
  geom_bar(stat = 'identity', fill = 'grey38', color = 'grey38', width = .65) +
  theme_set(theme_classic(base_size = 18)) +
  ylab('% rating inappropriate') +
  xlab(NULL) +
  # ylim(0,50) +
  scale_y_continuous(limits = c(0,50), expand = c(0,0)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = 'black', size = .5, linetype = 'solid')) +
  theme(axis.text = element_text(colour = 'black')) +
  scale_x_discrete(labels=c('A' = 'Badge', 'B' = 'Poster', 'AB' = 'A/B Test')) +
  ggtitle('B') + theme(plot.title = element_text(hjust = 0.05,size=30,margin = margin(t = 10, b = -30)))
# ggsave("Checklist.png", width = 7.0, height = 5, units = "in", dpi = 500)

#PANEL C: next, plot alternative vignette of checklist experiment
df.ChecklistAlt <- filter(df.Checklist, df.Checklist$Scenario == "Checklist_vary") #pull out alternative checklist experiment
df.ChecklistAlt.summary <- summarySE(df.ChecklistAlt, measurevar = "dv_binary", groupvars = "genericCond") #summarize proportion dv
df.ChecklistAlt.summary <- df.ChecklistAlt.summary %>% mutate(dv_binary = dv_binary*100) #turn into percentages
df.ChecklistAlt.summary$genericCond <- factor(df.ChecklistAlt.summary$genericCond, levels = c("A", "B", "AB")) #order condLabels to be: A, B, AB

pChecklistAlt <- ggplot(df.ChecklistAlt.summary, aes(x = genericCond, y = dv_binary)) +
  geom_bar(stat = 'identity', fill = 'grey38', color = 'grey38', width = .65) +
  theme_set(theme_classic(base_size = 18)) +
  ylab('% rating inappropriate') +
  xlab(NULL) +
  # ylim(0,50) +
  scale_y_continuous(limits = c(0,50), expand = c(0,0)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = 'black', size = .5, linetype = 'solid')) +
  theme(axis.text = element_text(colour = 'black')) +
  scale_x_discrete(labels=c('A' = 'Badge', 'B' = 'Poster', 'AB' = 'A/B Test')) +
  ggtitle('C') + theme(plot.title = element_text(hjust = 0.05,size=30,margin = margin(t = 10, b = -30))) +
  scale_fill_manual(values=c("#666666","white","white")) 
# ggsave("Checklist.png", width = 7.0, height = 5, units = "in", dpi = 500)

#PANEL D: finally, plot pollfish replication of checklist experiment
df.ChecklistPF <- filter(df.Checklist, df.Checklist$Scenario == "Checklist_pf") #pull out pollfish checklist experiment
df.ChecklistPF.summary <- summarySE(df.ChecklistPF, measurevar = "dv_binary", groupvars = "genericCond") #summarize proportion dv
df.ChecklistPF.summary <- df.ChecklistPF.summary %>% mutate(dv_binary = dv_binary*100) #turn into percentages
df.ChecklistPF.summary$genericCond <- factor(df.ChecklistPF.summary$genericCond, levels = c("A", "B", "AB")) #order condLabels to be: A, B, AB

pChecklistPF <- ggplot(df.ChecklistPF.summary, aes(x = genericCond, y = dv_binary)) +
  geom_bar(stat = 'identity', fill = 'grey38', color = 'grey38', width = .65) +
  theme_set(theme_classic(base_size = 18)) +
  ylab('% rating inappropriate') +
  xlab(NULL) +
  # ylim(0,50) +
  scale_y_continuous(limits = c(0,50), expand = c(0,0)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = 'black', size = .5, linetype = 'solid')) +
  theme(axis.text = element_text(colour = 'black')) +
  scale_x_discrete(labels=c('A' = 'Badge', 'B' = 'Poster', 'AB' = 'A/B Test')) +
  ggtitle('D') + theme(plot.title = element_text(hjust = 0.05,size=30,margin = margin(t = 10, b = -30)))
# ggsave("Checklist.png", width = 7.0, height = 5, units = "in", dpi = 500)

#Plot all panels in a grid
pChecklistGrid <- grid.arrange(pChecklistOrig,pChecklistRep,pChecklistAlt,pChecklistPF)
# ggsave("Figure1.png", plot = pChecklistGrid, width = 9.0, height = 7.5, units = "in", dpi = 500, path = "Output/")
# ggsave("Figure1.pdf", plot = pChecklistGrid, width = 7, units = "in", dpi = 1000, path = "Output/") #HighRes PNAS Width
ggsave("Figure1.tif", plot = pChecklistGrid, device = "tiff", width = 7, units = "in", dpi = 320, path = "Output/") #HighRes PNAS Width
ggsave("Figure1.pdf", plot = pChecklistGrid, device = "pdf", width = 7, units = "in", dpi = 320, path = "Output/") #HighRes PNAS Width

