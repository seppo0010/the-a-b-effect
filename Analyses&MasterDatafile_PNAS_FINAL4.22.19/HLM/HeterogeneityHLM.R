#The A/B Effect: Objecting to experiments that compare two unobjectionable policies or treatments
#HLM for heterogeneity of treatment effect (to produce figure in SI)

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
library(lme4) #for HLM
library(arm) #for extracting SE from coefficients
# library(merTools)

#read in the desired worksheet 
df.HLM <- read_excel("ABI_MasterDatafile.xlsx", sheet = "FullExperimentsOnly")
df.HLM <- filter(df.HLM,RepeatTurkerLabelCode != "1") #Exclude those with 1: participants anywhere in the dataset who took our very first survey AND another survey

# Keep only useful columns (here, grouping variables and the dv)
df.HLM <- df.HLM[,c(1:14)]

df.HLM$genericCond = factor(df.HLM$policyOrAB, levels = c("Policy", "AB Test")) #factor the baseline/treatment conditions for the HLM

hlm_model <- lme4::lmer(dv ~ 1 + policyOrAB + (policyOrAB|Scenario), data=df.HLM) #run model: Condition is fixed effect, Scenario is allowed to vary as a random effect (slope and intercept)
tidy(hlm_model) #see all model parameters

# # can plot the data to see what the model is doing. The intercept and slope for the (invisible) line connecting each pair of dots varies.
ggplot(fortify(hlm_model), aes(policyOrAB, dv, color=Scenario)) +
  stat_summary(fun.data=mean_se, geom="pointrange") +
  stat_summary(aes(y=.fitted), fun.y=mean, geom="line")

summary(hlm_model) #view model summary; see a strong positive effect of Condition.
fixef(hlm_model) #view the overall treatment effect (same as summary above)
ranef(hlm_model) #view the random effect of each Scenario (they vary widely)
coef(hlm_model) #view the coefficients for the difference between conditions within each scenario. Note that these coeffs are equal to the fixef + the ranef for each scenario.

se.fixef(hlm_model) #see standard error of treatment effect
se.ranef(hlm_model) #see standard errors for random effects coeffs
se.coef(hlm_model) #confirm that coefficient standard errors are based on random effects

hlm_table <- data.frame(coef(hlm_model)$Scenario) #make a dataframe of the treatment effect coeffs
hlm_table <- rownames_to_column(hlm_table) #add names so we can rename and operate
hlm_table <- hlm_table %>% dplyr::rename(Scenario=rowname) %>% dplyr::rename(Intercept=X.Intercept.) %>% dplyr::rename(Treatment=policyOrABPolicy) #rename columns

hlmCoef_se <- data.frame(se.coef(hlm_model)) #se.coef extracts the standard errors of each coefficient taking into account the random effects variance (see Gelman)
hlmCoef_se <- hlmCoef_se$Scenario.policyOrABPolicy #pull out only the random effects se within each Scenario

hlmCoef_CI <- hlmCoef_se*1.96 #to get 95% CI, multiply the random effects se by 1.96. 

#This is appropriate because we have enough degrees of freedom in each study to assume normally distributed t for each coeff (see below)
#https://stats.stackexchange.com/questions/117641/how-trustworthy-are-the-confidence-intervals-for-lmer-objects-through-effects-pa
#https://stats.stackexchange.com/questions/29981/should-confidence-intervals-for-linear-regression-coefficients-be-based-on-the-n
#Even Gelman says that HLM CI's are the way to go (as opposed to adjusting the width of these CI) (Gelman, Hill, & Yajima, 2012): 
# "Therefore, when doing social science or program evaluation, we do not recommend
# classical methods that alter p values or (equivalently) make confidence intervals wider.
# Instead, we prefer multilevel modeling, which shifts point estimates and their corresponding
# intervals closer to each other (that is, performs partial pooling) where necessary—especially
# when much of the variation in the data can be explained by noise."
#Confirmed that coefficient estimates in this HLM are closer to each other (and closer to the mean treatment effect) than in independent regressions within each Scenario.
#Similarly, the SE computed using random effects variance are indeed smaller than the SE computed from independent regressions. This produces slightly narrower and more precise CI (as Gelman summarized above)

#a second approach to CI would be to bootstrap a CI around the size of the fixed effect and then apply it to the treatment estimate within each Scenario. 
# But this approach ignores random effect variation within each Scenario (it just uses the r.e. variation over all)
# confint(hlm_model, level = 0.95,method = c("boot"),nsim = 500,boot.type = c("perc"),FUN = NULL,quiet = FALSE,oldNames = FALSE)

hlm_table <- cbind(hlm_table,hlmCoef_CI) #combine table of coeff estimates with the vector of 95% CI

hlm_table$Scenario <- factor(hlm_table$Scenario, c("Checklist","Checklist_exact","Checklist_vary","Checklist_pf" ,"Genetic Testing" ,"Autonomous Vehicles" ,"Retirement" ,"Recruit" ,"Charity" ,"Education","Basic Income" ,"Drug" , "DrugTeach" ,"DrugTeach_pf", "Checklist_Providers", "DrugTeach_Providers"),
                             levels = c("Checklist","Checklist_exact","Checklist_vary","Checklist_pf" ,"Genetic Testing" ,"Autonomous Vehicles" ,"Retirement" ,"Recruit" ,"Charity" ,"Education","Basic Income" ,"Drug" , "DrugTeach" ,"DrugTeach_pf", "Checklist_Providers", "DrugTeach_Providers"))

hlm_table$Scenario <- c("Safety Checklist",
                        "Safety Checklist Rep. (Exact)",
                        "Safety Checklist Rep. (Varied)",
                        "Safety Checklist Rep. (Mobile)",
                        "Genetic Testing",
                        "Autonomous Vehicles",
                        "Retirement Plans",
                        "Health Worker Recruitment",
                        "Poverty Alleviation",
                        "Teacher Wellbeing",
                        "Basic Income",
                        "Drug Effectiveness",
                        "Drug Effectiveness Walk-in",
                        "Drug Effectiveness Walk-in (Mobile)",
                        "Safety Checklist (Healthcare)",
                        "Drug Effectiveness Walk-in (Healthcare)")                                   

hlm_table %>%
  ggplot(aes(x = Scenario, y = Treatment)) +
  geom_pointrange(aes(ymin = Treatment-hlmCoef_CI, ymax = Treatment+hlmCoef_CI)) +
  geom_abline(intercept=0, slope=0,size=1) +
  geom_abline(intercept=fixef(hlm_model)[2], slope=0,size=1, linetype = "dashed") +
  theme_classic(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 12))  +
  theme(axis.text.y = element_text(color = "black", size = 12))  +
  ylab("Treatment Effect (Policy vs. AB Test)") +
  xlab("") +
  theme(plot.margin = margin(15, 5, 0, 30)) +
  scale_x_discrete(labels = c("Safety Checklist",
                              "Safety Checklist Rep. (Exact)",
                              "Safety Checklist Rep. (Varied)",
                              "Safety Checklist Rep. (Mobile)",
                              "Genetic Testing",
                              "Autonomous Vehicles",
                              "Retirement Plans",
                              "Health Worker Recruitment",
                              "Poverty Alleviation",
                              "Teacher Wellbeing",
                              "Basic Income",
                              "Drug Effectiveness",
                              "Drug Effectiveness Walk-in",
                              "Drug Effectiveness Walk-in (Mobile)",
                              "Safety Checklist (Healthcare)",
                              "Drug Effectiveness Walk-in (Healthcare)"))