####------------------------------------------------------------------------- 
#### 
####            R Code to analyze the data for the article:
####                         
####    Predicting Actual Social Skill Expression From Self-Concept Questionnaires 
####   
####
####   Simon M. Breil 1, Ina Mielke, Helmut Ahrens, Thomas Geldmacher, Janina Sensmeier, 
####              Bernhard Marschall, and Mitja D. Back
#### 
####        This code is licensed under a CC-BY4.0 license
####      (see https://creativecommons.org/licenses/by/4.0/)
####
####------------------------------------------------------------------------- -

#load necessary packages
library(readxl)
library(dplyr)
library(psych)
library(effsize)
library(GPArotation)
library(lavaan)

#For an overview of all variables see Codebook on https://osf.io/74qnu/

#get Data
#setwd("") #please set own working directory
Data <- read_excel("Anonymzed_Data_Predicting_Social_Skill_Expression.xlsx", na = "NA")
#please note that the age variable is not included here to keep anonymization of participants.

#Data Preparation####

#Big Five Scores
#Recode reversed Items. Items that need to be reversed are those with "_R"
Data$BFI01 <- 6 - Data$BFI01_R
Data$BFI03 <- 6 - Data$BFI03_R
Data$BFI07 <- 6 - Data$BFI07_R
Data$BFI08 <- 6 - Data$BFI08_R
Data$BFI10 <- 6 - Data$BFI10_R
Data$BFI14 <- 6 - Data$BFI14_R
Data$BFI17 <- 6 - Data$BFI17_R
Data$BFI19 <- 6 - Data$BFI19_R
Data$BFI20 <- 6 - Data$BFI20_R
Data$BFI21 <- 6 - Data$BFI21_R
Data$BFI24 <- 6 - Data$BFI24_R
Data$BFI26 <- 6 - Data$BFI26_R
Data$BFI27 <- 6 - Data$BFI27_R
Data$BFI28 <- 6 - Data$BFI28_R
Data$BFI30 <- 6 - Data$BFI30_R

#Create Big Five Scales. E = Extraversion, A = Agreeableness, N = Negative emotionality
Data$BFI_E <- rowMeans (select(Data,BFI01,BFI06,BFI11,BFI16,BFI21,BFI26))
Data$BFI_A <- rowMeans (select(Data,BFI02,BFI07,BFI12,BFI17,BFI22,BFI27))
Data$BFI_N <- rowMeans (select(Data,BFI04,BFI09,BFI14,BFI19,BFI24,BFI29))

#Build Big Five Facet Scores
#Extraversion: S = Sociability, A = Assertiveness, E = Energy level
Data$BFI_E_S <- rowMeans (select(Data,BFI01,BFI16))
Data$BFI_E_A <- rowMeans (select(Data,BFI06,BFI21))
Data$BFI_E_E <- rowMeans (select(Data,BFI11,BFI26))

#Agreeableness: C = Compassion, R = Respectfulness, T = Trust
Data$BFI_A_C <- rowMeans (select(Data,BFI02,BFI17))
Data$BFI_A_R <- rowMeans (select(Data,BFI07,BFI22))
Data$BFI_A_T <- rowMeans (select(Data,BFI12,BFI27))

#Negative emotionality: A = Anxiety, D = Depression, E = Emotional volatility
Data$BFI_N_A <- rowMeans (select(Data,BFI04,BFI19))
Data$BFI_N_D <- rowMeans (select(Data,BFI09,BFI24))
Data$BFI_N_V <- rowMeans (select(Data,BFI14,BFI29))

#Create BESSI Scales
#Leadership skill
Data$BESSI_Lead <- rowMeans (select(Data,Lead1,Lead2,Lead3,Lead4,Lead5,Lead6))
#Persuasion skill with alternative translations
Data$BESSI_Pers <- rowMeans (select(Data,Pers1,Pers2,Pers3,Pers4,Pers5,Pers6a))

#Perspective-taking skill
Data$BESSI_Empa <- rowMeans (select(Data,Empa1,Empa2,Empa3,Empa4,Empa5,Empa6))
#Capacity for social warmth
Data$BESSI_Soha <- rowMeans (select(Data,Soha1,Soha2,Soha3,Soha4,Soha5,Soha6))

#Stress regulation
Data$BESSI_Stres <- rowMeans (select(Data,Stres1,Stres2,Stres3,Stres4,Stres5,Stres6))
#Anger management
Data$BESSI_Angm <- rowMeans (select(Data,Angm1,Angm2,Angm3,Angm4,Angm5,Angm6))

#Agency skill
Data$BESSI_Agency <- rowMeans (select(Data,BESSI_Lead,BESSI_Pers))

#Communion skill
Data$BESSI_Communion <- rowMeans (select(Data,BESSI_Empa,BESSI_Soha))

#Resilience skill
Data$BESSI_Resilience <- rowMeans (select(Data,BESSI_Stres,BESSI_Angm))

#Observed skills - aggregate across raters
Data$Agency_observed <- rowMeans (select(Data,Agency_Observed_R1,Agency_Observed_R2),na.rm =T)
Data$Communion_observed <- rowMeans (select(Data,Communion_Observed_R1,Communion_Observed_R2),na.rm =T)
Data$Resilience_observed <- rowMeans (select(Data,Resilience_Observed_R1,Resilience_Observed_R2,Resilience_Observed_R3),na.rm =T)

#Analysis####
#Age and gender
describe(Data$Age) #Age not included to keep anonymization
table(Data$Gender)

##Table 3 Means, SDs, Reliabilities, and Correlations####
describe(select(Data, Agency_observed,Agency_self,BFI_E,BFI_E_S, BFI_E_A, BFI_E_E, BESSI_Agency, BESSI_Lead, BESSI_Pers))
ICC(na.omit(select(Data,Agency_Observed_R1,Agency_Observed_R2))) #.85      
omega(na.omit(select(Data,BFI01,BFI06,BFI11,BFI16,BFI21,BFI26))) #.86
omega(na.omit(select(Data,BFI01,BFI16)),nfactors = 2) #.54
omega(na.omit(select(Data,BFI06,BFI21)),nfactors = 2) #.67
omega(na.omit(select(Data,BFI11,BFI26)),nfactors = 2) #.71
omega(na.omit(select(Data,Lead1,Lead2,Lead3,Lead4,Lead5,Lead6,Pers1,Pers2,Pers3,Pers4,Pers5,Pers6a))) #.93
omega(na.omit(select(Data,Lead1,Lead2,Lead3,Lead4,Lead5,Lead6))) #.92
omega(na.omit(select(Data,Pers1,Pers2,Pers3,Pers4,Pers5,Pers6a))) #.91
corr.test (select(Data, Agency_observed,Agency_self,BFI_E,BFI_E_S, BFI_E_A, BFI_E_E, BESSI_Agency, BESSI_Lead, BESSI_Pers))

#specific correlations
cor.test (Data$Agency_observed, Data$BFI_E)
cor.test (Data$Agency_observed, Data$BESSI_Agency)
cor.test (Data$Agency_self, Data$BFI_E)
cor.test (Data$Agency_self, Data$BESSI_Agency)
cor.test (Data$Agency_observed, Data$Agency_self)

##Table 4 Means, SDs, Reliabilities, and Correlations####
describe(select(Data, Communion_observed,Communion_self,BFI_A,BFI_A_C, BFI_A_R, BFI_A_T, BESSI_Communion, BESSI_Empa, BESSI_Soha))
ICC(na.omit(select(Data,Communion_Observed_R1,Communion_Observed_R2))) #.90      
omega(na.omit(select(Data,BFI02,BFI07,BFI12,BFI17,BFI22,BFI27))) #.85
omega(na.omit(select(Data,BFI02,BFI17)),nfactors = 2) #.29
omega(na.omit(select(Data,BFI07,BFI22)),nfactors = 2) #.64
omega(na.omit(select(Data,BFI12,BFI27)),nfactors = 2) #.33
omega(na.omit(select(Data,Empa1,Empa2,Empa3,Empa4,Empa5,Empa6,Soha1,Soha2,Soha3,Soha4,Soha5,Soha6))) #.91
omega(na.omit(select(Data,Empa1,Empa2,Empa3,Empa4,Empa5,Empa6))) #.93
omega(na.omit(select(Data,Soha1,Soha2,Soha3,Soha4,Soha5,Soha6))) #.87
corr.test (select(Data, Communion_observed,Communion_self,BFI_A,BFI_A_C, BFI_A_R, BFI_A_T, BESSI_Communion, BESSI_Empa, BESSI_Soha))
#specific correlations
cor.test (Data$Communion_observed, Data$BFI_A)
cor.test (Data$Communion_observed, Data$BESSI_Communion)
cor.test (Data$Communion_self, Data$BFI_A)
cor.test (Data$Communion_self, Data$BESSI_Communion)
cor.test (Data$Communion_observed, Data$Communion_self)

##Table 5 Means, SDs, Reliabilities, and Correlations####
describe(select(Data, Resilience_observed,Resilience_self,BFI_N,BFI_N_A, BFI_N_D, BFI_N_V, BESSI_Resilience, BESSI_Stres, BESSI_Angm))
ICC(na.omit(select(Data,Resilience_Observed_R1,Resilience_Observed_R2))) #.89      
omega(na.omit(select(Data,BFI04,BFI09,BFI14,BFI19,BFI24,BFI29))) #.85
omega(na.omit(select(Data,BFI04,BFI19)),nfactors = 2) #.61
omega(na.omit(select(Data,BFI09,BFI24)),nfactors = 2) #.58
omega(na.omit(select(Data,BFI14,BFI29)),nfactors = 2) #.60
omega(na.omit(select(Data,Stres1,Stres2,Stres3,Stres4,Stres5,Stres6,Angm1,Angm2,Angm3,Angm4,Angm5,Angm6))) #.91
omega(na.omit(select(Data,Stres1,Stres2,Stres3,Stres4,Stres5,Stres6))) #.91
omega(na.omit(select(Data,Angm1,Angm2,Angm3,Angm4,Angm5,Angm6))) #.90
corr.test (select(Data, Resilience_observed,Resilience_self,BFI_N,BFI_N_A, BFI_N_D, BFI_N_V, BESSI_Resilience, BESSI_Stres, BESSI_Angm))
cor.test (Data$BFI_N, Data$Resilience_self) #check to see if below p < .05
#specific correlations
cor.test (Data$Resilience_observed, Data$BFI_N)
cor.test (Data$Resilience_observed, Data$BESSI_Resilience)
cor.test (Data$Resilience_self, Data$BFI_N)
cor.test (Data$Resilience_self, Data$BESSI_Resilience)
cor.test (Data$Resilience_observed, Data$Resilience_self)

#Relationships Across Skills Observer-rated
Ag_Com <- fisherz(cor(Data$Agency_observed,Data$Communion_observed,use = "complete.obs"))
Rel_Com <- fisherz(cor(Data$Resilience_observed,Data$Communion_observed,use = "complete.obs"))
Ag_Rel <- fisherz(cor(Data$Agency_observed,Data$Resilience_observed,use = "complete.obs"))
fisherz2r((Ag_Com+Rel_Com+Ag_Rel)/3)

#Relationships Across Skills Self-rated
Ag_Com <- fisherz(cor(Data$Agency_self,Data$Communion_self,use = "complete.obs"))
Rel_Com <- fisherz(cor(Data$Resilience_self,Data$Communion_self,use = "complete.obs"))
Ag_Rel <- fisherz(cor(Data$Agency_self,Data$Resilience_self,use = "complete.obs"))
fisherz2r((Ag_Com+Rel_Com+Ag_Rel)/3)



##Table 6 and Online Supplement Table S5####
#Agency: effects, p-values, and confidence intervals
Bs <- (lm(scale(Agency_observed) ~ scale(BESSI_Agency) + scale(BFI_E), Data))$coefficients[2:3]
Ps <- summary(lm(scale(Agency_observed) ~ scale(BESSI_Agency) + scale(BFI_E), Data))$coefficients[2:3,4]  
Conf <- t(confint(lm(scale(Agency_observed) ~ scale(BESSI_Agency) + scale(BFI_E), Data))[2:3,])
M1 <- rbind(Bs,Ps,Conf)

Bs <- (lm(scale(Agency_observed) ~ scale(BESSI_Lead)+ scale(BFI_E_S)+ scale(BFI_E_A)+ scale(BFI_E_E), Data))$coefficients[2:5]
Ps <- summary(lm(scale(Agency_observed) ~ scale(BESSI_Lead)+ scale(BFI_E_S)+ scale(BFI_E_A)+ scale(BFI_E_E), Data))$coefficients[2:5,4]  
Conf <- t(confint(lm(scale(Agency_observed) ~ scale(BESSI_Lead)+ scale(BFI_E_S)+ scale(BFI_E_A)+ scale(BFI_E_E), Data))[2:5,])
M2 <- rbind(Bs,Ps,Conf)

Bs <- (lm(scale(Agency_observed) ~ scale(BESSI_Pers)+ scale(BFI_E_S)+ scale(BFI_E_A)+ scale(BFI_E_E), Data))$coefficients[2:5]
Ps <- summary(lm(scale(Agency_observed) ~ scale(BESSI_Pers)+ scale(BFI_E_S)+ scale(BFI_E_A)+ scale(BFI_E_E), Data))$coefficients[2:5,4]  
Conf <- t(confint(lm(scale(Agency_observed) ~ scale(BESSI_Pers)+ scale(BFI_E_S)+ scale(BFI_E_A)+ scale(BFI_E_E), Data))[2:5,])
M3 <- rbind(Bs,Ps,Conf)

Bs <- (lm(scale(Agency_self) ~ scale(BESSI_Agency) + scale(BFI_E), Data))$coefficients[2:3]
Ps <- summary(lm(scale(Agency_self) ~ scale(BESSI_Agency) + scale(BFI_E), Data))$coefficients[2:3,4]  
Conf <- t(confint(lm(scale(Agency_self) ~ scale(BESSI_Agency) + scale(BFI_E), Data))[2:3,])
M4 <- rbind(Bs,Ps,Conf)

Bs <- (lm(scale(Agency_self) ~ scale(BESSI_Lead)+ scale(BFI_E_S)+ scale(BFI_E_A)+ scale(BFI_E_E), Data))$coefficients[2:5]
Ps <- summary(lm(scale(Agency_self) ~ scale(BESSI_Lead)+ scale(BFI_E_S)+ scale(BFI_E_A)+ scale(BFI_E_E), Data))$coefficients[2:5,4]  
Conf <- t(confint(lm(scale(Agency_self) ~ scale(BESSI_Lead)+ scale(BFI_E_S)+ scale(BFI_E_A)+ scale(BFI_E_E), Data))[2:5,])
M5 <- rbind(Bs,Ps,Conf)

Bs <- (lm(scale(Agency_self) ~ scale(BESSI_Pers)+ scale(BFI_E_S)+ scale(BFI_E_A)+ scale(BFI_E_E), Data))$coefficients[2:5]
Ps <- summary(lm(scale(Agency_self) ~ scale(BESSI_Pers)+ scale(BFI_E_S)+ scale(BFI_E_A)+ scale(BFI_E_E), Data))$coefficients[2:5,4]  
Conf <- t(confint(lm(scale(Agency_self) ~ scale(BESSI_Pers)+ scale(BFI_E_S)+ scale(BFI_E_A)+ scale(BFI_E_E), Data))[2:5,])
M6 <- rbind(Bs,Ps,Conf)

#Communion: effects, p-values, and confidence intervals
Bs <- (lm(scale(Communion_observed) ~ scale(BESSI_Communion) + scale(BFI_A), Data))$coefficients[2:3]
Ps <- summary(lm(scale(Communion_observed) ~ scale(BESSI_Communion) + scale(BFI_A), Data))$coefficients[2:3,4]  
Conf <- t(confint(lm(scale(Communion_observed) ~ scale(BESSI_Communion) + scale(BFI_A), Data))[2:3,])
M7 <- rbind(Bs,Ps,Conf)

Bs <- (lm(scale(Communion_observed) ~ scale(BESSI_Empa)+ scale(BFI_A_C)+ scale(BFI_A_R)+ scale(BFI_A_T), Data))$coefficients[2:5]
Ps <- summary(lm(scale(Communion_observed) ~ scale(BESSI_Empa)+ scale(BFI_A_C)+ scale(BFI_A_R)+ scale(BFI_A_T), Data))$coefficients[2:5,4]  
Conf <- t(confint(lm(scale(Communion_observed) ~ scale(BESSI_Empa)+ scale(BFI_A_C)+ scale(BFI_A_R)+ scale(BFI_A_T), Data))[2:5,])
M8 <- rbind(Bs,Ps,Conf)

Bs <- (lm(scale(Communion_observed) ~ scale(BESSI_Soha)+ scale(BFI_A_C)+ scale(BFI_A_R)+ scale(BFI_A_T), Data))$coefficients[2:5]
Ps <- summary(lm(scale(Communion_observed) ~ scale(BESSI_Soha)+ scale(BFI_A_C)+ scale(BFI_A_R)+ scale(BFI_A_T), Data))$coefficients[2:5,4]  
Conf <- t(confint(lm(scale(Communion_observed) ~ scale(BESSI_Soha)+ scale(BFI_A_C)+ scale(BFI_A_R)+ scale(BFI_A_T), Data))[2:5,])
M9 <- rbind(Bs,Ps,Conf)

Bs <- (lm(scale(Communion_self) ~ scale(BESSI_Communion) + scale(BFI_A), Data))$coefficients[2:3]
Ps <- summary(lm(scale(Communion_self) ~ scale(BESSI_Communion) + scale(BFI_A), Data))$coefficients[2:3,4]  
Conf <- t(confint(lm(scale(Communion_self) ~ scale(BESSI_Communion) + scale(BFI_A), Data))[2:3,])
M10 <- rbind(Bs,Ps,Conf)

Bs <- (lm(scale(Communion_self) ~ scale(BESSI_Empa)+ scale(BFI_A_C)+ scale(BFI_A_R)+ scale(BFI_A_T), Data))$coefficients[2:5]
Ps <- summary(lm(scale(Communion_self) ~ scale(BESSI_Empa)+ scale(BFI_A_C)+ scale(BFI_A_R)+ scale(BFI_A_T), Data))$coefficients[2:5,4]  
Conf <- t(confint(lm(scale(Communion_self) ~ scale(BESSI_Empa)+ scale(BFI_A_C)+ scale(BFI_A_R)+ scale(BFI_A_T), Data))[2:5,])
M11 <- rbind(Bs,Ps,Conf)

Bs <- (lm(scale(Communion_self) ~ scale(BESSI_Soha)+ scale(BFI_A_C)+ scale(BFI_A_R)+ scale(BFI_A_T), Data))$coefficients[2:5]
Ps <- summary(lm(scale(Communion_self) ~ scale(BESSI_Soha)+ scale(BFI_A_C)+ scale(BFI_A_R)+ scale(BFI_A_T), Data))$coefficients[2:5,4]  
Conf <- t(confint(lm(scale(Communion_self) ~ scale(BESSI_Soha)+ scale(BFI_A_C)+ scale(BFI_A_R)+ scale(BFI_A_T), Data))[2:5,])
M12 <- rbind(Bs,Ps,Conf)


#Resilience: effects, p-values, and confidence intervals
Bs <- (lm(scale(Resilience_observed) ~ scale(BESSI_Resilience) + scale(BFI_N), Data))$coefficients[2:3]
Ps <- summary(lm(scale(Resilience_observed) ~ scale(BESSI_Resilience) + scale(BFI_N), Data))$coefficients[2:3,4]  
Conf <- t(confint(lm(scale(Resilience_observed) ~ scale(BESSI_Resilience) + scale(BFI_N), Data))[2:3,])
M13 <- rbind(Bs,Ps,Conf)

Bs <- (lm(scale(Resilience_observed) ~ scale(BESSI_Stres)+ scale(BFI_N_A)+ scale(BFI_N_D)+ scale(BFI_N_V), Data))$coefficients[2:5]
Ps <- summary(lm(scale(Resilience_observed) ~ scale(BESSI_Stres)+ scale(BFI_N_A)+ scale(BFI_N_D)+ scale(BFI_N_V), Data))$coefficients[2:5,4]  
Conf <- t(confint(lm(scale(Resilience_observed) ~ scale(BESSI_Stres)+ scale(BFI_N_A)+ scale(BFI_N_D)+ scale(BFI_N_V), Data))[2:5,])
M14 <- rbind(Bs,Ps,Conf)

Bs <- (lm(scale(Resilience_observed) ~ scale(BESSI_Angm)+ scale(BFI_N_A)+ scale(BFI_N_D)+ scale(BFI_N_V), Data))$coefficients[2:5]
Ps <- summary(lm(scale(Resilience_observed) ~ scale(BESSI_Angm)+ scale(BFI_N_A)+ scale(BFI_N_D)+ scale(BFI_N_V), Data))$coefficients[2:5,4]  
Conf <- t(confint(lm(scale(Resilience_observed) ~ scale(BESSI_Angm)+ scale(BFI_N_A)+ scale(BFI_N_D)+ scale(BFI_N_V), Data))[2:5,])
M15 <- rbind(Bs,Ps,Conf)

Bs <- (lm(scale(Resilience_self) ~ scale(BESSI_Resilience) + scale(BFI_N), Data))$coefficients[2:3]
Ps <- summary(lm(scale(Resilience_self) ~ scale(BESSI_Resilience) + scale(BFI_N), Data))$coefficients[2:3,4]  
Conf <- t(confint(lm(scale(Resilience_self) ~ scale(BESSI_Resilience) + scale(BFI_N), Data))[2:3,])
M16 <- rbind(Bs,Ps,Conf)

Bs <- (lm(scale(Resilience_self) ~ scale(BESSI_Stres)+ scale(BFI_N_A)+ scale(BFI_N_D)+ scale(BFI_N_V), Data))$coefficients[2:5]
Ps <- summary(lm(scale(Resilience_self) ~ scale(BESSI_Stres)+ scale(BFI_N_A)+ scale(BFI_N_D)+ scale(BFI_N_V), Data))$coefficients[2:5,4]  
Conf <- t(confint(lm(scale(Resilience_self) ~ scale(BESSI_Stres)+ scale(BFI_N_A)+ scale(BFI_N_D)+ scale(BFI_N_V), Data))[2:5,])
M17 <- rbind(Bs,Ps,Conf)

Bs <- (lm(scale(Resilience_self) ~ scale(BESSI_Angm)+ scale(BFI_N_A)+ scale(BFI_N_D)+ scale(BFI_N_V), Data))$coefficients[2:5]
Ps <- summary(lm(scale(Resilience_self) ~ scale(BESSI_Angm)+ scale(BFI_N_A)+ scale(BFI_N_D)+ scale(BFI_N_V), Data))$coefficients[2:5,4]  
Conf <- t(confint(lm(scale(Resilience_self) ~ scale(BESSI_Angm)+ scale(BFI_N_A)+ scale(BFI_N_D)+ scale(BFI_N_V), Data))[2:5,])
M18 <- rbind(Bs,Ps,Conf)

Observer <- as.data.frame(t(data.frame(M1,M2,M3,M7,M8,M9,M13,M14,M15)))
Observer <- Observer %>% mutate_if(is.numeric, round, digits=2)
write.table(Observer, "Observer.csv", sep=";",dec=".",row.names = T)

Self <- as.data.frame(t(data.frame(M4,M5,M6,M10,M11,M12,M16,M17,M18)))
Self <- Self %>% mutate_if(is.numeric, round, digits=2)
write.table(Self, "Self.csv", sep=";",dec=".",row.names = T)

#Online Supplement S1####
#EFA with all 36 BESSI items
items <- Data[c("Lead1","Lead2","Lead3","Lead4","Lead5","Lead6","Pers1","Pers2","Pers3","Pers4","Pers5","Pers6a",
                "Empa1","Empa2","Empa3","Empa4","Empa5","Empa6","Soha1","Soha2","Soha3","Soha4","Soha5","Soha6",
                "Stres1","Stres2","Stres3","Stres4","Stres5","Stres6","Angm1","Angm2","Angm3","Angm4","Angm5","Angm6")]
# Parallel Analysis
fa.parallel(items,fa="fa") #pa suggests 4 factors, scree plot rather 3 factors
# Factor Anaylsis with 3 factors
factors <- fa(items, nfactors = 3) #each factor seems to be represented by two BESSI scales of the same dimension
factors
#Online Supplement S2####
#Please note that age was not included to keep anonymization
#Control analyses S2 - Agency-Observed
summary (lm(scale(Agency_observed) ~ scale(Agency_self) + Gender + Age, Data))
summary (lm(scale(Agency_observed) ~ scale(BFI_E) + Gender + Age, Data))
summary (lm(scale(Agency_observed) ~ scale(BFI_E_S) + Gender + Age, Data))
summary (lm(scale(Agency_observed) ~ scale(BFI_E_A) + Gender + Age, Data))
summary (lm(scale(Agency_observed) ~ scale(BFI_E_E) + Gender + Age, Data))
summary (lm(scale(Agency_observed) ~ scale(BESSI_Agency) + Gender + Age, Data))
summary (lm(scale(Agency_observed) ~ scale(BESSI_Lead) + Gender + Age, Data))
summary (lm(scale(Agency_observed) ~ scale(BESSI_Pers) + Gender + Age, Data))

#Control analyses (gender and age) - Communion-Observed
summary (lm(scale(Communion_observed) ~ scale(Communion_self) + Gender + Age, Data))
summary (lm(scale(Communion_observed) ~ scale(BFI_A) + Gender + Age, Data))
summary (lm(scale(Communion_observed) ~ scale(BFI_A_C) + Gender + Age, Data))
summary (lm(scale(Communion_observed) ~ scale(BFI_A_R) + Gender + Age, Data))
summary (lm(scale(Communion_observed) ~ scale(BFI_A_T) + Gender + Age, Data))
summary (lm(scale(Communion_observed) ~ scale(BESSI_Communion) + Gender + Age, Data))
summary (lm(scale(Communion_observed) ~ scale(BESSI_Empa) + Gender + Age, Data))
summary (lm(scale(Communion_observed) ~ scale(BESSI_Soha) + Gender + Age, Data))

#Control analyses (gender and age) - Resilience-Observed
summary (lm(scale(Resilience_observed) ~ scale(Resilience_self) + Gender + Age, Data))
summary (lm(scale(Resilience_observed) ~ scale(BFI_N) + Gender + Age, Data))
summary (lm(scale(Resilience_observed) ~ scale(BFI_N_A) + Gender + Age, Data))
summary (lm(scale(Resilience_observed) ~ scale(BFI_N_D) + Gender + Age, Data))
summary (lm(scale(Resilience_observed) ~ scale(BFI_N_V) + Gender + Age, Data))
summary (lm(scale(Resilience_observed) ~ scale(BESSI_Resilience) + Gender + Age, Data))
summary (lm(scale(Resilience_observed) ~ scale(BESSI_Stres) + Gender + Age, Data))
summary (lm(scale(Resilience_observed) ~ scale(BESSI_Angm) + Gender + Age, Data))

#Control analyses (gender and age) - Agency-Self
summary (lm(scale(Agency_self) ~ scale(BFI_E) + Gender + Age, Data))
summary (lm(scale(Agency_self) ~ scale(BFI_E_S) + Gender + Age, Data))
summary (lm(scale(Agency_self) ~ scale(BFI_E_A) + Gender + Age, Data))
summary (lm(scale(Agency_self) ~ scale(BFI_E_E) + Gender + Age, Data))
summary (lm(scale(Agency_self) ~ scale(BESSI_Agency) + Gender + Age, Data))
summary (lm(scale(Agency_self) ~ scale(BESSI_Lead) + Gender + Age, Data))
summary (lm(scale(Agency_self) ~ scale(BESSI_Pers) + Gender + Age, Data))

#Control analyses (gender and age) - Communion-Self
summary (lm(scale(Communion_self) ~ scale(BFI_A) + Gender + Age, Data))
summary (lm(scale(Communion_self) ~ scale(BFI_A_C) + Gender + Age, Data))
summary (lm(scale(Communion_self) ~ scale(BFI_A_R) + Gender + Age, Data))
summary (lm(scale(Communion_self) ~ scale(BFI_A_T) + Gender + Age, Data)) #Significant Gender Effect
summary (lm(scale(Communion_self) ~ scale(BESSI_Communion) + Gender + Age, Data))
summary (lm(scale(Communion_self) ~ scale(BESSI_Empa) + Gender + Age, Data))
summary (lm(scale(Communion_self) ~ scale(BESSI_Soha) + Gender + Age, Data))

#Control analyses (gender and age) - Resilience-Self
summary (lm(scale(Resilience_self) ~ scale(BFI_N) + Gender + Age, Data)) #Controlling for Gender no effect of negative emotionality on self-rating resilience
summary (lm(scale(Resilience_self) ~ scale(BFI_N_A) + Gender + Age, Data))
summary (lm(scale(Resilience_self) ~ scale(BFI_N_D) + Gender + Age, Data))
summary (lm(scale(Resilience_self) ~ scale(BFI_N_V) + Gender + Age, Data))
summary (lm(scale(Resilience_self) ~ scale(BESSI_Resilience) + Gender + Age, Data))
summary (lm(scale(Resilience_self) ~ scale(BESSI_Stres) + Gender + Age, Data))
summary (lm(scale(Resilience_self) ~ scale(BESSI_Angm) + Gender + Age, Data))

#Get effects for gender
Agency_observed <- t.test (Agency_observed ~ Gender, Data)$estimate
Agency_self <- t.test (Agency_self ~ Gender, Data)$estimate
Communion_observed <- t.test (Communion_observed ~ Gender, Data)$estimate
Communion_self <- t.test (Communion_self ~ Gender, Data)$estimate
Resilience_observed <- t.test (Resilience_observed ~ Gender, Data)$estimate
Resilience_self <- t.test (Resilience_self ~ Gender, Data)$estimate
Means <- rbind (Agency_observed, Agency_self,Communion_observed,Communion_self,Resilience_observed,Resilience_self)

Agency_observed_p <- t.test (Agency_observed ~ Gender, Data)$p.value
Agency_self_p <- t.test (Agency_self ~ Gender, Data)$p.value
Communion_observed_p <- t.test (Communion_observed ~ Gender, Data)$p.value
Communion_self_p <- t.test (Communion_self ~ Gender, Data)$p.value
Resilience_observed_p <- t.test (Resilience_observed ~ Gender, Data)$p.value
Resilience_self_p <- t.test (Resilience_self ~ Gender, Data)$p.value
Ps <- rbind (Agency_observed_p, Agency_self_p,Communion_observed_p,Communion_self_p,Resilience_observed_p,Resilience_self_p)

Data$Gender <- as.factor (Data$Gender)
Agency_observed_d <- effsize::cohen.d (Agency_observed ~ Gender, Data)$estimate
Agency_self_d <- effsize::cohen.d (Agency_self ~ Gender, Data)$estimate
Communion_observed_d <- effsize::cohen.d (Communion_observed ~ Gender, Data)$estimate
Communion_self_d <- effsize::cohen.d (Communion_self ~ Gender, Data)$estimate
Resilience_observed_d <- effsize::cohen.d (Resilience_observed ~ Gender, Data)$estimate
Resilience_self_d <- effsize::cohen.d (Resilience_self ~ Gender, Data)$estimate
Ds <- rbind (Agency_observed_d, Agency_self_d,Communion_observed_d,Communion_self_d,Resilience_observed_d,Resilience_self_d)

Agency_observed_r <- cor (Data$Agency_observed, Data$Age,use="complete.obs")
Agency_self_r <- cor (Data$Agency_self, Data$Age,use="complete.obs")
Communion_observed_r <- cor (Data$Communion_observed, Data$Age,use="complete.obs")
Communion_self_r <- cor (Data$Communion_self, Data$Age,use="complete.obs")
Resilience_observed_r <- cor (Data$Resilience_observed, Data$Age,use="complete.obs")
Resilience_self_r <- cor (Data$Resilience_self, Data$Age,use="complete.obs")
Rs <- rbind (Agency_observed_r, Agency_self_r,Communion_observed_r,Communion_self_r,Resilience_observed_r,Resilience_self_r)

S1 <- data.frame(Means, Ps, Ds,Rs)
S1 <- S1 %>% mutate_if(is.numeric, round, digits=2)
S1

#Online Supplement S3 ####
#Correlations of main variables across constructs
S2<-corr.test(select(Data, Agency_observed,Communion_observed,Resilience_observed,
                     Agency_self,Communion_self,Resilience_self,
                     BFI_E,BFI_A,BFI_N,
                     BESSI_Agency,BESSI_Communion,BESSI_Resilience))
S2_correlation <- base::round(S2$r, digits = 2)
S2_p <- base::round(S2$p, digits = 3)

#Online Supplement S4####

#Create Parcels for Extraversion
BFI_E_latent <- '
Extraversion =~ BFI01 + BFI06 + BFI11 + BFI16 + BFI21 + BFI26'
Latent <- sem(BFI_E_latent,Data ,missing="fiml",std.lv=TRUE)
Order <- as.data.frame(inspect(Latent,"std"))
Order[order(-Order$Extraversion),]
Data$BFI_E_Parcel1 <- rowMeans(Data[,c("BFI16","BFI01")])
Data$BFI_E_Parcel2 <- rowMeans(Data[,c("BFI26","BFI21")])
Data$BFI_E_Parcel3 <- rowMeans(Data[,c("BFI11","BFI06")])

#Create Parcels for Agreeableness
BFI_A_latent <- '
Agree =~ BFI02 + BFI07 + BFI12 + BFI17 + BFI22 + BFI27'
Latent <- sem(BFI_A_latent,Data ,missing="fiml",std.lv=TRUE)
Order <- as.data.frame(inspect(Latent,"std"))
Order[order(-Order$Agree),]
Data$BFI_A_Parcel1 <- rowMeans(Data[,c("BFI07","BFI17")])
Data$BFI_A_Parcel2 <- rowMeans(Data[,c("BFI22","BFI12")])
Data$BFI_A_Parcel3 <- rowMeans(Data[,c("BFI27","BFI02")])

#Create Parcels for Negative emotionality
BFI_N_latent <- '
Negative =~ BFI04 + BFI09 + BFI14 + BFI19 + BFI24 + BFI29'
Latent <- sem(BFI_N_latent,Data ,missing="fiml",std.lv=TRUE)
Order <- as.data.frame(inspect(Latent,"std"))
Order[order(-Order$Negative),]
Data$BFI_N_Parcel1 <- rowMeans(Data[,c("BFI14","BFI09")])
Data$BFI_N_Parcel2 <- rowMeans(Data[,c("BFI19","BFI29")])
Data$BFI_N_Parcel3 <- rowMeans(Data[,c("BFI24","BFI04")])

#Create Parcels for Agency Skill
BESSI_A_latent <- '
Agency =~ Lead1 + Lead2 + Lead3 + Lead4 + Lead5 + Lead6 + Pers1 + Pers2 + Pers3 + Pers4 + Pers5 + Pers6a'
Latent <- sem(BESSI_A_latent,Data ,missing="fiml",std.lv=TRUE)
Order <- as.data.frame(inspect(Latent,"std"))
Order[order(-Order$Agency),]
Data$BESSI_Agency_Parcel1 <- rowMeans(Data[,c("Lead3","Pers3","Lead6","Lead4")])
Data$BESSI_Agency_Parcel2 <- rowMeans(Data[,c("Lead2","Pers4","Pers5","Pers2")])
Data$BESSI_Agency_Parcel3 <- rowMeans(Data[,c("Pers1","Lead1","Pers6a","Lead5")])

#Create Parcels for Communion Skill
BESSI_C_latent <- '
Communion =~ Empa1 + Empa2 + Empa3 + Empa4 + Empa5 + Empa6 + Soha1 + Soha2 + Soha3 + Soha4 + Soha5 + Soha6'
Latent <- sem(BESSI_C_latent,Data ,missing="fiml",std.lv=TRUE)
Order <- as.data.frame(inspect(Latent,"std"))
Order[order(-Order$Communion),]
Data$BESSI_Communion_Parcel1 <- rowMeans(Data[,c("Empa2","Empa4","Empa3","Soha4")])
Data$BESSI_Communion_Parcel2 <- rowMeans(Data[,c("Empa6","Soha2","Soha6","Soha5")])
Data$BESSI_Communion_Parcel3 <- rowMeans(Data[,c("Empa5","Empa1","Soha1","Soha3")])

#Create Parcels for Interpersonal Resilience Skill
BESSI_R_latent <- '
Resilience =~ Stres1 + Stres2 + Stres3 + Stres4 + Stres5 + Stres6 + Angm1 + Angm2 + Angm3 + Angm4 + Angm5 + Angm6'
Latent <- sem(BESSI_R_latent,Data ,missing="fiml",std.lv=TRUE)
Order <- as.data.frame(inspect(Latent,"std"))
Order[order(-Order$Resilience),]
Data$BESSI_Resilience_Parcel1 <- rowMeans(Data[,c("Angm5","Angm3","Stres4","Stres1")])
Data$BESSI_Resilience_Parcel2 <- rowMeans(Data[,c("Angm6","Stres2","Stres5","Stres3")])
Data$BESSI_Resilience_Parcel3 <- rowMeans(Data[,c("Angm4","Angm1","Stres6","Angm2")])

#extract reliability for one item measures
(1-0.85)*var(Data$Agency_observed, na.rm=TRUE) #0.3051863
(1-0.90)*var(Data$Communion_observed, na.rm=TRUE) #0.170372
(1-0.89)*var(Data$Resilience_observed, na.rm=TRUE) #0.1709428


#Latent Correlations Agency Domain
Agency <- '
observed_Agency =~ Agency_observed
Agency_observed~~0.3051863*Agency_observed
self_agency =~ Agency_self
Extraversion =~ BFI_E_Parcel1 + BFI_E_Parcel2 + BFI_E_Parcel3
Agency_Skill =~ BESSI_Agency_Parcel1 + BESSI_Agency_Parcel2 + BESSI_Agency_Parcel3 '
Agency <- sem(Agency,Data,missing="fiml",std.lv =TRUE)
summary(Agency, fit.measures= T, standard=T)

#Latent Correlations Communion Domain
Communion <- '
observed_Communion =~ Communion_observed
Communion_observed~~0.170372*Communion_observed
self_Communion =~ Communion_self
Agreeableness =~ BFI_A_Parcel1 + BFI_A_Parcel2 + BFI_A_Parcel3
Communion_Skill =~ BESSI_Communion_Parcel1 + BESSI_Communion_Parcel2 + BESSI_Communion_Parcel3 '
Communion <- sem(Communion,Data,missing="fiml",std.lv =TRUE)
summary(Communion, fit.measures= T, standard=T)

#Latent Correlations Resilience Domain
Resilience <- '
observed_Resilience =~ Resilience_observed
Resilience_observed~~0.1709428*Resilience_observed
self_Resilience =~ Resilience_self
NegativeEmotionality =~ BFI_N_Parcel1 + BFI_N_Parcel2 + BFI_N_Parcel3
Resilience_Skill =~ BESSI_Resilience_Parcel1 + BESSI_Resilience_Parcel2 + BESSI_Resilience_Parcel3'
Resilience <- sem(Resilience,Data,missing="fiml",std.lv =TRUE)
summary(Resilience, fit.measures= T, standard=T)

#Footnote 3: Differences between our sample and original samples####
t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat) 
}


# Extraversion
t.test2(3.13,3.34,0.65,0.59,1338,133)
# Sociability
t.test2(3.01,3.00,0.83,0.79,1338,133)
# Assertiveness
t.test2(3.07,3.35,0.90,0.74,1338,133)
# Energy level
t.test2(3.32,3.67,0.82,0.74,1338,133)
# Agreeableness
t.test2(3.70,3.97,0.57,0.54,1338,133)
# Compassion
t.test2(3.94,4.08,0.74,0.60,1338,133)
# Respectfulness
t.test2(3.96,4.27,0.73,0.65,1338,133)
# Trust
t.test2(3.20,3.54,0.74,0.76,1338,133)
# Negative Emotionality
t.test2(2.74,2.62,0.74,0.65,1338,133)
# Anxiety
t.test2(3.01,3.13,0.81,0.85,1338,133)
# Depression
t.test2(2.61,2.29,0.97,0.72,1338,133)
# Volatility
t.test2(2.60,2.45,0.89,0.80,1338,133)
        
# Leadership Skill
t.test2(3.07,3.19,0.91,0.62,786,133)
# Persuasive Skill
t.test2(3.15,3.15,0.71,0.66,786,133)
# Perspective-Taking Skill
t.test2(3.57,3.79,0.76,0.57,786,133)
# Capacity for Social Warmth
t.test2(3.57,3.57,0.71,0.57,786,133)
# Stress regulation
t.test2(3.15,2.93,0.89,0.66,785,133)
# Anger management
t.test2(3.11,3.32,0.87,0.64,685,133)

