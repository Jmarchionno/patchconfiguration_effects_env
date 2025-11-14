setwd("/Users/joe/Desktop/R_projects/CH3_Patchconfig/patchconfiguration_effects_env")

###Analysis of variance
##is there a correlation between treatment type and TC,TIC,TN

library(tidyverse)
library(ggplot2)

TCdat<- read.csv('/Users/joe/Desktop/R_projects/CH3_Patchconfig/
                 patchconfiguration_effects_env/data/
                 Marchionno Final TC TIC results 6-18-25.csv')

##plot treatment vs variables and run ANOVA
##treatment x TOC

ggplot(TCdat, aes(x=Treatment,y=wt..TOC.by.difference)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter()+
  theme_minimal()

#ANOVA TOC

aov_TOC <- aov(TCdat$wt..TOC.by.difference ~ TCdat$Treatment)
summary(aov_TOC)

##treatment x TIC

ggplot(TCdat, aes(x=Treatment,y=X.TIC)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter()+
  theme_minimal()

#ANOVA TIC

aov_TIC <- aov(TCdat$X.TIC ~ TCdat$Treatment)
summary(aov_TIC)

##treatment x CaCO3

ggplot(TCdat, aes(x=Treatment,y=X.CaCO3)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter()+
  theme_minimal()

#ANOVA CaCO3

aov_CaCO3 <- aov(TCdat$X.CaCO3 ~ TCdat$Treatment)
summary(aov_CaCO3)

##treatment x TN

ggplot(TCdat, aes(x=Treatment,y=wt..N)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter()+
  theme_minimal()

#ANOVA TN

aov_TN <- aov(TCdat$wt..N ~ TCdat$Treatment)
summary(aov_TN)
