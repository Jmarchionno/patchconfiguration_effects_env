setwd("/Users/joe/Desktop/R_projects/CH3_Patchconfig/patchconfiguration_effects_env")

###Analysis of variance
##Are there significant differences between treatment type and TC,TIC,TN

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(vegan)
library(readxl)

TCdat<- read.csv('/Users/joe/Desktop/R_projects/CH3_Patchconfig/patchconfiguration_effects_env/data/Marchionno Final TC TIC results 6-18-25.csv')

##plot treatment vs variables and run ANOVA
##treatment x TOC

ggplot(TCdat, aes(x=Treatment,y=wt..TOC.by.difference)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter()+
  theme_minimal()+
  stat_compare_means()

#ANOVA TOC

aov_TOC <- aov(TCdat$wt..TOC.by.difference ~ TCdat$Treatment)
summary(aov_TOC)

##treatment x TIC

ggplot(TCdat, aes(x=Treatment,y=X.TIC)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter()+
  theme_minimal()+
  stat_compare_means()

#ANOVA TIC

aov_TIC <- aov(TCdat$X.TIC ~ TCdat$Treatment)
summary(aov_TIC)

##treatment x CaCO3

ggplot(TCdat, aes(x=Treatment,y=X.CaCO3)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter()+
  theme_minimal()+
  stat_compare_means()

#ANOVA CaCO3

aov_CaCO3 <- aov(TCdat$X.CaCO3 ~ TCdat$Treatment)
summary(aov_CaCO3)

##treatment x TN

ggplot(TCdat, aes(x=Treatment,y=wt..N)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter()+
  theme_minimal()+
  stat_compare_means()

#ANOVA TN

aov_TN <- aov(TCdat$wt..N ~ TCdat$Treatment)
summary(aov_TN)

######################################################
###Are there differences between treatment type and grain size
######################################################
tex_dat <- read.csv('/Users/joe/Desktop/R_projects/CH3_Patchconfig/patchconfiguration_effects_env/data/Marchionno_Texture.csv')

###################
## treatment vs >6mm
ggplot(tex_dat, aes(x=Treatment,y=fraction...6mm)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter()+
  theme_minimal()+
  ylab("fraction >6mm")+
  stat_compare_means()

#ANOVA treatment vs >6mm

aov_T_6mm <- aov(tex_dat$fraction...6mm ~ tex_dat$Treatment)
summary(aov_T_6mm)

####################
## treatment vs 2mm-6mm
ggplot(tex_dat, aes(x=Treatment,y=fraction.2.6mm)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter()+
  theme_minimal()+
  ylab("fraction 2mm-6mm")+
  stat_compare_means()

#ANOVA treatment vs 2mm-6mm

aov_T_2mm_6mm <- aov(tex_dat$fraction.2.6mm ~ tex_dat$Treatment)
summary(aov_T_2mm_6mm)

####################
## treatment vs 0.063mm-2mm

ggplot(tex_dat, aes(x=Treatment,y=fraction..063.2mm)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter()+
  theme_minimal()+
  ylab("fraction 0.063mm-2mm")+
  stat_compare_means()

#ANOVA treatment vs 0.063mm-2mm

aov_T_.063mm_2mm <- aov(tex_dat$fraction..063.2mm ~ tex_dat$Treatment)
summary(aov_T_.063mm_2mm)

####################
## treatment vs <0.063mm

ggplot(tex_dat, aes(x=Treatment,y=fraction..0.063mm)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter()+
  theme_minimal()+
  ylab("fraction <0.063mm")+
  stat_compare_means()

#ANOVA treatment vs <0.063mm

aov_T_.063mm <- aov(tex_dat$fraction..0.063mm ~ tex_dat$Treatment)
summary(aov_T_.063mm)

#### read in oyster data ####

oyster_dat <- read.csv('/Users/joe/Desktop/R_projects/CH3_Patchconfig/patchconfiguration_effects_env/data/oyster_data.csv')

## plot treatment vs % cover

ggplot(oyster_dat, aes(x=treatment,y=percent.cover)) +
  geom_boxplot(outlier.shape = NA)+
  geom_jitter()+
  theme_minimal()+
  ylab("percent cover")+
  stat_compare_means()

################################
################################
###PERMANOVA and NMDS
#Create new dataframe

perm_tex_dat<-tex_dat[, c(9,11,13,16)]

#Run NMDS Model for Visualizing the composition

nmds_result<-metaMDS (perm_tex_dat, distance = "bray")

#Extract NMDS Scores 
nmds_scores <-as.data.frame(scores(nmds_result)$sites)
