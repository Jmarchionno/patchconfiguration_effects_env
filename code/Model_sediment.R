setwd("/Users/joe/Desktop/R_projects/CH3_Patchconfig/patchconfiguration_effects_env")



library(tidyverse)
library(ggplot2)
library(ggpubr)
library(vegan)
library(readxl)
library(dplyr)
library(psych)

###Analysis of variance
##Are there significant differences between treatment type and TC,TIC,TN

#TCdat<- read.csv('/Users/joe/Desktop/R_projects/CH3_Patchconfig/patchconfiguration_effects_env/data/Marchionno Final TC TIC results 6-18-25.csv')

##plot treatment vs variables and run ANOVA
##treatment x TOC

# ggplot(TCdat, aes(x=Treatment,y=wt..TOC.by.difference)) + 
#   geom_boxplot(outlier.shape = NA)+
#   geom_jitter()+
#   theme_minimal()+
#   stat_compare_means()
# 
# #ANOVA TOC
# 
# aov_TOC <- aov(TCdat$wt..TOC.by.difference ~ TCdat$Treatment)
# summary(aov_TOC)
# 
# ##treatment x TIC
# 
# ggplot(TCdat, aes(x=Treatment,y=X.TIC)) + 
#   geom_boxplot(outlier.shape = NA)+
#   geom_jitter()+
#   theme_minimal()+
#   stat_compare_means()
# 
# #ANOVA TIC
# 
# aov_TIC <- aov(TCdat$X.TIC ~ TCdat$Treatment)
# summary(aov_TIC)
# 
# ##treatment x CaCO3
# 
# ggplot(TCdat, aes(x=Treatment,y=X.CaCO3)) + 
#   geom_boxplot(outlier.shape = NA)+
#   geom_jitter()+
#   theme_minimal()+
#   stat_compare_means()
# 
# #ANOVA CaCO3
# 
# aov_CaCO3 <- aov(TCdat$X.CaCO3 ~ TCdat$Treatment)
# summary(aov_CaCO3)
# 
# ##treatment x TN
# 
# ggplot(TCdat, aes(x=Treatment,y=wt..N)) + 
#   geom_boxplot(outlier.shape = NA)+
#   geom_jitter()+
#   theme_minimal()+
#   stat_compare_means()
# 
# #ANOVA TN
# 
# aov_TN <- aov(TCdat$wt..N ~ TCdat$Treatment)
# summary(aov_TN)

######################################################
###Are there differences between treatment type and sediment grain size
######################################################
#tex_dat <- read.csv('/Users/joe/Desktop/R_projects/CH3_Patchconfig/patchconfiguration_effects_env/data/Marchionno_Texture.csv')

###################
## treatment vs >6mm
# ggplot(tex_dat, aes(x=Treatment,y=fraction...6mm)) + 
#   geom_boxplot(outlier.shape = NA)+
#   geom_jitter()+
#   theme_minimal()+
#   ylab("fraction >6mm")+
#   stat_compare_means()
# 
# #ANOVA treatment vs >6mm
# 
# aov_T_6mm <- aov(tex_dat$fraction...6mm ~ tex_dat$Treatment)
# summary(aov_T_6mm)
# 
# ####################
# ## treatment vs 2mm-6mm
# ggplot(tex_dat, aes(x=Treatment,y=fraction.2.6mm)) + 
#   geom_boxplot(outlier.shape = NA)+
#   geom_jitter()+
#   theme_minimal()+
#   ylab("fraction 2mm-6mm")+
#   stat_compare_means()
# 
# #ANOVA treatment vs 2mm-6mm
# 
# aov_T_2mm_6mm <- aov(tex_dat$fraction.2.6mm ~ tex_dat$Treatment)
# summary(aov_T_2mm_6mm)
# 
# ####################
# ## treatment vs 0.063mm-2mm
# 
# ggplot(tex_dat, aes(x=Treatment,y=fraction..063.2mm)) + 
#   geom_boxplot(outlier.shape = NA)+
#   geom_jitter()+
#   theme_minimal()+
#   ylab("fraction 0.063mm-2mm")+
#   stat_compare_means()
# 
# #ANOVA treatment vs 0.063mm-2mm
# 
# aov_T_.063mm_2mm <- aov(tex_dat$fraction..063.2mm ~ tex_dat$Treatment)
# summary(aov_T_.063mm_2mm)
# 
# ####################
# ## treatment vs <0.063mm
# 
# ggplot(tex_dat, aes(x=Treatment,y=fraction..0.063mm)) + 
#   geom_boxplot(outlier.shape = NA)+
#   geom_jitter()+
#   theme_minimal()+
#   ylab("fraction <0.063mm")+
#   stat_compare_means()
# 
# #ANOVA treatment vs <0.063mm
# 
# aov_T_.063mm <- aov(tex_dat$fraction..0.063mm ~ tex_dat$Treatment)
# summary(aov_T_.063mm)

#### read in oyster data ####

#oyster_dat <- read.csv('/Users/joe/Desktop/R_projects/CH3_Patchconfig/patchconfiguration_effects_env/data/oyster_latlong_elevation.csv')

## plot treatment vs % cover

#ggplot(oyster_dat, aes(x=treatment,y=percent.cover)) +
  #geom_boxplot(outlier.shape = NA)+
  #geom_jitter()+
  #theme_minimal()+
  #ylab("percent cover")+
  #stat_compare_means()

## ANOVA treatment vs elevation 

#treatment_elevationNA_dat<-read.csv('/Users/joe/Desktop/R_projects/CH3_Patchconfig/patchconfiguration_effects_env/data/oyster_latlong_elevation_tex_element_NAremoved.csv')

#ggplot(treatment_elevationNA_dat, aes(x=treatment,y=elevation..m.NAVD.)) +
  #geom_boxplot(outlier.shape = NA)+
  #geom_jitter()+
  #theme_minimal()+
  #ylab("elevation (m)")+
  #stat_compare_means()

#ov_treatment_elevation <- aov(treatment_elevationNA_dat$elevation..m.NAVD. ~ treatment_elevationNA_dat$treatment)
#summary(aov_treatment_elevation)

################################
################################
###PERMANOVA and NMDS for texture
#Create new dataframe

#perm_tex_dat<-tex_dat[, c(9,11,13,16)]

#Run NMDS Model for Visualizing the composition

#set.seed(123) #Pr 0.0637
#set.seed(234) #Pr 0.0597
#set.seed(345) #Pr 0.0607
#set.seed(456) #Pr 0.0603
#set.seed(567) #Pr 0.0626
#set.seed(768) #Pr 0.0592 <-this one
#set.seed(890) #Pr 0.0615
#set.seed(901) #Pr 0.0606
#set.seed(012) #Pr 0.0623
#set.seed(000) #Pr 0.0596

#nmds_tex_result<-metaMDS (perm_tex_dat, distance = "bray")

#Extract NMDS Scores 
#nmds_tex_scores <-as.data.frame(scores(nmds_tex_result)$sites)

#Find out the centroids

#group_tex_centroids <- data.frame(
  #Treatment = c("h", "l","c"),
  #Centroid_X = c(mean(nmds_tex_scores$NMDS1[tex_dat$Treatment == "h"]),
                #mean(nmds_tex_scores$NMDS1[tex_dat$Treatment == "l"]),
                 #mean(nmds_tex_scores$NMDS1[tex_dat$Treatment == "c"])),
  
  #Centroid_Y = c(mean(nmds_tex_scores$NMDS2[tex_dat$Treatment == "h"]),
                 #mean(nmds_tex_scores$NMDS2[tex_dat$Treatment == "l"]),
                 #mean(nmds_tex_scores$NMDS2[tex_dat$Treatment == "c"])))

###Create data frame for ggplot

#plot_NMDS_tex_data<-data.frame(Treatment = tex_dat$Treatment,
  #NMDS1=nmds_scores$NMDS1,
  #NMDS2=nmds_scores$NMDS2,
  #xend=c(rep(group_centroids[1,2],10),rep(group_centroids[2,2],10)),
  #yend=c(rep(group_centroids[1,3],10),rep( group_centroids[2,3],10)))

#lot_NMDS_tex_data <- data.frame(
  #Treatment = tex_dat$Treatment,
  #NMDS1 = nmds_tex_scores$NMDS1,
  #NMDS2 = nmds_tex_scores$NMDS2)

#plot_NMDS_tex_data <- merge(
  #plot_NMDS_tex_data,
  #group_tex_centroids,
  #by = "Treatment")

#names(plot_NMDS_tex_data)[4:5] <- c("xend", "yend")

#Plot the data

#ggplot(plot_NMDS_tex_data, aes(NMDS1,NMDS2)) + 
  #geom_point(aes(color = Treatment),size=2)+ 
  #stat_ellipse(geom = "polygon", alpha = 0.04, aes(group = Treatment), 
               #color = "black",fill="blue")+ 
  #geom_point(data = group_tex_centroids, aes(x = Centroid_X, y = Centroid_Y), 
             #color = "black", size = 2, shape = 7)+
  #geom_segment(data = plot_NMDS_tex_data, aes(x =NMDS1, y = NMDS2, 
                                     #xend = xend, yend = yend, color = Treatment), alpha = 0.5)+
  #scale_color_manual(name= "Treatment",labels= unique(plot_NMDS_tex_data$Treatment),
                     #values= c("darkolivegreen", "darkviolet","darkorange1"))+ theme_bw()

##############PERMANOVA##############################


#Distance Matrix

#perm_tex_dist<-vegdist(perm_tex_dat, method='bray')

#Assumptions

#dispersion<-betadisper(perm_tex_dist, group=tex_dat$Treatment,type = "centroid")

#plot(dispersion)

#anova(dispersion)

#Test

#perma_tex_result<-adonis2( perm_tex_dist~as.factor(plot_NMDS_tex_data$Treatment), data=perm_tex_dist,
                       #permutations=9999)

#perma_tex_result

######################################################
######################################################
### Repeat but examine differences between treatment type and element analysis
######################################################
######################################################
######################################################

#tex_el_dat<-read.csv('/Users/joe/Desktop/R_projects/CH3_Patchconfig/patchconfiguration_effects_env/data/Marchionno_TCTN_Texture.csv')

#perm_tex_el_dat<-tex_el_dat[, c(6,9,10,11,12)]

#Run NMDS Model for Visualizing the composition

#set.seed(999) #Pr 0.2747
#set.seed(888) #Pr 
#set.seed(777) #Pr 
#set.seed(666) #Pr 
#set.seed(555) #Pr 
#set.seed(444) #Pr 
#set.seed(333) #Pr 
#set.seed(222) #Pr 
#set.seed(111) #Pr 
#set.seed(001) #Pr 

#Transform

#perm_tex_el_dat_trans<-sqrt(perm_tex_el_dat)

#nmds_tex_el_result<-metaMDS (perm_tex_el_dat_trans, distance = "bray")

#Extract NMDS Scores 
#nmds_tex_el_scores <-as.data.frame(scores(nmds_tex_el_result)$sites)

#Find out the centroids

#group_tex_el_centroids <- data.frame(
  #Treatment = c("h", "l","c"),
  #Centroid_X = c(mean(nmds_tex_el_scores$NMDS1[tex_el_dat$Treatment == "h"]),
                 #mean(nmds_tex_el_scores$NMDS1[tex_el_dat$Treatment == "l"]),
                 #mean(nmds_tex_el_scores$NMDS1[tex_el_dat$Treatment == "c"])),
  
  #Centroid_Y = c(mean(nmds_tex_el_scores$NMDS2[tex_el_dat$Treatment == "h"]),
                 #mean(nmds_tex_el_scores$NMDS2[tex_el_dat$Treatment == "l"]),
                 #mean(nmds_tex_el_scores$NMDS2[tex_el_dat$Treatment == "c"])))

###Create data frame for ggplot

#plot_NMDS_tex_el_data <- data.frame(
  #Treatment = tex_el_dat$Treatment,
  #NMDS1 = nmds_tex_el_scores$NMDS1,
  #NMDS2 = nmds_tex_el_scores$NMDS2)

#plot_NMDS_tex_el_data <- merge(
  #plot_NMDS_tex_el_data,
  #group_tex_el_centroids,
  #by = "Treatment")

#names(plot_NMDS_tex_el_data)[4:5] <- c("xend", "yend")

#Plot the data

#ggplot(plot_NMDS_tex_el_data, aes(NMDS1,NMDS2)) + 
  #geom_point(aes(color = Treatment),size=2)+ 
  #stat_ellipse(geom = "polygon", alpha = 0.04, aes(group = Treatment), 
             #  color = "black",fill="blue")+ 
  #geom_point(data = group_tex_el_centroids, aes(x = Centroid_X, y = Centroid_Y), 
      #       color = "black", size = 2, shape = 7)+
  #geom_segment(data = plot_NMDS_tex_el_data, aes(x =NMDS1, y = NMDS2, 
            #                                  xend = xend, yend = yend, color = Treatment), alpha = 0.5)+
 # scale_color_manual(name= "Treatment",labels= unique(plot_NMDS_tex_el_data$Treatment),
                 #    values= c("darkgoldenrod", "darkblue","darkseagreen3"))+ theme_bw()

##############PERMANOVA##############################


#Distance Matrix

#perm_tex_el_dist<-vegdist(perm_tex_el_dat, method='bray')

#Assumptions

#dispersion_el<-betadisper(perm_tex_el_dist, group=tex_el_dat$Treatment,type = "centroid")

#plot(dispersion_el)

#anova(dispersion_el)

#Test

#perma_tex_el_result<-adonis2( perm_tex_el_dist~as.factor(plot_NMDS_tex_el_data$Treatment), data=perm_tex_el_dist,
                        #   permutations=9999)

#perma_tex_el_result

######################################################
######################################################
### Oyster Density summary stats tables and box plots
######################################################
######################################################
######################################################

#####Oyster Density control vs treatment ######


# ============================================================
# Oyster Reef Restoration — Boxplots
# Density, elevation, C:N ratio, and sediment grain-size fractions
# by treatment (h = high, c = control, l = low)
# ============================================================


# ---- 1. Load data -------------------------------------------------
oyster_alldat<-read.csv("/Users/joe/Desktop/R_projects/CH3_Patchconfig/patchconfiguration_effects_env/data/oyster_latlong_elevation_tex_element.csv")


# Rename columns by POSITION (not by their original text) so the script
# doesn't break if punctuation/spacing in the header gets altered by
# whatever reads the file (e.g. base read.csv() converts "C:N ratio" to
# "C.N.ratio"). This assumes the columns are still in their original
# order below — run `names(oyster_alldat)` first if you're not sure.
colnames(oyster_alldat) <- c(
  "sample_id", "treatment", "treatment_substrate", "substrate_sampled",
  "density", "percent_cover", "easting", "northing", "elevation",
  "longitude", "latitude", "mean_oyster_density",
  "wt_pct_n", "wt_pct_total_c", "pct_caco3", "pct_tic", "wt_pct_toc",
  "cn_ratio", "frac_gt_6mm", "frac_2_6mm", "frac_063_2mm", "frac_lt_063mm"
)

# Order treatment levels for consistent plotting (control, low, high)
oyster_alldat <- oyster_alldat %>%
  mutate(treatment = factor(treatment, levels = c("c", "l", "h"),
                            labels = c("Control", "Low", "High")))

# ---- 2. Oyster density by treatment --------------------------------
p_density <- ggplot(oyster_alldat, aes(x = treatment, y = density, fill = treatment)) +
  geom_boxplot(outlier.shape = 21, alpha = 0.8) +
  labs(title = "Oyster Density by Treatment",
       x = "Treatment", y = expression("Density (#/m"^2*")")) +
  theme_minimal() +
  theme(legend.position = "none")

# ---- 3. Elevation by treatment --------------------------------------
p_elevation <- ggplot(oyster_alldat, aes(x = treatment, y = elevation, fill = treatment)) +
  geom_boxplot(outlier.shape = 21, alpha = 0.8) +
  labs(title = "Elevation by Treatment",
       x = "Treatment", y = "Elevation (m, NAVD88)") +
  theme_minimal() +
  theme(legend.position = "none")

# ---- 4. C:N ratio by treatment (sediment/shell samples only) --------
p_cn <- oyster_alldat %>%
  filter(!is.na(cn_ratio)) %>%
  ggplot(aes(x = treatment, y = cn_ratio, fill = treatment)) +
  geom_boxplot(outlier.shape = 21, alpha = 0.8) +
  labs(title = "Sediment C:N Ratio by Treatment",
       x = "Treatment", y = "C:N Ratio") +
  theme_minimal() +
  theme(legend.position = "none")

# ---- 5. Sediment grain-size fractions (faceted) ----------------------
# Reshape the four fraction columns to long format for faceting
fraction_long <- oyster_alldat %>%
  filter(!is.na(frac_gt_6mm)) %>%
  select(treatment, frac_gt_6mm, frac_2_6mm, frac_063_2mm, frac_lt_063mm) %>%
  pivot_longer(cols = starts_with("frac_"),
               names_to = "grain_size", values_to = "fraction") %>%
  mutate(grain_size = factor(grain_size,
                             levels = c("frac_gt_6mm", "frac_2_6mm", "frac_063_2mm", "frac_lt_063mm"),
                             labels = c("> 6 mm", "2-6 mm", "0.063-2 mm", "< 0.063 mm")))

p_fractions <- ggplot(fraction_long, aes(x = treatment, y = fraction, fill = treatment)) +
  geom_boxplot(outlier.shape = 21, alpha = 0.8) +
  facet_wrap(~ grain_size, nrow = 1) +
  labs(title = "Sediment Grain-Size Fractions by Treatment",
       x = "Treatment", y = "Fraction of sample") +
  theme_minimal() +
  theme(legend.position = "none")

# ---- 6. Display all plots --------------------------------------------
print(p_density)
print(p_elevation)
print(p_cn)
print(p_fractions)

# ---- 7. Optional: save plots to file ----------------------------------
# ggsave("density_boxplot.png", p_density, width = 6, height = 4, dpi = 300)
# ggsave("elevation_boxplot.png", p_elevation, width = 6, height = 4, dpi = 300)
# ggsave("cn_ratio_boxplot.png", p_cn, width = 6, height = 4, dpi = 300)
# ggsave("sediment_fractions_boxplot.png", p_fractions, width = 10, height = 4, dpi = 300)
