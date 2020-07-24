################################################################################
# Code to perform meta-analyses on paper "Enhancing risk prediction of 
# progression to severe disease during the febrile phase of dengue: 
# A systematic review and meta-analysis"
#
# Cite as: 
# Sangkaew et. al., 2020, Enhancing risk prediction of prgression to severe
# disease during the febrile phase of dengue: A systmeatic review 
# and meta-analysis, The Lancet Infectious Diseases (2020), doi:****
#
# Description: 
# See README
################################################################################

# load libraries----------------------------------------------------------------
library(xlsx)
library(dplyr) 
library(meta)
library(netmeta)
library(metafor)
library(tidyr)
library(ggplot2)
library(forestplot)

# load utils file --------------------------------------------------------------
source("functions.R")

# Included studies' characteristics and risk of bias assessment-----------------
df_all <- read.csv("all_included.csv")
str(df_all)
summary(df_all)

rob_plot <- rob.summary(df_all[ which(df_all$Meta.analysis == "Yes"), 7:12])

# Import and create datasets for each factor to perform meta-analysis-----------

# meta-analysis of age and weight factors 
df_macon <- read.csv("df_macon.csv") 

# meta-analysis of WBC, HCT, PLT, AST, ALT, and ALB factors 
df_magen_co <- read.csv("df_magen_co.csv") 

# meta-analysis of sex, comorbidity, apparence of signs and symptoms, and immune 
# status factors 
df_magen_ca <- read.csv("df_magen_ca.csv") 

# meta-analysis of nutritional status
df_nutri <- read.csv("df_nutri.csv")

# meta analysis of serotype
df_serotype <- read.csv("df_serotype.csv")

# Age factor -------------------------------------------------------------------

# Performing meta-analysis for age variable reported with a continuous formats
MA_age <- filter(df_macon, vari == "age")

str(MA_age)

# Main analysis
Age_mod <-metacont(Ne, 
                   Me, 
                   Se,
                   Nc, 
                   Mc, 
                   Sc, 
                   data = MA_age,
                   studlab = Id,
                   comb.fixed = F,
                   comb.random = T,
                   prediction = T,
                   sm = "SMD" )

# Sub-analysis by age criteria
# Children
MA_age_ch <- df_macon %>%
  filter(vari == "age" & Pop == "children")

Age_mod_ch <-metacont(Ne, 
                      Me, 
                      Se,
                      Nc, 
                      Mc, 
                      Sc, 
                      data = MA_age_ch,
                      studlab = Id,
                      comb.fixed = F,
                      comb.random = T,
                      prediction = T,
                      sm = "SMD" )

# Adults
MA_age_ad <- df_macon %>%
  filter(vari == "age" & Pop == "adults")

Age_mod_ad <-metacont(Ne, 
                      Me, 
                      Se,
                      Nc, 
                      Mc, 
                      Sc, 
                      data = MA_age_ad,
                      studlab = Id,
                      comb.fixed = F,
                      comb.random = T,
                      prediction = T,
                      sm = "SMD" )

# Sub-analysis by the WHO classifications

MA_age_cl <- filter(MA_age, MA_age$Cl == "1997 WHO Classification" | 
                      MA_age$Cl == "2009 WHO Classification")

Age_mod_cl <-metacont(Ne, 
                      Me, 
                      Se,
                      Nc, 
                      Mc, 
                      Sc, 
                      data = MA_age_cl,
                      studlab = Id,
                      comb.fixed = F,
                      comb.random = T,
                      prediction = T,
                      sm = "SMD" )

Age_mod_cl <-update.meta(Age_mod_cl, 
                         byvar = MA_age_cl$Cl,
                         comb.random = T,
                         comb.fixed = F)

# Detecting outliers and most influential studies
spot.outliers.random(Age_mod)
influence.analysis(Age_mod, method.tau = "DL", hakn = F)

spot.outliers.random(Age_mod_ch)
influence.analysis(Age_mod_ch, method.tau = "DL", hakn = F)

spot.outliers.random(Age_mod_ad)
influence.analysis(Age_mod_ad, method.tau = "DL", hakn = F)

spot.outliers.random(Age_mod_cl)
influence.analysis(Age_mod_cl, method.tau = "DL", hakn = F)
influence.analysis(Age_mod_cl, method.tau = "DL", hakn = F)

# Small-study effect bias
# Ploting a funel plot
funnel(Age_mod, xlab = "Mean difference")

# Egger's test
metabias(Age_mod, method.bias = "linreg", k.min = 4)
metabias(Age_mod_ch, method.bias = "linreg", k.min = 4)
metabias(Age_mod_ad, method.bias = "linreg", k.min = 4)

# Weight------------------------------------------------------------------------

MA_weight <- filter(df_macon, vari == "weight")
MA_weight_ch <- filter(df_macon, vari == "weight" & Pop == "children")

Weight_mod <- metacont(Ne, 
                       Me, 
                       Se,
                       Nc, 
                       Mc, 
                       Sc, 
                       data = MA_weight,
                       studlab = Id ,
                       comb.fixed = F,
                       comb.random = T,
                       prediction = T,
                       sm = "SMD" )

Weight_mod_ch <- metacont(Ne, 
                          Me, 
                          Se,
                          Nc, 
                          Mc, 
                          Sc, 
                          data = MA_weight_ch,
                          studlab = Id ,
                          comb.fixed = F,
                          comb.random = T,
                          prediction = T,
                          sm = "SMD" )

### Detecting outliers and influential studies
spot.outliers.random(Weight_mod)
influence.analysis(Weight_mod, method.tau = "DL", hakn = F)

spot.outliers.random(Weight_mod_ch)
influence.analysis(Weight_mod_ch, method.tau = "DL", hakn = F)

### Small-study effect bias

# Ploting a funel plot
funnel(Weight_mod, xlab = "Mean difference")
funnel(Weight_mod_ch, xlab = "Mean difference")

# Egger's test
metabias(Weight_mod, method.bias = "linreg", k.min = 4)
metabias(Weight_mod_ch, method.bias = "linreg", k.min = 4)

# Platelet----------------------------------------------------------------------
MA_PLT <- filter(df_magen_co, vari == "plt")
MA_PLT_ch <- filter(df_magen_co, vari == "plt" & Pop == "Children")
MA_PLT_ad <- filter(df_magen_co, vari == "plt" & Pop == "Adults")

str(MA_PLT)

PLT_mod <-metagen(TE,
                  seTE,
                  studlab = Id,
                  MA_PLT,
                  comb.fixed = F,
                  comb.random = T,
                  prediction = F,
                  sm= "SMD")

# Perform subgroup analysis by the WHO classifications
PLT_mod_cl <- update.meta(PLT_mod, 
                          byvar =  MA_PLT$Cl,
                          comb.fixed = F,
                          comb.random = T)

# Perform subgroup analysis by the age criteria
PLT_mod_ch <-metagen(TE,
                     seTE,
                     studlab = Id,
                     MA_PLT_ch,
                     comb.fixed = F,
                     comb.random = T,
                     prediction = F,
                     sm= "SMD")

PLT_mod_ad <-metagen(TE,
                     seTE,
                     studlab = Id,
                     MA_PLT_ad,
                     comb.fixed = F,
                     comb.random = T,
                     prediction = F,
                     sm= "SMD")

### Detecting outliers and influential studies
spot.outliers.random(PLT_mod)
influence.analysis(PLT_mod, method.tau = "DL", hakn = F)

spot.outliers.random(PLT_mod_ch)
influence.analysis(PLT_mod_ch, method.tau = "DL", hakn = F)

spot.outliers.random(PLT_mod_ad)
influence.analysis(PLT_mod_ad, method.tau = "DL", hakn = F)

# Small-study effect bias

# Ploting a funel plot
funnel(PLT_mod, xlab = "SMD")
funnel(PLT_mod_ch, xlab = "SMD")
funnel(PLT_mod_ad, xlab = "SMD")

# Egger's test
metabias(PLT_mod, method.bias = "linreg")
metabias(PLT_mod_ch, method.bias = "linreg", k.min = 4)
metabias(PLT_mod_ad, method.bias = "linreg", k.min = 4)

# WBC --------------------------------------------------------------------------

MA_WBC <- filter(df_magen_co, vari == "wbc")
str(MA_WBC)

WBC_mod <- metagen(TE,
                   seTE,
                   studlab = Id,
                   MA_WBC,
                   comb.fixed = F,
                   comb.random = T,
                   prediction = F,
                   sm= "SMD")

### Detecting outliers and influential studies
spot.outliers.random(WBC_mod)
influence.analysis(WBC_mod, method.tau = "DL", hakn = F)

# Small-study effect bias

# Ploting a funel plot
funnel(WBC_mod, xlab = "SMD")

# Egger's test
metabias(WBC_mod, method.bias = "linreg")

# HCT --------------------------------------------------------------------------
MA_HCT <- filter(df_magen_co, vari == "hct")
MA_HCT_ch <- filter(df_magen_co, vari == "hct" & Pop == "Children")
MA_HCT_ad <- filter(df_magen_co, vari == "hct" & Pop == "Adults")
str(MA_HCT)

HCT_mod <- metagen(TE,
                   seTE,
                   studlab = Id,
                   MA_HCT,
                   comb.fixed = F,
                   comb.random = T,
                   prediction = F,
                   sm= "SMD")

HCT_mod_ch <-metagen(TE,
                     seTE,
                     studlab = paste(Au, paste0("(",Yr,")")),
                     MA_HCT_ch,
                     comb.fixed = F,
                     comb.random = T,
                     prediction = F,
                     sm= "SMD")

HCT_mod_ad <-metagen(TE,
                     seTE,
                     studlab = Id,
                     MA_HCT_ad,
                     comb.fixed = F,
                     comb.random = T,
                     prediction = F,
                     sm= "SMD")


### Detecting outliers and influential studies
spot.outliers.random(HCT_mod)
influence.analysis(HCT_mod, method.tau = "DL", hakn = F)

spot.outliers.random(HCT_mod_ch)
influence.analysis(HCT_mod, method.tau = "DL", hakn = F)

spot.outliers.random(HCT_mod_ch)
influence.analysis(HCT_mod, method.tau = "DL", hakn = F)

# Small-study effect bias

# Ploting a funel plot
funnel(HCT_mod, xlab = "SMD")
funnel(HCT_mod_ch, xlab = "SMD")
funnel(HCT_mod_ad, xlab = "SMD")

# Egger's test
metabias(HCT_mod, method.bias = "linreg", k.min = 4)
metabias(HCT_mod_ch, method.bias = "linreg", k.min =  4)
metabias(HCT_mod_ad, method.bias = "linreg", k.min = 4)

# AST --------------------------------------------------------------------------
MA_AST <- filter(df_magen_co, vari == "ast")
MA_AST_ch <- filter(df_magen_co, vari == "ast" & Pop == "Children")

AST_mod <-metagen(TE,
                  seTE,
                  studlab = Id,
                  MA_AST,
                  comb.fixed = F,
                  comb.random = T,
                  prediction = F,
                  sm= "SMD")

AST_mod_ch <-metagen(TE,
                     seTE,
                     studlab = Id,
                     MA_AST_ch,
                     comb.fixed = F,
                     comb.random = T,
                     prediction = F,
                     sm= "SMD")


### Detecting outliers and influential studies
spot.outliers.random(AST_mod)
influence.analysis(AST_mod, method.tau = "DL", hakn = F)

spot.outliers.random(AST_mod_ch)
influence.analysis(AST_mod_ch, method.tau = "DL", hakn = F)

# Small-study effect bias
# Ploting a funel plot
funnel(AST_mod, xlab = "SMD")
funnel(AST_mod_ch, xlab = "SMD")

# Egger's test
metabias(AST_mod, method.bias = "linreg", k.min = 4)
metabias(AST_mod_ch, method.bias = "linreg", k.min = 4)

### trim and fill procedure
TnF_AST_mod <- trimfill(AST_mod)
funnel(TnF_AST_mod, xlab = "SMD")

# ALT --------------------------------------------------------------------------
MA_ALT <- filter(df_magen_co, vari == "alt")
MA_ALT_ch <- filter(df_magen_co, vari == "alt" & Pop == "Children")

ALT_mod <-metagen(TE,
                  seTE,
                  studlab = Id,
                  MA_ALT,
                  comb.fixed = F,
                  comb.random = T,
                  prediction = F,
                  sm= "SMD")

ALT_mod_ch <-metagen(TE,
                     seTE,
                     studlab = Id,
                     MA_ALT_ch,
                     comb.fixed = F,
                     comb.random = T,
                     prediction = F,
                     sm= "SMD")

### Detecting outliers and influential studies
spot.outliers.random(ALT_mod)
influence.analysis(ALT_mod, method.tau = "DL", hakn = F)

spot.outliers.random(ALT_mod_ch)
influence.analysis(ALT_mod_ch, method.tau = "DL", hakn = F)

# Small-study effect bias

# Ploting a funel plot
funnel(ALT_mod, xlab = "SMD")
funnel(ALT_mod_ch, xlab = "SMD")


# Egger's test
metabias(ALT_mod, method.bias = "linreg")
metabias(ALT_mod_ch, method.bias = "linreg")

### trim and fill procedure
TnF_ALT_mod <- trimfill(ALT_mod)
funnel(TnF_ALT_mod, xlab = "SMD")

# ALB -------------------------------------------------------------------------- 
MA_ALB <- filter(df_magen_co, vari == "alb")
str(MA_ALB)

ALB_mod <-metagen(TE,
                  seTE,
                  studlab = Id,
                  MA_ALB,
                  comb.fixed = F,
                  comb.random = T,
                  prediction = F,
                  sm= "SMD")

### Detecting outliers and influential studies
spot.outliers.random(ALB_mod)
influence.analysis(ALB_mod, method.tau = "DL", hakn = F)

### Small-study effect bias
funnel(ALB_mod, xlab = "SMD")

# Egger's test
metabias(ALB_mod, method.bias = "linreg", k.min =4)

# Sex --------------------------------------------------------------------------
MA_gender <- filter(df_magen_ca, vari == "sex")
MA_gender_ch <- filter(df_magen_ca, vari == "sex" & Pop == "Children")
MA_gender_ad <- filter(df_magen_ca, vari == "sex" & Pop == "Adults")

Gender_mod <- metagen( TE = log(TE), 
                       seTE = seTE, 
                       data = MA_gender,
                       studlab = paste(Au , paste0("(",Yr,")")),
                       comb.fixed = F,
                       comb.random = T,
                       prediction = F,
                       sm="OR")

Gender_mod_ch <- metagen( TE = log(TE), 
                          seTE = seTE, 
                          data = MA_gender_ch,
                          studlab = paste(Au , paste0("(",Yr,")")),
                          comb.fixed = F,
                          comb.random = T,
                          prediction = F,
                          sm="OR")

Gender_mod_ad <- metagen( TE = log(TE), 
                          seTE = seTE, 
                          data = MA_gender_ad,
                          studlab = paste(Au , paste0("(",Yr,")")),
                          comb.fixed = F,
                          comb.random = T,
                          prediction = F,
                          sm="OR")

Gendre_mod_cl <-update.meta(Gender_mod, 
                            byvar = MA_gender$Cl,
                            comb.random = T,
                            comb.fixed = F)

### Detecting outliers and influential studies
spot.outliers.random(Gender_mod)
influence.analysis(Gender_mod, method.tau = "DL", hakn = F)

spot.outliers.random(Gender_mod_ch)
influence.analysis(Gender_mod_ch, method.tau = "DL", hakn = F)

spot.outliers.random(Gender_mod_ad)
influence.analysis(Gender_mod_ad, method.tau = "DL", hakn = F)

### Small-study effect bias
funnel(Gender_mod, xlab = "OR")
funnel(Gender_mod_ch, xlab = "OR")
funnel(Gender_mod_ad, xlab = "OR")

# Egger's test
metabias(Gender_mod, method.bias = "linreg", k.min =4)
metabias(Gender_mod_ch, method.bias = "linreg", k.min =4)
metabias(Gender_mod_ad, method.bias = "linreg", k.min =4)

### trim and fill procedure
TnF_Gender_mod <- trimfill(Gender_mod)
funnel(TnF_Gender_mod, xlab = "OR")

# Mixed comorbidities-----------------------------------------------------------
MA_mixcomor <- filter(df_magen_ca, vari == "mixcomor")
MA_mixcomor_ad <- filter(df_magen_ca, vari == "mixcomor" & Pop == "Adults")

Mixcomor_mod<- metagen( TE = log(TE), 
                        seTE = seTE, 
                        data = MA_mixcomor,
                        studlab = Id,
                        comb.fixed = F,
                        comb.random = T,
                        prediction = F,
                        sm="OR")

Mixcomor_mod_ad<- metagen( TE = log(TE), 
                           seTE = seTE, 
                           data = MA_mixcomor_ad,
                           studlab = Id,
                           comb.fixed = F,
                           comb.random = T,
                           prediction = F,
                           sm="OR")

### Detecting outliers and influential studies
spot.outliers.random(Mixcomor_mod)
influence.analysis(Mixcomor_mod, method.tau = "DL", hakn = F)

spot.outliers.random(Mixcomor_mod_ad)
influence.analysis(Mixcomor_mod_ad, method.tau = "DL", hakn = F)

### Small-study effect bias
funnel(Mixcomor_mod, xlab = "OR")
funnel(Mixcomor_mod_ad, xlab = "OR")

# Egger's test
metabias(Mixcomor_mod, method.bias = "linreg", k.min =4)
metabias(Mixcomor_mod_ad, method.bias = "linreg", k.min =4)

# HTN --------------------------------------------------------------------------
MA_HTN <- filter(df_magen_ca, vari == "htn")

HTN_mod <- metagen( TE = log(TE), 
                    seTE = seTE, 
                    data = MA_HTN,
                    studlab = Id,
                    comb.fixed = F,
                    comb.random = T,
                    prediction = F,
                    sm="OR")

### Detecting outliers and influential studies
spot.outliers.random(HTN_mod)
influence.analysis(HTN_mod, method.tau = "DL", hakn = F)

### Small-study effect bias
##Assumption: effect sizes drives publication bias (small eff. sizes are missing)
funnel(HTN_mod, xlab = "Odds ratio")
metabias(HTN_mod, method.bias = "linreg", k.min = 4)

### trim and fill procedure
TnF_HTN_mod <- trimfill(HTN_mod)
funnel(TnF_HTN_mod, xlab = "Odds ratio")

# DM----------------------------------------------------------------------------
MA_DM <- filter(df_magen_ca, vari == "dm")
MA_DM_ad <- filter(df_magen_ca, vari == "dm" & Pop == "Adults")

DM_mod<- metagen( TE = log(TE), 
                  seTE = seTE, 
                  data = MA_DM,
                  studlab = Id,
                  comb.fixed = F,
                  comb.random = T,
                  prediction = F,
                  sm="OR")

DM_mod_ad <- metagen( TE = log(TE), 
                      seTE = seTE, 
                      data = MA_DM_ad,
                      studlab = Id,
                      comb.fixed = F,
                      comb.random = T,
                      prediction = F,
                      sm="OR")

### Detecting outliers and influential studies
spot.outliers.random(DM_mod)
influence.analysis(DM_mod, method.tau = "DL", hakn = F)

# Small-study effect bias
# Assumption: effect sizes drive publication bias (small eff. sizes are missing)

spot.outliers.random(DM_mod_ad)
influence.analysis(DM_mod_ad, method.tau = "DL", hakn = F)

metabias(DM_mod, method.bias = "linreg", k.min = 4)
metabias(DM_mod_ad, method.bias = "linreg", k.min = 4)

# trim and fill procedure
TnF_DM_mod <- trimfill(DM_mod)
funnel(TnF_DM_mod, xlab = "Odds ratio")

# Renal disease ----------------------------------------------------------------
MA_renal <- filter(df_magen_ca, vari == "rd")
MA_renal_ad <- filter(df_magen_ca, vari == "rd" & Pop == "Adults")

renal_mod<- metagen( TE = log(TE), 
                     seTE = seTE, 
                     data = MA_renal,
                     studlab = Id,
                     comb.fixed = F,
                     comb.random = T,
                     prediction = F,
                     sm="OR")

renal_mod_ad<- metagen( TE = log(TE), 
                        seTE = seTE, 
                        data = MA_renal_ad,
                        studlab = Id,
                        comb.fixed = F,
                        comb.random = T,
                        prediction = F,
                        sm="OR")

### Detecting outliers and influential studies
spot.outliers.random(renal_mod)
influence.analysis(renal_mod, method.tau = "DL", hakn = F)

spot.outliers.random(renal_mod_ad)
influence.analysis(renal_mod_ad, method.tau = "DL", hakn = F)

### Small-study effect bias
##Assumption: effect sizes drive publication bias (small eff. sizes are missing)
funnel(renal_mod, xlab = "Odds ratio")
funnel(renal_mod_ad, xlab = "Odds ratio")

metabias(renal_mod, method.bias = "linreg", k.min = 4)
metabias(renal_mod_ad, method.bias = "linreg", k.min = 4)

### trim and fill procedure
TnF_renal_mod <- trimfill(renal_mod)
funnel(TnF_renal_mod, xlab = "Odds ratio")

TnF_renal_mod_ad <- trimfill(renal_mod)
funnel(TnF_renal_mod_ad, xlab = "Odds ratio")

# MA for CVS -------------------------------------------------------------------
MA_CVS <- filter(df_magen_ca, vari == "cvs")
MA_CVS_ad <- filter(df_magen_ca, vari == "cvs" & Pop == "Adults") 

CVS_mod<- metagen( TE = log(TE), 
                   seTE = seTE, 
                   data = MA_CVS,
                   studlab = Id,
                   comb.fixed = F,
                   comb.random = T,
                   prediction = F,
                   sm="OR")

CVS_mod_ad<- metagen( TE = log(TE), 
                      seTE = seTE, 
                      data = MA_CVS_ad,
                      studlab = Id,
                      comb.fixed = F,
                      comb.random = T,
                      prediction = F,
                      sm="OR")

### Detecting outliers and influential studies
spot.outliers.random(CVS_mod)
influence.analysis(CVS_mod, method.tau = "DL", hakn = F)

spot.outliers.random(CVS_mod_ad)
influence.analysis(CVS_mod_ad, method.tau = "DL", hakn = F)

### Small-study effect bias
##Assumption: effect sizes drive publication bias (small eff. sizes are missing)
funnel(CVS_mod, xlab = "Odds ratio")
funnel(CVS_mod_ad, xlab = "Odds ratio")

metabias(CVS_mod, method.bias = "linreg", k.min = 4 )
metabias(CVS_mod_ad, method.bias = "linreg", k.min = 4 )
### trim and fill procedure
TnF_CVS_mod <- trimfill(CVS_mod)
funnel(TnF_CVS_mod, xlab = "Odds ratio")

TnF_CVS_mod_ad <- trimfill(CVS_mod_ad)
funnel(TnF_CVS_mod_ad, xlab = "Odds ratio")

# Rash -------------------------------------------------------------------------
MA_rash <- filter(df_magen_ca, vari == "rash")

rash_mod<- metagen( TE = log(TE), 
                    seTE = seTE, 
                    data = MA_rash,
                    studlab = Id,
                    comb.fixed = F,
                    comb.random = T,
                    prediction = F,
                    sm="OR")

### Detecting outliers and influential studies
spot.outliers.random(rash_mod)
influence.analysis(rash_mod, method.tau = "DL", hakn = F)

### Small-study effect bias
##Assumption: effect sizes drive publication bias (small eff. sizes are missing)
funnel(rash_mod, xlab = "Odds ratio")
metabias(rash_mod, method.bias = "linreg", k.min = 4)

### trim and fill procedure
TnF_rash_mod <- trimfill(rash_mod)
funnel(TnF_rash_mod, xlab = "Odds ratio")

# Vomit ------------------------------------------------------------------------
MA_vomit <- filter(df_magen_ca, vari == "vomit")
MA_vomit_ch <- filter(df_magen_ca, vari == "vomit" & Pop == "Children")
vomit_mod<- metagen( TE = log(TE), 
                     seTE = seTE, 
                     data = MA_vomit,
                     studlab = Id,
                     comb.fixed = F,
                     comb.random = T,
                     prediction = F,
                     sm="OR")

vomit_mod_ch<- metagen( TE = log(TE), 
                        seTE = seTE, 
                        data = MA_vomit,
                        studlab = Id,
                        comb.fixed = F,
                        comb.random = T,
                        prediction = F,
                        sm="OR")

vomit_mod_cl <-update.meta(vomit_mod, 
                           byvar = MA_vomit$Cl,
                           comb.random = T,
                           comb.fixed = F)


### Detecting outliers and influential studies
spot.outliers.random(vomit_mod)
influence.analysis(vomit_mod, method.tau = "DL", hakn = F)

spot.outliers.random(vomit_mod_ch)
influence.analysis(vomit_mod_ch, method.tau = "DL", hakn = F)

spot.outliers.random(vomit_mod_cl)
influence.analysis(vomit_mod_cl, method.tau = "DL", hakn = F)

### Small-study effect bias
##Assumption: effect sizes drive publication bias (small eff. sizes are missing)
funnel(vomit_mod, xlab = "Odds ratio")
funnel(vomit_mod_ch, xlab = "Odds ratio")
funnel(vomit_mod_cl, xlab = "Odds ratio")

metabias(vomit_mod, method.bias = "linreg", k.min = 4)
metabias(vomit_mod_ch, method.bias = "linreg", k.min = 4)
metabias(vomit_mod_cl, method.bias = "linreg", k.min = 4)
### trim and fill procedure
TnF_vomit_mod <- trimfill(vomit_mod)
funnel(TnF_vomit_mod, xlab = "Odds ratio")

TnF_vomit_mod_ch <- trimfill(vomit_mod_ch)
funnel(TnF_vomit_mod_ch, xlab = "Odds ratio")

TnF_vomit_mod_cl <- trimfill(vomit_mod_cl)
funnel(TnF_vomit_mod_cl, xlab = "Odds ratio")

# MA for Abdominal_pain --------------------------------------------------------
MA_abpain <- filter(df_magen_ca, vari == "abpain")
MA_abpain_ch <- filter(df_magen_ca, vari == "abpain" & Pop == "Children")

abpain_mod<- metagen( TE = log(TE), 
                      seTE = seTE, 
                      data = MA_abpain,
                      studlab = Id,
                      comb.fixed = F,
                      comb.random = T,
                      prediction = F,
                      sm="OR")

abpain_mod_ch<- metagen( TE = log(TE), 
                         seTE = seTE, 
                         data = MA_abpain_ch,
                         studlab = Id,
                         comb.fixed = F,
                         comb.random = T,
                         prediction = F,
                         sm="OR")

MA_abpain_cl <- update.meta(abpain_mod, 
                            byvar = MA_abpain$Cl,
                            comb.random = T,
                            comb.fixed = F)

### Detecting outliers and influential studies
spot.outliers.random(abpain_mod)
influence.analysis(abpain_mod, method.tau = "DL", hakn = F)

spot.outliers.random(abpain_mod_ch)
influence.analysis(abpain_mod_ch, method.tau = "DL", hakn = F)

spot.outliers.random(MA_abpain_cl)
influence.analysis(MA_abpain_cl, method.tau = "DL", hakn = F)

### Small-study effect bias
##Assumption: effect sizes drive publication bias (small eff. sizes are missing)
funnel(abpain_mod, xlab = "Odds ratio")
funnel(MA_abpain_cl, xlab = "Odds ratio")

metabias(abpain_mod, method.bias = "linreg", k.min = 4 )

### trim and fill procedure
TnF_abpain_mod <- trimfill(abpain_mod)
funnel(TnF_abpain_mod, xlab = "Odds ratio")

# Headache ---------------------------------------------------------------------
MA_headache <- filter(df_magen_ca, vari == "headache")

headache_mod<- metagen( TE = log(TE), 
                        seTE = seTE, 
                        data = MA_headache,
                        studlab = paste(Au , paste0("(",Yr,")")),
                        comb.fixed = F,
                        comb.random = T,
                        prediction = F,
                        sm="OR")

### Detecting outliers and influential studies
spot.outliers.random(headache_mod)
influence.analysis(headache_mod, method.tau = "DL", hakn = F)

### Small-study effect bias
##Assumption: effect sizes drives publication bias (small eff. sizes are missing)
funnel(headache_mod, xlab = "Odds ratio")
metabias(headache_mod, method.bias = "linreg", k.min = 4)

# All bleeding -----------------------------------------------------------------
MA_bleed <- filter(df_magen_ca, vari == "bleed")
MA_bleed_ch <- filter(df_magen_ca, vari == "bleed" & Pop == "Children")

bleed_mod<- metagen( TE = log(TE), 
                     seTE = seTE, 
                     data = MA_bleed,
                     studlab = Id,
                     comb.fixed = F,
                     comb.random = T,
                     prediction = F,
                     sm="OR")

bleed_mod_def <-update.meta(bleed_mod, 
                            byvar = MA_bleed$bl.def,
                            comb.random = T,
                            comb.fixed = F)

bleed_mod_cl <-update.meta(bleed_mod, 
                           byvar = MA_bleed$Cl,
                           comb.random = T,
                           comb.fixed = F)

bleed_mod_ch<- metagen( TE = log(TE), 
                        seTE = seTE, 
                        data = MA_bleed_ch,
                        studlab = Id,
                        comb.fixed = F,
                        comb.random = T,
                        prediction = F,
                        sm="OR")

### Detecting outliers and influential studies
spot.outliers.random(bleed_mod)
influence.analysis(bleed_mod, method.tau = "DL", hakn = F)

spot.outliers.random(bleed_mod_ch)
influence.analysis(bleed_mod_ch, method.tau = "DL", hakn = F)

### Small-study effect bias
##Assumption: effect sizes drive publication bias (small eff. sizes are missing)
funnel(bleed_mod, xlab = "Odds ratio")
metabias(bleed_mod, method.bias = "linreg", k.min = 4)

funnel(bleed_mod_ch, xlab = "Odds ratio")
metabias(bleed_mod_ch, method.bias = "linreg", k.min = 4)

### trim and fill procedure
TnF_bleed_mod <- trimfill(bleed_mod)
funnel(TnF_bleed_mod, xlab = "Odds ratio")

# Tourniquet -------------------------------------------------------------------
MA_touniquet <- filter(df_magen_ca, vari == "tt")
tourniquet_mod<- metagen( TE = log(TE), 
                          seTE = seTE, 
                          data = MA_touniquet,
                          studlab = paste(Au , paste0("(",Yr,")")),
                          comb.fixed = F,
                          comb.random = T,
                          prediction = F,
                          sm="OR")

### Detecting outliers and influential studies
spot.outliers.random(tourniquet_mod)
influence.analysis(tourniquet_mod, method.tau = "DL", hakn = F)

### Small-study effect bias
##Assumption: effect sizes drive publication bias (small eff. sizes are missing)
funnel(tourniquet_mod, xlab = "Odds ratio")
metabias(tourniquet_mod, method.bias = "linreg", k.min = 4)

### trim and fill procedure
TnF_touniquet_mod <- trimfill(tourniquet_mod)
funnel(TnF_touniquet_mod, xlab = "Odds ratio")

# MA for body fluid effusion ---------------------------------------------------
MA_Eff <- filter(df_magen_ca, vari == "cfa")

CFA_mod<- metagen( TE = log(TE), 
                   seTE = seTE, 
                   data = MA_Eff,
                   studlab = Id,
                   comb.fixed = F,
                   comb.random = T,
                   prediction = F,
                   sm="OR")

### Detecting outliers and influential studies
spot.outliers.random(CFA_mod)
influence.analysis(CFA_mod, method.tau = "DL", hakn = F)

### Small-study effect bias
##Assumption: effect sizes drive publication bias (small eff. sizes are missing)
funnel(CFA_mod, xlab = "Odds ratio")

### trim and fill procedure
TnF_CFA_mod <- trimfill(CFA_mod)
funnel(TnF_CFA_mod, xlab = "Odds ratio")

# Immune status ----------------------------------------------------------------
MA_imres <- filter(df_magen_ca, vari == "imres")
MA_imres_ch <- filter(df_magen_ca, vari == "imres" & Pop == "Children")
MA_imres_ad <- filter(df_magen_ca, vari == "imres" & Pop == "Adults")
imres_mod <- metagen( TE = log(TE), 
                      seTE = seTE, 
                      data = MA_imres,
                      studlab = Id,
                      comb.fixed = F,
                      comb.random = T,
                      prediction = F,
                      sm="OR")

imres_mod_ch <- metagen( TE = log(TE), 
                         seTE = seTE, 
                         data = MA_imres_ch,
                         studlab = Id,
                         comb.fixed = F,
                         comb.random = T,
                         prediction = F,
                         sm="OR")

imres_mod_ad <- metagen( TE = log(TE), 
                         seTE = seTE, 
                         data = MA_imres_ad,
                         studlab = Id,
                         comb.fixed = F,
                         comb.random = T,
                         prediction = F,
                         sm="OR")

### Detecting outliers and influential studies
spot.outliers.random(imres_mod)
influence.analysis(imres_mod, method.tau = "DL", hakn = F)

spot.outliers.random(imres_mod_ch)
influence.analysis(imres_mod_ch, method.tau = "DL", hakn = F)

spot.outliers.random(imres_mod_ad)
influence.analysis(imres_mod_ad, method.tau = "DL", hakn = F)

##subgroup analysis for classification

imres_mod_cl<-update.meta(imres_mod, 
                          byvar =MA_imres$Cl,
                          comb.random = T,
                          comb.fixed = F)

### Small-study effect bias
##Assumption: effect sizes drive publication bias (small eff. sizes are missing)
funnel(imres_mod, xlab = "Odds ratio")
metabias(imres_mod, method.bias = "linreg")

funnel(imres_mod_ch, xlab = "Odds ratio")
metabias(imres_mod_ch, method.bias = "linreg")

funnel(imres_mod_ad, xlab = "Odds ratio")
metabias(imres_mod_ad, method.bias = "linreg")

# Nutritional status -----------------------------------------------------------
MA_nutri_paired <- pairwise(treat = nutri_st, 
                            event = events, 
                            n= n_grp, 
                            studlab = Id, 
                            data = df_nutri, 
                            sm = "OR")

Nutri_mod<- netmeta(TE, seTE, 
                    nutri_st1, 
                    nutri_st2, 
                    studlab, 
                    data = MA_nutri_paired, 
                    comb.fixed = FALSE, 
                    comb.random = TRUE, 
                    reference.group = 2)

Compare_nutrimod <- netsplit(Nutri_mod)

results_nutri <- data.frame( "OR" = exp(Compare_nutrimod$random$TE), 
                             "95%IC:Lower" = exp(Compare_nutrimod$random$lower), 
                             "95%IC:Upper" = exp(Compare_nutrimod$random$upper))

# Serotype ---------------------------------------------------------------------
MA_serotype_ch <- filter(df_serotype, Population_group == "Children")
MA_serotype_ad <- filter(df_serotype, Population_group == "Adults")

MA_serotype_paired <- pairwise(treat = MA_serotype$serotype, 
                               event = events, 
                               n= n_grp, 
                               studlab = Id, 
                               data = df_serotype, 
                               sm = "OR")

MA_serotype_paired_ch <- pairwise(treat = MA_serotype_ch$serotype, 
                                  event = events, 
                                  n= n_grp, 
                                  studlab = Id, 
                                  data = MA_serotype_ch, 
                                  sm = "OR")

MA_serotype_paired_ad <- pairwise(treat = MA_serotype_ad$serotype, 
                                  event = events, 
                                  n= n_grp, 
                                  studlab = Id, 
                                  data = MA_serotype_ad, 
                                  sm = "OR")

Serotype_mod<- netmeta(TE, seTE, serotype1, serotype2, studlab, 
                       data = MA_serotype_paired, comb.fixed = FALSE, 
                       comb.random = TRUE, reference.group = 1)

Serotype_mod_ch<- netmeta(TE, seTE, serotype1, serotype2, studlab, 
                          data = MA_serotype_paired_ch, comb.fixed = FALSE, 
                          comb.random = TRUE, reference.group = 1)

Serotype_mod_ad<- netmeta(TE, seTE, serotype1, serotype2, studlab, 
                          data = MA_serotype_paired_ad, comb.fixed = FALSE, 
                          comb.random = TRUE, reference.group = 1)

Compare_serotypemod <- netsplit(Serotype_mod)
Compare_serotypemod_ch <- netsplit(Serotype_mod_ch)
Compare_serotypemod_ad <- netsplit(Serotype_mod_ad)

results_serotype <- data.frame( 
  "OR" = exp(Compare_serotypemod$direct.random$TE), 
  "95%IC:Lower" = exp(Compare_serotypemod$direct.random$lower), 
  "95%IC:Upper" = exp(Compare_serotypemod$direct.random$upper))

results_serotype_ch <- data.frame( 
  "OR" = exp(Compare_serotypemod_ch$direct.random$TE), 
  "95%IC:Lower" = exp(Compare_serotypemod_ch$direct.random$lower), 
  "95%IC:Upper" = exp(Compare_serotypemod_ch$direct.random$upper))

results_serotype_ad <- data.frame( 
  "OR" = exp(Compare_serotypemod_ad$direct.random$TE),
  "95%IC:Lower" = exp(Compare_serotypemod_ad$direct.random$lower), 
  "95%IC:Upper" = exp(Compare_serotypemod_ad$direct.random$upper))

# Create the summary forest plot of factors reported as standard 
# mean differences
tabletext_1 <- read.csv("summary_meta-analysis_SMD.csv")
data_1 <- read.csv("data_1.csv")
workdir <- getwd()
pdf(file.path(workdir,"Figure_Pooled_SMD.pdf"), width= 15.98*2, 
    height= 11.93*2)
forestplot(labeltext=tabletext_1, graph.pos=4, 
           mean=c(NA,data_1$SMD), 
           lower=c(NA, data_1$LoCI), upper=c(NA, data_1$UpCI),
           title="",
           graphwidth = unit(90, "mm"),
           grid = T,
           clip = c(-1.5,2),
           txt_gp=fpTxtGp(label=list(gpar(cex=1.5, fontfamily = "Times" )),
                          ticks=gpar(cex=1.2, fontfamily = "Times"),
                          xlab=gpar(cex = 1.5, fontfamily = "Times"),
                          title=gpar(cex = 1.2)),
           hrzl_lines=list("2" = gpar(lwd=2, col="black",columns=c(1:6) )),
           xlab = "                      ",
           is.summary=c(T, rep(F, times=nrow(tabletext_1)-1)),
           col=fpColors(box= "darkblue"  , lines="black", zero = "gray50"),
           zero=0, cex=0.9, lineheight = unit(9,"mm"), 
           boxsize=0.3, colgap=unit(7,"mm"),lwd.ci=2, ci.vertices=F, 
           ci.vertices.height = 0.2)
dev.off()

# Create the summary forest plot of factors reported as odds ratio
tabletext_2 <- read.csv("summary_meta-analysis_ORs.csv")
data_2 <- read.csv("data_2.csv")
pdf(file.path(workdir,"Figures_Pooled_OR.pdf"), width=8.27*3,
    height= 11.69*3 )
forestplot(labeltext=tabletext_2, graph.pos=5, 
           mean=c(NA,data_2$OR), 
           lower=c(NA, data_2$LoCI), upper=c(NA, data_2$UpCI),
           title="",
           graphwidth = unit(150, "mm"),
           clip = c(0.1,5),
           grid = TRUE,
           hrzl_lines=list("3" = gpar(lwd=2, col="black",
                                      columns=c(1:7))),
           xlab="<--Less severe--- OR    ---More severe-->",
           txt_gp=fpTxtGp(label=list(gpar(cex=1.5, fontfamily = "Times" )),
                          ticks=gpar(cex=1.2, fontfamily = "Times"),
                          xlab=gpar(cex = 1.5, fontfamily = "Times"),
                          title=gpar(cex = 1.2)),
           col=fpColors(box= "darkblue", lines="black", zero = "gray50"),
           zero=1, cex=0.9, lineheight = unit(9,"mm"), boxsize=0.3, 
           colgap=unit(7,"mm"),lwd.ci=2, ci.vertices=F, 
           ci.vertices.height = 0.2)
dev.off()

