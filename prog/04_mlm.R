# load packages ----------------------------------------------------------------
library(tidyverse)
library(haven)
library(Matrix)
library(lme4)


# load data --------------------------------------------------------------------
df_finish <- readRDS("./data/df_finish.RDS")


######################### PACKAGES ######################### 

library(tidyverse)
# tools::package_dependencies("Matrix", which = "LinkingTo", reverse=TRUE)[[1]]
# install.packages("lme4")
library(Matrix)
library(lme4)



###############################

setwd("C://Users/HP/Documents/DatensÃ¤tze/pairfam_Welle11/pairfam Welle11/Data/Stata/")
df_finish <- readRDS("df_finish.RDS")


##################### getrennte DatensÃ¤tze nach Geschlecht ##################### 
sample_f <- df_finish[df_finish$frau=="1",]
sample_m <- df_finish[df_finish$frau=="0",]


install.packages("usethis")
library(usethis)
use_git_config(user.name = "leena96", user.email = "l-maass@t-online.de")


############################### Modelle FE ################################ 

### Mehrebenenregression nach Geschlecht mit Interaktion 

# Frauen -----------------------------------------------------------------------
f_mod1 <- lmer(distanz_finish ~ online + year + (1|id) , data = sample_f)
f_mod2 <- lmer(distanz_finish ~ online*year + (1|id) , data = sample_f)
summary(f_mod1)

# Männer -----------------------------------------------------------------------
m_mod1 <- lmer(distanz_finish ~ online + year + (1|id) , data = sample_m)
m_mod2 <- lmer(distanz_finish ~ online*year + (1|id) , data = sample_m)
summary(m_mod1)



### Mehrebenenregression nach Geschlecht mit Interaktion vollstÃ¤ndig
# Frauen -----------------------------------------------------------------------
f_mod3 <- lmer(distanz_finish ~ online + year + gkpol_kat1 + isced2 +  cohort + migrationshintergrund + (1|id) , data = sample_f)
f_mod4 <- lmer(distanz_finish ~ online*year + gkpol_kat1 + isced2 +  cohort + migrationshintergrund + (1|id) , data = sample_f)
summary(f_mod1)

# Männer -----------------------------------------------------------------------
m_mod3 <- lmer(distanz_finish ~ online + year + gkpol_kat1 + isced2 +  cohort + migrationshintergrund + (1|id) , data = sample_m)
m_mod4 <- lmer(distanz_finish ~ online*year + gkpol_kat1 + isced2 +  cohort + migrationshintergrund + (1|id) , data = sample_m)
summary(m_mod1)



### Mehrebenenregression nach Geschlecht mit Interaktion vollstÃ¤ndig mit Random Slope fÃ¼r Jahr
# Frauen -----------------------------------------------------------------------
f_mod5 <- lmer(distanz_finish ~ online*year + gkpol_kat1 + isced2 +  cohort + migrationshintergrund + (year|id) , data = sample_f)
summary(f_mod5)

# Männer -----------------------------------------------------------------------
m_mod5 <- lmer(distanz_finish ~ online*year + gkpol_kat1 + isced2 +  cohort + migrationshintergrund + (year|id) , data = sample_m)
summary(m_mod5)



### Mehrebenenregression nach Geschlecht mit Interaktion vollstÃ¤ndig 3-Ebenen-Modell mit ID in Year
# Frauen -----------------------------------------------------------------------
f_mod6 <- lmer(distanz_finish ~ online*year + gkpol_kat1 + isced2 +  cohort + migrationshintergrund + (1|id:year) , data = sample_f)
summary(f_mod6)

# Männer -----------------------------------------------------------------------
m_mod6 <- lmer(distanz_finish ~ online*year + gkpol_kat1 + isced2 +  cohort + migrationshintergrund + (1|id:year) , data = sample_m)
summary(m_mod6)