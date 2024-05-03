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




##################### getrennte DatensÃ¤tze nach Geschlecht ##################### 
sample_f <- df_finish[df_finish$frau=="1",]
sample_m <- df_finish[df_finish$frau=="0",]



############################### Modelle FE ################################ 

### Nullmodelle Frauen und Männer ----------------------------------------------
f_mod0 <- lmer(distanz_finish ~ (1|id) , data = sample_f)
performance::icc(f_mod0) 

m_mod0 <- lmer(distanz_finish ~ (1|id) , data = sample_m)
performance::icc(m_mod0)



### Mehrebenenregression nach Geschlecht mit Interaktion -----------------------

# Frauen -----------------------------------------------------------------------
f_mod1 <- lmer(distanz_finish ~ online + year + (1|id) , data = sample_f)
f_mod2 <- lmer(distanz_finish ~ online*year + (1|id) , data = sample_f)
summary(f_mod1)

texreg::screenreg(list(f_mod1,f_mod2))
performance::icc(f_mod1)


# Männer -----------------------------------------------------------------------
m_mod1 <- lmer(distanz_finish ~ online + year + (1|id) , data = sample_m)
m_mod2 <- lmer(distanz_finish ~ online*year + (1|id) , data = sample_m)
summary(m_mod1)



### Mehrebenenregression nach Geschlecht mit Interaktion vollstaendig ----------
# Frauen -----------------------------------------------------------------------
f_mod3 <- lmer(distanz_finish ~ online + year + gkpol_kat1 + isced_2 +  cohort + migstatus + (1|id) , data = sample_f)
f_mod4 <- lmer(distanz_finish ~ online*year + gkpol_kat1 + isced_2 +  cohort + migstatus + (1|id) , data = sample_f)
summary(f_mod3)

texreg::screenreg(list(f_mod3,f_mod4))


# Männer -----------------------------------------------------------------------
m_mod3 <- lmer(distanz_finish ~ online + year + gkpol_kat1 + isced_2 +  cohort + migstatus + (1|id) , data = sample_m)
m_mod4 <- lmer(distanz_finish ~ online*year + gkpol_kat1 + isced_2 +  cohort + migstatus + (1|id) , data = sample_m)
texreg::screenreg(list(m_mod3,m_mod4))



### Mehrebenenregression nach Geschlecht mit Interaktion vollstÃ¤ndig mit Random Slope fÃ¼r Jahr 
# Anmerkung: nicht möglich
# Frauen -----------------------------------------------------------------------
# f_mod5 <- lmer(distanz_finish ~ online*year + gkpol_kat1 + isced_2 +  cohort + migstatus + (year|id) , data = sample_f)
# texreg::screenreg(list(f_mod5))

# Männer -----------------------------------------------------------------------
# m_mod5 <- lmer(distanz_finish ~ online*year + gkpol_kat1 + isced_2 +  cohort + migstatus + (year|id) , data = sample_m)
# summary(m_mod5)



### Mehrebenenregression nach Geschlecht mit Interaktion vollstaendig 3-Ebenen-Modell mit ID in Year
# Anmerkung: funktioniert so auch nicht
# Frauen -----------------------------------------------------------------------
# f_mod6 <- lmer(distanz_finish ~ online*year + gkpol_kat1 + isced_2 +  cohort + migstatus + (1|id:year) , data = sample_f)
# summary(f_mod6)
# 
# # Männer -----------------------------------------------------------------------
# m_mod6 <- lmer(distanz_finish ~ online*year + gkpol_kat1 + isced_2 +  cohort + migstatus + (1|id:year) , data = sample_m)
# summary(m_mod6)