# load packages ----------------------------------------------------------------
pacman::p_load("tidyverse", "haven", "Matrix", "lme4")

# load data --------------------------------------------------------------------
df_finish <- readRDS("./data/df_finish.RDS")

# separate data sets by gender -------------------------------------------------
sample_f <- df_finish[df_finish$female=="1",]
sample_m <- df_finish[df_finish$female=="0",]



############################### Modelle FE ################################ 

### Zero models women and men --------------------------------------------------

# women ------------------------------------------------------------------------
f_mod0 <- lmer(distance_finish ~ (1|id) , data = sample_f)
performance::icc(f_mod0) 

# men --------------------------------------------------------------------------
m_mod0 <- lmer(distance_finish ~ (1|id) , data = sample_m)
performance::icc(m_mod0)


### multilevel regression by gender with interaction ---------------------------

# women ------------------------------------------------------------------------
f_mod1 <- lmer(distance_finish ~ online + year + (1|id) , data = sample_f)
f_mod2 <- lmer(distance_finish ~ online*year + (1|id) , data = sample_f)
summary(f_mod1)

texreg::screenreg(list(f_mod1,f_mod2))
performance::icc(f_mod1)


# men --------------------------------------------------------------------------
m_mod1 <- lmer(distance_finish ~ online + year + (1|id) , data = sample_m)
m_mod2 <- lmer(distance_finish ~ online*year + (1|id) , data = sample_m)
summary(m_mod1)



### multilevel regression by gender with interaction (complete model) ----------
# women ------------------------------------------------------------------------
f_mod3 <- lmer(distance_finish ~ online + year + gkpol_fct + isced_fct +  cohort + migstatus + (1|id) , data = sample_f)
f_mod4 <- lmer(distance_finish ~ online*year + gkpol_fct + isced_fct +  cohort + migstatus + (1|id) , data = sample_f)
summary(f_mod3)

texreg::screenreg(list(f_mod3,f_mod4))


# men --------------------------------------------------------------------------
m_mod3 <- lmer(distance_finish ~ online + year + gkpol_fct + isced_fct +  cohort + migstatus + (1|id) , data = sample_m)
m_mod4 <- lmer(distance_finish ~ online*year + gkpol_fct + isced_fct +  cohort + migstatus + (1|id) , data = sample_m)
texreg::screenreg(list(m_mod3,m_mod4))



### multilevel regression by gender with interaction and random slope for year (full model)
# Anmerkung: nicht möglich, da Fallzahl zu klein
# women ------------------------------------------------------------------------
# f_mod5 <- lmer(distance_finish ~ online*year + gkpol_fct + isced_fct +  cohort + migstatus + (year|id) , data = sample_f)
# texreg::screenreg(list(f_mod5))

# men --------------------------------------------------------------------------
# m_mod5 <- lmer(distance_finish ~ online*year + gkpol_fct + isced_fct +  cohort + migstatus + (year|id) , data = sample_m)
# summary(m_mod5)



### multilevel regression by gender with interaction (3-level model with ID in Year)
# Anmerkung: funktioniert so auch nicht
# women ------------------------------------------------------------------------
# f_mod6 <- lmer(distance_finish ~ online*year + gkpol_fct + isced_fct +  cohort + migstatus + (1|id:year) , data = sample_f)
# summary(f_mod6)
# 
# men --------------------------------------------------------------------------
# m_mod6 <- lmer(distance_finish ~ online*year + gkpol_fct + isced_fct +  cohort + migstatus + (1|id:year) , data = sample_m)
# summary(m_mod6)