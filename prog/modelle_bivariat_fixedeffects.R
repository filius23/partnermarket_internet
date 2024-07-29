# load packages ----------------------------------------------------------------
pacman::p_load("tidyverse", "texreg", "marginaleffects", "ggeffects", "modelsummary", "flextable", "plm")

# load data --------------------------------------------------------------------
df_finish <- readRDS("./data/df_finish.RDS")
windowsFonts(Times=windowsFont("Times New Roman"))

# separate data sets by gender -------------------------------------------------
sample_f <- df_finish[df_finish$frau=="1",]
sample_m <- df_finish[df_finish$frau=="0",]





################################## Models FE ################################### 

### Panel regression by gender with interaction --------------------------------

# women
fixed_ia_f <- plm(distanz_finish ~ online*year, 
                  data = sample_f, index = "id", model="random")
summary(fixed_ia_f)

# men
fixed_ia_m <- plm(distanz_finish ~ online*year, 
                  data = sample_m, index = "id", model = "random")
summary(fixed_ia_m)

texreg::screenreg(list(fixed_ia_f,fixed_ia_m))


### predicted values and graph -------------------------------------------------
# woman
pred_fe_f <- ggeffect(fixed_ia_f, terms = c("online[0,1]","year[2,3,4,5,6,7,8,9,10,11]"))
head(data.frame(pred_fe_f))
class(pred_fe_f$group)
pred_fe_f$group <- as.numeric(as.character(pred_fe_f$group))
class(pred_fe_f$group)

aa <- ggplot(pred_fe_f, aes(x = group + 2007 + ifelse(x==0,-.075,+.075), y = predicted, color = factor(x), group = x)) +
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 1) +
  theme_minimal(base_size = 11, base_family = "Times") +
  scale_color_manual(values = c("black","grey"), breaks = c(0,1), labels = c("offline","online")) +
  scale_x_continuous(breaks = seq(2009,2017,2)) + 
  labs(subtitle = "Adjusted predictions women",
    color = "meeting place",
    x = "year", y = "predicted value") +
  theme(legend.position="bottom")
aa
ggsave("aa.png", width = 12, height = 12, units = "cm")


# Men
pred_fe_m <- ggeffect(fixed_ia_m, terms = c("online[0,1]","year[2,3,4,5,6,7,8,9,10,11]"))
head(data.frame(pred_fe_m))
class(pred_fe_m$group)
pred_fe_m$group <- as.numeric(as.character(pred_fe_m$group))
class(pred_fe_m$group)

bb <- ggplot(pred_fe_m, aes(x = group + 2007 + ifelse(x==0,-.075,+.075), y = predicted, color = factor(x), group = x)) +
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 1) +
  theme_minimal(base_size = 11, base_family = "Times") +
  scale_color_manual(values = c("black","grey"), breaks = c(0,1), labels = c("offline","oOnline")) +
  scale_x_continuous(breaks = seq(2009,2017,2)) + 
  labs(subtitle = "Adjusted predictions men",
    color = "meeting place",
    x = "year", y = "predicted value") +
  theme(legend.position="bottom")
bb
