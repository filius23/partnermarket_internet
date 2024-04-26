####################  PAKETE  ####################  

pacman::p_load("tidyverse", "texreg", "marginaleffects", "ggeffects", "modelsummary", "flextable", "plm") #"ggfortify"





#######################  FILE/DATENSATZ FÜR DIE ANALYSE  ####################### 

# setwd("C:/Users/HP/Documents/Datensätze/pairfam_Welle11/pairfam Welle11/Data/Stata")
list.files()

df_finish <- readRDS("./data/df_finish.RDS")

windowsFonts(Times=windowsFont("Times New Roman"))





##################### getrennte Datensätze nach Geschlecht ##################### 

sample_f <- df_finish[df_finish$frau=="1",]
sample_m <- df_finish[df_finish$frau=="0",]





############################### Modelle FE ################################ 

### Panelregression nach Geschlecht mit Interaktion 

# Frauen
fixed_ia_f <- plm(distanz_finish ~ online*year, data = sample_f, index = "id", model="random")
summary(fixed_ia_f)

# Männer
fixed_ia_m <- plm(distanz_finish ~ online*year, data = sample_m, index = "id", model = "random")
summary(fixed_ia_m)

texreg::screenreg(list(fixed_ia_f,fixed_ia_m)) # gleiches Ergebnis



### vorhergesagte Werte und Graphik

# Frauen
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
  scale_color_manual(values = c("black","grey"), breaks = c(0,1), labels = c("Offline","Online")) +
  scale_x_continuous(breaks = seq(2009,2017,2)) + 
  labs(#title = "Vorhergesagte Distanzen für die Frauen",
    subtitle = "Adjusted Predictions für die Frauen",
    color = "Kennenlernort",
    x = "Jahr", y = "vorhergesagter Wert") +
  theme(legend.position="bottom")
aa
ggsave("aa.png", width = 12, height = 12, units = "cm")


# Männer
pred_fe_m <- ggeffect(fixed_ia_m, terms = c("online[0,1]","year[2,3,4,5,6,7,8,9,10,11]"))
head(data.frame(pred_fe_m))
class(pred_fe_m$group)
pred_fe_m$group <- as.numeric(as.character(pred_fe_m$group))
class(pred_fe_m$group)

bb <- ggplot(pred_fe_m, aes(x = group + 2007 + ifelse(x==0,-.075,+.075), y = predicted, color = factor(x), group = x)) +
  geom_point() + 
  geom_line() + # durch Linien verbinden
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 1) +
  theme_minimal(base_size = 11, base_family = "Times") +
  scale_color_manual(values = c("black","grey"), breaks = c(0,1), labels = c("Offline","Online")) +
  scale_x_continuous(breaks = seq(2009,2017,2)) + 
  labs(#title = "Vorhergesagte Distanzen für die Männer",
    subtitle = "Adjusted Predictions für die Männer",
    color = "Kennenlernort",
    x = "Jahr", y = "vorhergesagter Wert") +
  theme(legend.position="bottom")
bb
ggsave("bb.png", width = 12, height = 12, units = "cm")