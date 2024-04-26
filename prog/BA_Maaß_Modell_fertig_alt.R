####################  PAKETE  ####################  

library(ggfortify)
library(ggeffects)
library()
library()  #fixest feols(AV ~ UV | id , trade)
library()



pacman::p_load("tidyverse", "texreg", "margins", "fixest", "ggeffects", )

#######################  FILE/DATENSATZ FÜR DIE ANALYSE  ####################### 

setwd("C:/Users/HP/Documents/Datensätze/pairfam_Welle11/pairfam Welle11/Data/Stata")
list.files()

df_finish <- readRDS("df_finish.RDS")

windowsFonts(Times=windowsFont("Times New Roman"))


#####################

#  ansehen, für wieviele es eine Online und eine Offline Kennenlernen gibt:
df_finish %>% group_by(id) %>% mutate(on_off = online == lag(online)) %>% xtabs(~on_off,.,addNA = T)

# zusätzlich n als Info, für wieviele es überhaupt mehrere Paarbildungen gibt:
df_finish %>% group_by(id) %>% mutate(on_off = online == lag(online), n = 1:n()) %>% xtabs(~on_off+n,.,addNA = T)




##################### getrennte Datensätze nach Geschlecht ##################### 

sample_f <- df_finish[df_finish$frau=="1",]
sample_m <- df_finish[df_finish$frau=="0",]

df_finish$online <- as.factor(df_finish$online)


############################### Modelle gepoolt ################################ 

# Nullmodelle lm für beide Geschlechter

nm_f <- lm(distanz_finish ~ online, data = sample_f)
summary(nm_f)

nm_m <- lm(distanz_finish ~ online, data = sample_m)
summary(nm_m)

texreg::screenreg(list(nm_f,nm_m)) # gleiches Ergebnis


# Nullmodell lm mit Interaktion für beide Geschlechter

nm_ia_f <- lm(distanz_finish ~ online*year, data = sample_f)
summary(nm_ia_f)

nm_ia_m <- lm(distanz_finish ~ online*year, data = sample_m)
summary(nm_ia_m)

texreg::screenreg(list(nm_ia_f,nm_ia_m)) # gleiches Ergebnis



#### Extraktion der Regressionsergebnisse nach Geschlecht (Nm und Nm mit IA nach Geschlecht)

library(modelsummary)
library(flextable)

#weiblich
modelsummary(list("Bivariates Modell"=nm_f,"Modell mit Interaktion"=nm_ia_f),
             output = "flextable", 
             gof_omit = "IC|F|L",stars = T,fmt ="%.3f") %>%
  set_caption("Regressionsergebnisse der Frauen") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>%
  theme_booktabs() %>%
  autofit() %>%
  save_as_docx("output_nm_f", path = "output_nm_f_A.docx")


#männlich
modelsummary(list("Bivariates Modell"=nm_m,"Modell mit Interaktion"=nm_ia_m),
             output = "flextable", 
             gof_omit = "IC|F|L",stars = T,fmt ="%.3f") %>%
  set_caption("Regressionsergebnisse der Männer") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>%
  theme_booktabs() %>%
  autofit() %>%
  save_as_docx("output_nm_m", path = "output_nm_m_A.docx")



### vorhergesagte Werte und Grahik

### Frauen nm_f
nm_f <- ggplot(sample_f, aes(x=year+2007+ ifelse(online==0,-.075,+.075),y=distanz_finish,color=factor(online)))+
  geom_smooth(method="lm", se =F)+
  theme_minimal(base_size = 11, base_family = "Times")+
  scale_color_manual(values = c("black","grey"), breaks = c(0,1), labels = c("Offline","Online"))+
  scale_x_continuous(breaks = seq(2009,2017,2)) +
  labs(#title = "Vorhergesagte Distanzen von Frauen",
    color = "Kennenlernort",
    x = "Jahr", y = "vorhergesagter Wert")+
  theme(legend.position="bottom")
nm_f
ggsave("nm_f.png", width = 12, height = 12, units = "cm")



### Männer  nm_m
nm_m <- ggplot(sample_m, aes(x=year+2007,y=distanz_finish,color=factor(online)))+
  geom_smooth(method="lm", se =F)+
  theme_minimal(base_size = 11, base_family = "Times")+
  scale_color_manual(values = c("black","grey"), breaks = c(0,1), labels = c("Offline","Online"))+
  scale_x_continuous(breaks = seq(2009,2017,2)) +
  labs(#title = "Vorhergesagte Distanzen von Männern",
    color = "Kennenlernort",
    x = "Jahr", y = "vorhergesagter Wert")+
  theme(legend.position="bottom")
nm_m
ggsave("nm_m.png", width = 12, height = 12, units = "cm")



### Modell mit IA

# Frauen Interaktionseffekt nm_ia_f

ggeffect(model = nm_ia_f, terms = c("online[0,1]"))

pred_df2_f <- ggeffect(model = nm_ia_f, terms = c("online[0,1]","year[2,3,4,5,6,7,8,9,10,11]"))
head(data.frame(pred_df2_f))

47.27-51.36
66.94-64.46
75.88-79.96 
79.96-64.46
86.76-43.19
class(pred_df2_f$group)

pred_df2_f$group <- as.numeric(as.character(pred_df2_f$group))
class(pred_df2_f$group)


ia_f <- ggplot(pred_df2_f, aes(x = group +2007 + ifelse(x==0,-.075,+.075), y = predicted, color = factor(x), group = x)) +
  geom_point() + # punkte einzeichnen/// position = jitter(width=0.5, height=0.1) funktioniert nicht
  geom_line() + # durch Linien verbinden
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), # Konfidenzintervalle
                width = 1) +
  theme_minimal(base_size = 11, base_family = "Times") +
  scale_color_manual(values = c("black","grey"), breaks = c(0,1), labels = c("Offline","Online")) +
  scale_x_continuous(breaks = seq(2009,2017,2)) + 
  labs(#title = "Vorhergesagte Distanzen für die Frauen",
    #subtitle = "Adjusted Predictions für die Frauen",
    color = "Kennenlernort",
    x = "Jahr", y = "vorhergesagter Wert") +
  theme(legend.position="bottom")
ia_f
ggsave("ia_f.png", width = 12, height = 12, units = "cm")


#männer
ggeffect(model = nm_ia_m, terms = c("online[0,1]"))
pred_df2_m <- ggeffect(model = nm_ia_m, terms = c("online[0,1]","year[2,3,4,5,6,7,8,9,10,11]"))
head(data.frame(pred_df2_m))
class(pred_df2_m$group)
pred_df2_m$group <- as.numeric(as.character(pred_df2_m$group))
class(pred_df2_m$group)


# #K=0
# 60.67-63.28  
# 45.02-47.63  
# 
# #K=1
# 62.94-58.45
##
# 63.28-58.45
# 98.88-39.80
ia_m <- ggplot(pred_df2_m, aes(x = group +2007 + ifelse(x==0,-.075,+.075), y = predicted, color = factor(x), group = x)) +
  geom_point() + # punkte einzeichnen
  geom_line() + # durch Linien verbinden
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), # Konfidenzintervalle
                width = 1) +
  theme_minimal(base_size = 11, base_family = "Times") +
  scale_color_manual(values = c("black","grey"), breaks = c(0,1), labels = c("Offline","Online")) +
  scale_x_continuous(breaks = seq(2009,2017,2)) + 
  labs(#title = "Vorhergesagte Distanzen für die Männer",
    #subtitle = "Adjusted Predictions für die Männer",
    color = "Kennenlernort",
    x = "Jahr", y = "vorhergesagter Wert") +
  theme(legend.position="bottom")
ia_m
ggsave("ia_m.png", width = 12, height = 12, units = "cm")

############################### Modelle FE ################################ 

### interaktion feols

#Frauen
feols_ia_f <- feols(distanz_finish ~ online*year| id ,data = sample_f)
summary(feols_ia_f)

#Männer
feols_ia_m <- feols(distanz_finish ~ online*year| id ,data = sample_m)
summary(feols_ia_m)

texreg::screenreg(list(feols_ia_f,feols_ia_m)) # gleiches Ergebnis



###vorhergesagte Werte und Graphik

pfad2 <- "C:/Users/HP/Dropbox/6. Semester/Bachelorarbeit/"
source(paste0(pfad2,"/fixest_erweiterung.R"))

#frauen
a <-  expand.grid("year" =seq(2,11,1),"online"=0:1)   # wichtig: hier "Kennenlernort_num"
relpred(feols_ia_f, newdata = a) # mit der numerischen Variable funktioniert es
pred_a_f <- bind_cols(a,relpred(feols_ia_f, newdata = a)) # bind_cols, um die eingesetzten Werte auch im datensatz zu haben
pred_a_f

66.024760-60.184176
42.662423-36.821839

15.456606-19.320758
30.913212-34.777364
aa <- ggplot(pred_a_f, aes(x = year +2007 + ifelse(online==0,-.075,+.075), y = fit, color = factor(online), group = online)) +
  geom_point() + # punkte einzeichnen
  geom_line() + # durch Linien verbinden
  geom_errorbar(aes(ymin = lwr, ymax = upr), # Konfidenzintervalle
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


#männer
b <-  expand.grid("year" =seq(2,11,1),"online"=0:1)   # wichtig: hier "Kennenlernort_num"
relpred(feols_ia_m, newdata = b) # mit der numerischen Variable funktioniert es
pred_b_m <- bind_cols(b,relpred(feols_ia_m, newdata = b)) # bind_cols, um die eingesetzten Werte auch im datensatz zu haben
pred_b_m

bb <- ggplot(pred_b_m, aes(x = year +2007 + ifelse(online==0,-.075,+.075), y = fit, color = factor(online), group = online)) +
  geom_point() + # punkte einzeichnen
  geom_line() + # durch Linien verbinden
  geom_errorbar(aes(ymin = lwr, ymax = upr), # Konfidenzintervalle
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

















########################### Modell vollständig Datenaufbereitung ########################### 

### Datenaufberitung
#sex
table(df_finish$sex)

as.data.frame(model.matrix(~ df_finish$sex))
as.data.frame(model.matrix(~ df_finish$sex)) %>% rename_with(.,~gsub("df_finish\\$","",.x),everything()) # umbenennen

sex_dummys <-  as.data.frame(model.matrix(~ df_finish$sex)) %>% rename_with(.,~gsub("df_finish\\$","",.x),everything()) %>% select(-`(Intercept)`)
sex_dummys

df_f2 <- bind_cols(df_finish,sex_dummys)

#gkpol
table(df_finish$gkpol)

gkpol_dummys <-  as.data.frame(model.matrix(~ df_f2$gkpol)) %>% rename_with(.,~gsub("df_f2\\$","",.x),everything()) %>% select(-`(Intercept)`)
gkpol_dummys

df_f3 <- bind_cols(df_f2,gkpol_dummys)


table(df_f3$gkpolGroßstadt_g_100.000)
table(df_f3$gkpolMittelstadt)


#isced2
table(df_f3$isced_2)

isced_dummys <-  as.data.frame(model.matrix(~ df_f3$isced_2)) %>% rename_with(.,~gsub("df_f3\\$","",.x),everything()) %>% select(-`(Intercept)`)
isced_dummys

df_f4 <- bind_cols(df_f3,isced_dummys)



### Frauen und Männer Datensatz

sample_f2 <- df_f4[df_f4$sex=="w",]
sample_m2 <- df_f4[df_f4$sex=="m",]


###Häufigkeiten

##Frauen

#ansehen, für wieviele es eine Online und eine Offline Kennenlernen gibt:
sample_f2 %>% group_by(id) %>% mutate(on_off = Kennenlernort == lag(Kennenlernort)) %>% xtabs(~on_off,.,addNA = T)
# zusätzlich n als Info, für wieviele es überhaupt mehrere Paarbildungen gibt:
sample_f2 %>% group_by(id) %>% mutate(on_off = Kennenlernort == lag(Kennenlernort), n = 1:n()) %>% xtabs(~on_off+n,.,addNA = T)


#Männer
#ansehen, für wieviele es eine Online und eine Offline Kennenlernen gibt:
sample_m2 %>% group_by(id) %>% mutate(on_off = Kennenlernort == lag(Kennenlernort)) %>% xtabs(~on_off,.,addNA = T)
# zusätzlich n als Info, für wieviele es überhaupt mehrere Paarbildungen gibt:
sample_m2 %>% group_by(id) %>% mutate(on_off = Kennenlernort == lag(Kennenlernort), n = 1:n()) %>% xtabs(~on_off+n,.,addNA = T)




############# Modell lm vollständig ############## 


### Modelle berechnen

#Frauen
lm_f <- lm(distanz_finish ~ online*year+gkpolGroßstadt_g_100.000+gkpolMittelstadt+isced_2mittel+isced_2hoch+factor(cohort),data = sample_f2) #siehe margins 2
screenreg(lm_f)

#Männer
lm_m <- lm(distanz_finish ~ online*year+gkpolGroßstadt_g_100.000+gkpolMittelstadt+isced_2mittel+isced_2hoch+factor(cohort),data = sample_m2) #siehe margins 2

#Modell anzeigen lassen
texreg::screenreg(list(lm_f,lm_m))


### Extraktion der Regressionsergebnisse OLS

modelsummary(list("OLS-Regression Frauen"=lm_f,"OLS-Regression Männer"=lm_m),
             output = "flextable", 
             gof_omit = "IC|F|L",stars = T,fmt ="%.3f") %>%
  set_caption("Regressionsergebnisse für das vollständige OLS-Modell") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>%
  theme_booktabs() %>%
  autofit() %>%
  save_as_docx("output_voll_lm", path = "output_voll_lm.docx")



########### Modell FE vollständig ############## 

table(df_finish$cohort)

1340/1981*100
### Modelle berechnen

#Frauen
mv_feols_f <- feols(distanz_finish ~ online*year+gkpolGroßstadt_g_100.000+gkpolMittelstadt+isced_2mittel+isced_2hoch| id ,data = sample_f2) #siehe margins 2
screenreg(mv_feols_f)

#Männer
mv_feols_m <- feols(distanz_finish ~ online*year+gkpolGroßstadt_g_100.000+gkpolMittelstadt+isced_2mittel+isced_2hoch| id ,data = sample_m2) #siehe margins 2
screenreg(mv_feols_m)

texreg::screenreg(list(mv_feols_f,mv_feols_m))




### Extraktion der Regressionsergebnisse feols
modelsummary(list("Fixed-Effect-Modell Frauen"=mv_feols_f,"Fixed-Effect-Modell Männer"=mv_feols_m),
             output = "flextable", 
             gof_omit = "IC|F|L",stars = T,fmt ="%.3f") %>%
  set_caption("Regressionsergebnisse für das vollständige Modell") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>%
  theme_booktabs() %>%
  autofit() %>%
  save_as_docx("output_feols", path = "output_voll_feols.docx")




### vorhergesagte werte berechnen 

pfad2 <- "C:/Users/HP/Dropbox/6. Semester/Bachelorarbeit/"
source(paste0(pfad2,"/fixest_erweiterung.R"))


#Frauen
pred_df21_f <-  expand.grid("year" =seq(2,11,1),"online"=0:1)   # wichtig: hier "Kennenlernort_num"
relpred(mv_feols_f, newdata = pred_df21_f) # mit der numerischen Variable funktioniert es

pred_df_full_f <- bind_cols(pred_df21_f,relpred(mv_feols_f, newdata = pred_df21_f)) # bind_cols, um die eingesetzten Werte auch im datensatz zu haben
pred_df_full_f

67.57741-63.54873
29.99790-23.99832
#männer
pred_df21_m <-  expand.grid("year" =seq(2,11,1),"online"=0:1)   # wichtig: hier "Kennenlernort_num"
relpred(mv_feols_m, newdata = pred_df21_m) # mit der numerischen Variable funktioniert es

pred_df_full_m <- bind_cols(pred_df21_m,relpred(mv_feols_m, newdata = pred_df21_m)) # bind_cols, um die eingesetzten Werte auch im datensatz zu haben
pred_df_full_m
34.5-26.5
67.6-31.31
26.561460-27.453986 
34.594199-28.542270  

### Grapik

#weiblich
margin_f <- ggplot(data=pred_df_full_f,aes(x = year+ 2007 + ifelse(online==0,-.075,+.075), y= fit ,ymin=lwr, ymax=upr, color = factor(online))) + 
  geom_line() + 
  geom_errorbar() + 
  geom_point() +
  theme_minimal(base_size = 11, base_family = "Times") +
  scale_color_manual(values = c("black","grey"), breaks = 0:1, labels = c("Offline", "Online"))  +
  labs(y = "Vorhergesagte Werte", x = "Jahre", colour = "Kennenlernort")+
  theme(legend.position="bottom")
margin_f

ggsave("margin_f.png", width = 12, height = 12, units = "cm")



#männlich
margin_m <- ggplot(data=pred_df_full_m,aes(x = year+ 2007 + ifelse(online==0,-.075,+.075), y= fit ,ymin=lwr, ymax=upr, color = factor(online))) + 
  geom_line() + 
  geom_errorbar() + 
  geom_point() +
  theme_minimal(base_size = 11, base_family = "Times") +
  scale_color_manual(values = c("black","grey"), breaks = 0:1, labels = c("Offline", "Online"))  +
  labs(y = "Vorhergesagte Werte", x = "Jahre", colour = "Kennenlernort")+
  theme(legend.position="bottom")
margin_m

ggsave("margin_m.png", width = 12, height = 12, units = "cm")




##extraktion männer lm und feols
modelsummary(list("OLS-Regression Männer"=lm_m,"Fixed-Effect-Modell Männer"=mv_feols_m),
             output = "flextable", 
             gof_omit = "IC|F|L",stars = T,fmt ="%.3f") %>%
  set_caption("Regressionsergebnisse für das vollständige Modell") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>%
  theme_booktabs() %>%
  autofit() %>%
  save_as_docx("output_feols", path = "output_voll_männer.docx")


#extraktion frauen lm und feols

modelsummary(list("OLS-Regression Frauen"=lm_f,"Fixed-Effect-Modell Frauen"=mv_feols_f),
             output = "flextable", 
             gof_omit = "IC|F|L",stars = T,fmt ="%.3f") %>%
  set_caption("Regressionsergebnisse für das vollständige OLS-Modell") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>%
  theme_booktabs() %>%
  autofit() %>%
  save_as_docx("output_voll_lm", path = "output_voll_frauen.docx")
