####################  PAKETE  ####################  

pacman::p_load("tidyverse", "texreg", "margins", "ggeffects", "modelsummary", "flextable", "plm") #"ggfortify"





#######################  FILE/DATENSATZ FÜR DIE ANALYSE  ####################### 

setwd("C:/Users/HP/Documents/Datensätze/pairfam_Welle11/pairfam Welle11/Data/Stata")
list.files()

df_finish <- readRDS("df_finish.RDS")





########################### Modell vollständig Datenaufbereitung ########################### 

### Datenaufbereitung  
# @Andreas: weißt du, weshalb wir das damals so für diese Variablen gemacht haben und für die Variable Cohort nicht?
# Frau @Andreas: ist das hier unbedingt noch notwendig? nun besteht die Variable Frau aus 0/1
table(df_finish$frau)

as.data.frame(model.matrix(~ df_finish$frau))
as.data.frame(model.matrix(~ df_finish$frau)) %>% rename_with(.,~gsub("df_finish\\$","",.x),everything()) # umbenennen

frau_dummys <-  as.data.frame(model.matrix(~ df_finish$frau)) %>% rename_with(.,~gsub("df_finish\\$","",.x),everything()) %>% select(-`(Intercept)`)
frau_dummys

df_f2 <- bind_cols(df_finish,frau_dummys)

#gkpol
table(df_finish$gkpol_kat1)

gkpol_dummys <-  as.data.frame(model.matrix(~ df_f2$gkpol_kat1)) %>% rename_with(.,~gsub("df_f2\\$","",.x),everything()) %>% select(-`(Intercept)`)
gkpol_dummys

df_f3 <- bind_cols(df_f2,gkpol_dummys)


table(df_f3$gkpol_kat1Grossstadt)
table(df_f3$gkpol_kat1Stadt)


#isced2
table(df_f3$isced_2)

isced_dummys <-  as.data.frame(model.matrix(~ df_f3$isced_2)) %>% rename_with(.,~gsub("df_f3\\$","",.x),everything()) %>% select(-`(Intercept)`)
isced_dummys

df_f4 <- bind_cols(df_f3,isced_dummys)



### Frauen und Männer Datensatz

sample_f2 <- df_f4[df_f4$frau==1,]
sample_m2 <- df_f4[df_f4$frau==0,]





###Häufigkeiten

## Frauen

# ansehen, für wieviele es eine Online und eine Offline Kennenlernen gibt:
sample_f2 %>% group_by(id) %>% mutate(on_off = online == lag(online)) %>% xtabs(~on_off,.,addNA = T)
# zusätzlich n als Info, für wieviele es überhaupt mehrere Paarbildungen gibt:
sample_f2 %>% group_by(id) %>% mutate(on_off = online == lag(online), n = 1:n()) %>% xtabs(~on_off+n,.,addNA = T)


## Männer
# ansehen, für wieviele es eine Online und eine Offline Kennenlernen gibt:
sample_m2 %>% group_by(id) %>% mutate(on_off = online == lag(online)) %>% xtabs(~on_off,.,addNA = T)
# zusätzlich n als Info, für wieviele es überhaupt mehrere Paarbildungen gibt:
sample_m2 %>% group_by(id) %>% mutate(on_off = online == lag(online), n = 1:n()) %>% xtabs(~on_off+n,.,addNA = T)




############# Modelle lm vollständig ############## 


### Modelle berechnen

# Frauen
lm_f <- lm(distanz_finish ~ online*year+gkpol_kat1Stadt+gkpol_kat1Grossstadt+isced_2mittel+isced_2hoch+factor(cohort),data = sample_f2) #siehe margins 2
screenreg(lm_f)

# Männer
lm_m <- lm(distanz_finish ~ online*year+gkpol_kat1Stadt+gkpol_kat1Grossstadt+isced_2mittel+isced_2hoch+factor(cohort),data = sample_m2) #siehe margins 2

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

### Modelle berechnen

# Frauen
fixed_full_f <- plm(distanz_finish ~ online*year+gkpol_kat1Stadt+gkpol_kat1Grossstadt+isced_2mittel+isced_2hoch, data = sample_f2, index = "id", model="random")

# Männer
fixed_full_m <- plm(distanz_finish ~ online*year+gkpol_kat1Stadt+gkpol_kat1Grossstadt+isced_2mittel+isced_2hoch, data = sample_m2, index = "id", model="random")

texreg::screenreg(list(fixed_full_f,fixed_full_m))



### Extraktion der Regressionsergebnisse feols
modelsummary(list("Fixed-Effect-Modell Frauen"=fixed_full_f,"Fixed-Effect-Modell Männer"=fixed_full_m),
             output = "flextable", 
             gof_omit = "IC|F|L",stars = T,fmt ="%.3f") %>%
  set_caption("Regressionsergebnisse für das vollständige Modell") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>%
  theme_booktabs() %>%
  autofit() %>%
  save_as_docx("output_feols", path = "output_voll_feols.docx")



### vorhergesagte Werte berechnen 

# Frauen
pred_fe_f2 <- ggeffect(fixed_full_f, terms = c("online[0,1]","year[2,3,4,5,6,7,8,9,10,11]"))
head(data.frame(pred_fe_f2))
class(pred_fe_f2$group)
pred_fe_f2$group <- as.numeric(as.character(pred_fe_f2$group))
class(pred_fe_f2$group)

# Männer
pred_fe_m2 <- ggeffect(fixed_full_m, terms = c("online[0,1]","year[2,3,4,5,6,7,8,9,10,11]"))
head(data.frame(pred_fe_m2))
class(pred_fe_m2$group)
pred_fe_m2$group <- as.numeric(as.character(pred_fe_m2$group))
class(pred_fe_m2$group)



### Grapik

#weiblich
margin_f <- ggplot(pred_fe_f2,aes(x = group+ 2007 + ifelse(x==0,-.075,+.075), y= predicted ,ymin=conf.low, ymax=conf.high, color = factor(x))) + 
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




########## Extraktion männer lm und fixed
modelsummary(list("OLS-Regression Männer"=lm_m,"Fixed-Effect-Modell Männer"=fixed_full_m),
             output = "flextable", 
             gof_omit = "IC|F|L",stars = T,fmt ="%.3f") %>%
  set_caption("Regressionsergebnisse für das vollständige Modell") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>%
  theme_booktabs() %>%
  autofit() %>%
  save_as_docx("output_feols", path = "output_voll_männer.docx")


########## Extraktion frauen lm und fixed
modelsummary(list("OLS-Regression Frauen"=lm_f,"Fixed-Effect-Modell Frauen"=fixed_full_f),
             output = "flextable", 
             gof_omit = "IC|F|L",stars = T,fmt ="%.3f") %>%
  set_caption("Regressionsergebnisse für das vollständige OLS-Modell") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>%
  theme_booktabs() %>%
  autofit() %>%
  save_as_docx("output_voll_lm", path = "output_voll_frauen.docx")