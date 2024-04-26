####################  PAKETE  ####################  

pacman::p_load("tidyverse", "texreg", "margins", "ggeffects", "modelsummary", "flextable", "plm") #"ggfortify"





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



### vorhergesagte Werte und Abbildung

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



### Modelle mit Interaktion

# Frauen Interaktionseffekt nm_ia_f
pred_ia_f <- ggeffect(nm_ia_f, terms=c("online[0,1]","year[2,3,4,5,6,7,8,9,10,11]"))
head(data.frame(pred_ia_f))
class(pred_ia_f$group)
pred_ia_f$group <- as.numeric(as.character(pred_ia_f$group))
class(pred_ia_f$group)

ia_f <- ggplot(pred_ia_f, aes(x = group +2007 + ifelse(x==0,-.075,+.075), y = predicted, color = factor(x), group = x)) +
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


# Männer
ggeffect(model = nm_ia_m, terms = c("online[0,1]"))
pred_ia_m <- ggeffect(model = nm_ia_m, terms = c("online[0,1]","year[2,3,4,5,6,7,8,9,10,11]"))
head(data.frame(pred_ia_m))
class(pred_ia_m$group)
pred_ia_m$group <- as.numeric(as.character(pred_ia_m$group))
class(pred_ia_m$group)

ia_m <- ggplot(pred_ia_m, aes(x = group +2007 + ifelse(x==0,-.075,+.075), y = predicted, color = factor(x), group = x)) +
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