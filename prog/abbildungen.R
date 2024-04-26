####################  packages  #################### 

library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(mosaic)


####################  files  #################### 

setwd("C:/Users/HP/Documents/Datens?tze/pairfam_Welle11/pairfam Welle11/Data/Stata")
list.files()
df_finish <- readRDS("df_finish.RDS")

windowsFonts(Times=windowsFont("Times New Roman")) #für schriftart




#################### graphs ####################

########## graph 1: relative frequencies for the newly formed partnerships from online and offline meeting context
g1 <- df_finish %>% select(id,wave,online,distanz_finish) %>% 
  rename(distance = distanz_finish) %>%
  group_by(online,wave) %>% count() %>% 
  group_by(wave) %>% 
  mutate(sum = sum(n),
         p = n/sum) %>% 
  arrange(wave,online) 

abb1 <- ggplot(g1, aes(x=as.numeric(wave)+2007, y=p, colour=online)) +
  geom_line() +
  geom_point(size=2) +
  ylim(0, 1)+    
  theme_minimal(base_size = 11, base_family = "Times")+
  labs(title = "Relative frequency of new relationships initiated online and offline",
    caption =  "Quelle: pairfam Welle 2-12", 
    y = "relative frequency",
    x = "wave",
    colour = "meeting context") +
  theme(legend.position="bottom") + expand_limits(y = c(0,.5))+
  scale_colour_grey(start = 0.7, end = 0)
abb1
ggsave("abb1.png", width = 12, height = 12, units = "cm")


#Daten Tabelle g1 extrahieren Anhang : Relative und absolute Häufigkeiten der neu geschlossenen Partnerschaften nach der Welle (Quelle: pairfam, Welle 2-11)
relH <- g1 %>% group_by(wave) %>% pivot_wider(names_from = online, values_from = c(p,n,sum)) %>% arrange(wave)

data.frame(relH) %>% flextable() %>% 
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>% 
  colformat_double(decimal.mark=",",big.mark ="", digits = 1) %>% 
  autofit() 





########## graph 2:  für graph: Fahrzeiten zum Partner im Online-/Offline-Vergleich
# Legende nicht vorhanden
g2 <- df_finish %>% ungroup() %>% select(wave,online,distanz_finish) %>% 
  rename(distance = distanz_finish) %>% 
  filter(!is.na(online)) %>% filter(!is.na(distance)) %>% 
  group_by(wave, online)%>% #mittelwert nach kategorie berechnen
  mutate(distance_mean = mean(distance)) %>%
  mutate(distance_median = median(distance))

facet1 <- g2 %>% pivot_longer(cols = c("distance_mean","distance_median"))
facet2 <- facet1 %>% select(wave,online,name,value) %>% group_by(wave,name,online,value)%>% count() %>% ungroup()          # warum funktioniert gruppierung nicht?

facet2$distance_new <- case_when(facet2$name == "distance_mean" & facet2$online == "offline" ~ "distance mean offline", 
                                 facet2$name == "distance_mean" & facet2$online == "online"~ "distance mean online", 
                                 facet2$name == "distance_median" & facet2$online == "offline"~ "distance median offline",
                                 facet2$name == "distance_median" & facet2$online == "online" ~ "distance median online")


### offline/offline facets 
vergl_distanz <- ggplot(facet2, aes(x=as.numeric(wave)+2007,y=value,shape=distance_new)) + #shape group und fill funktionieren nicht
  geom_line()+
  geom_point()+
  facet_grid( ~ online,scales = "free_y",)+
  ylim(0,120)+
  theme_minimal(base_size = 11, base_family = "Times")+
  theme(legend.position="bottom") +
  labs(title = "Travelling time to the partner",
       caption =  "Quelle: pairfam Welle 2-13",
       y = "distance (minutes)",
       x = "wave", 
       group="distance_new")+
  scale_shape_discrete(limits = c("Distanz Mean Offline", "Distanz Median Offline", "Distanz Mean Online", "Distanz Median Online"),
                       labels = c("Mittelwert","Median","Mittelwert","Median"))
vergl_distanz
ggsave("vergl_distanz.png", width = 12, height = 12, units = "cm")


# die dazugehörigen Werte als tabelle erstellen FÜR ANHANG!
Anhang2 <- facet2 %>% select(-n) %>% pivot_wider(names_from = wave, values_from = value) %>% 
  arrange(online,name) %>% select(-name,-online) %>% 
  rename(`Welle 2` = `2`,
         `Welle 3` = `3`,
         `Welle 4` = `4`,
         `Welle 5` = `5`,
         `Welle 6` = `6`,
         `Welle 7` = `7`,
         `Welle 8` = `8`,
         `Welle 9` = `9`,
         `Welle 10` = `10`,
         `Welle 11` = `11`) %>% # , `Welle 12` = `12`
  group_by(distance_new)

Anhang2_1 <- data.frame(Anhang2) %>% flextable() %>% 
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>% 
  colformat_double(decimal.mark=",",big.mark ="", digits = 1) %>% 
  autofit() %>% 
  save_as_docx("Anhang1", path = "Distanz_Tab_über_Wellen2.docx")





###### katg. Var: Vergleichen wir die Mittelwerte Distanz zwischen den Online&Offline: ANHANG
mean_dis_vergl <- df_finish %>%
  drop_na(distanz_finish, online) %>%
  group_by(online) %>%
  summarise(mean_dis = mean(distanz_finish)) 
mean_dis_vergl


mittelwertvergleich <- df_finish %>% 
  ggplot() +
  aes(x = online, y = distanz_finish) +
  geom_boxplot(width = .1) +
  geom_jitter(width = .1, alpha = .1) +
  theme_minimal(base_size = 11, base_family = "Times") +
  ylim(0,600) +
  labs(title = "Mean value comparison of the meeting locations",
       #caption =  "Quelle: pairfam Welle 2-11", 
       y = "Distance to partner (minutes)") +
  theme_minimal(base_size = 11, base_family = "Times") +
  geom_point(data = mean_dis_vergl,
             aes(x=online, y=mean_dis),
             color = "red",
             size = 5,
             shape = 19) +
  geom_line(data = mean_dis_vergl,
            aes(x=online, y=mean_dis),
            group = 1,
            color = "red")

mittelwertvergleich
ggsave("mittelwertvergleich.png", width = 12, height = 12, units = "cm")
