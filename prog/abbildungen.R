# load packages ----------------------------------------------------------------
pacman::p_load("tidyverse", "ggplot2", "systemfonts", "RColorBrewer", "mosaic")

# load data --------------------------------------------------------------------
df_finish <- readRDS("./data/df_finish.RDS")

# plot settings ----------------------------------------------------------------
# font_fam1 <- "Open Sans" # r
font_fam1 <- "Times New Roman"

theme_set(
  theme_minimal(base_family = font_fam1,base_size = 12) + 
    theme(legend.position = "top",
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          legend.background = element_rect(fill = "grey98"),
          strip.text.x = element_text(angle = 0, size = rel(1.35)),
          strip.text.y = element_text(angle = 0, hjust = 0, size = rel(1.25)),
          rect = element_rect(fill = NA, linetype = 1, colour = NA),
          panel.grid.minor = element_blank(),panel.grid.major.x = element_blank(),
          panel.spacing.y = unit(.5,"lines"),
          axis.text = element_text(size = rel(1.1)),
          axis.ticks = element_line(color = "grey25"),
          axis.ticks.length=unit(.185,units = "lines"),
          axis.line = element_line(linewidth = .15)) 
)



# graphs --------------------------------------------------------------------

### graph 1: relative frequencies for the newly formed partnerships from online and offline meeting context
g1 <- df_finish %>% select(id,wave,online,distance_finish) %>% 
  rename(distance = distance_finish) %>%
  group_by(online,wave) %>% count() %>% 
  group_by(wave) %>% 
  mutate(sum = sum(n),
         p = n/sum) %>% 
  arrange(wave,online) 

ggplot(g1, aes(x=as.numeric(wave)+2007, y=p, linetype=factor(online))) +
  geom_line() +
  geom_point(size=2) +
  ylim(0, 1)+    
  scale_linetype_discrete(
    name = "meeting place",
    breaks=c("0", "1"),
    labels=c("offline","online")) +
  labs(title = "Relative frequency of the meeting place \n of the newly formed partnerships",
    caption =  "Source: pairfam, wave 2-12", 
    y = "relative frequency",
    x = "wave") +
  theme(legend.position="bottom",
        strip.text.y = element_text(angle = 0,size=rel(2)),
        strip.text.x = element_text(angle = 0,size=rel(2))) + expand_limits(y = c(0,.5)) 
graph1
#ggsave("graph1.png", width = 12, height = 12, units = "cm")


#Daten Tabelle g1 extrahieren Anhang : Relative und absolute Haeufigkeiten der neu geschlossenen Partnerschaften nach der Welle (Quelle: pairfam, Welle 2-11)
relH <- g1 %>% group_by(wave) %>% pivot_wider(names_from = online, values_from = c(p,n,sum)) %>% arrange(wave)

data.frame(relH) %>% flextable() %>% 
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>% 
  colformat_double(big.mark ="", digits = 1) %>% 
  autofit() 





### graph 1: relative frequencies for the newly formed partnerships from online and offline meeting context
# Legende nicht vorhanden
g2 <- df_finish %>% ungroup() %>% select(wave,online,distance_finish) %>% 
  rename(distance = distance_finish) %>% 
  filter(!is.na(online)) %>% filter(!is.na(distance)) %>% 
  group_by(wave, online)%>% #mittelwert nach kategorie berechnen
  mutate(distance_mean = mean(distance)) %>%
  mutate(distance_median = median(distance))

facet1 <- g2 %>% pivot_longer(cols = c("distance_mean","distance_median"))
facet2 <- facet1 %>% select(wave,online,name,value) %>% group_by(wave,name,online,value)%>% count() %>% ungroup()          # warum funktioniert gruppierung nicht?

facet2$distance_new <- case_when(facet2$name == "distance_mean" & facet2$online == "0" ~ "distance mean offline", 
                                 facet2$name == "distance_mean" & facet2$online == "1"~ "distance mean online", 
                                 facet2$name == "distance_median" & facet2$online == "0"~ "distance median offline",
                                 facet2$name == "distance_median" & facet2$online == "1" ~ "distance median online") 

facet2 %>% arrange(distance_new, online=="0") %>% print(n=30)


### offline/offline facets 
#vergl_distanz <- 
  
ggplot(facet2, aes(x=as.numeric(wave)+2007,y=value,shape=distance_new)) + #shape group und fill funktionieren nicht
  geom_line()+
  geom_point()+
  facet_grid( ~ online,scales = "free_y")+
  ylim(0,120)+
  theme_minimal(base_size = 11, base_family = "Times")+
  theme(legend.position="bottom") +
  labs(title = "Travelling time to the partner",
       caption =  "Quelle: pairfam Welle 2-13",
       y = "distance (minutes)",
       x = "wave", 
       shape="distance_new") +
  scale_shape_discrete(labels = c("distance mean offline", "distance mean online", "distance median offline", "distance median online"))
vergl_distanz
ggsave("vergl_distanz.png", width = 12, height = 12, units = "cm")


# die dazugehoerigen Werte als tabelle erstellen Fuer ANHANG!
Anhang2 <- facet2 %>% select(-n) %>% pivot_wider(names_from = wave, values_from = value) %>% 
  arrange(online,name) %>% select(-name,-online) %>% 
  rename(`wave 2` = `2`,
         `wave 3` = `3`,
         `wave 4` = `4`,
         `wave 5` = `5`,
         `wave 6` = `6`,
         `wave 7` = `7`,
         `wave 8` = `8`,
         `wave 9` = `9`,
         `wave 10` = `10`,
         `wave 11` = `11`) %>% 
  group_by(distance_new)

Anhang2_1 <- data.frame(Anhang2) %>% flextable() %>% 
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>% 
  colformat_double(decimal.mark=",",big.mark ="", digits = 1) %>% 
  autofit() #%>% 
  #save_as_docx("Anhang1", path = "Distanz_Tab_ueber_Wellen2.docx")





###### katg. Var: Vergleichen wir die Mittelwerte Distanz zwischen den Online&Offline: ANHANG
mean_dis_vergl <- df_finish %>%
  drop_na(distance_finish, online) %>%
  group_by(online) %>%
  summarise(mean_dis = mean(distance_finish)) 
mean_dis_vergl


mittelwertvergleich <- df_finish %>% 
  ggplot(aes(x = factor(online), y = distance_finish)) +
  geom_boxplot(width = .1) +
  geom_jitter(width = .1, alpha = .1) +
  ylim(0,500) +
  labs(title = "Mean value comparison of the meeting place",
       caption = "Source: pairfam, wave 2-11", 
       y = "Distance to partner (minutes)",
       x = "Meeting place") +
  theme_minimal(base_size = 11, base_family = "Times") +
  geom_point(data = mean_dis_vergl,
             aes(x=factor(online), y=mean_dis),
             color = "red",
             size = 5,
             shape = 19) +
  geom_line(data = mean_dis_vergl,
            aes(x=factor(online), y=mean_dis),
            group = 1,
            color = "red")

mittelwertvergleich
ggsave("mittelwertvergleich.png", width = 12, height = 12, units = "cm")
