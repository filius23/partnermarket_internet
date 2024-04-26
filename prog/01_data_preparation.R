# load packages ----------------------------------------------------------------

library(tidyverse)
library(haven)

# Variables --------------------------------------------------------------------

#Welle 1: sd3, sd4g, sd5ezbm, sd5ezby, sd5ezbm, sd5ezby, sd7ez, sd8exbm, sd8exby, sd10, hc1pxi1,
#          hc1pxi2, mig4, igr14
#Welle 11 (Refreshmeint): sd5ezbm, sd5ezby, sd8e1bm, sd8e1by, sd10, mig4
#Welle 2-11: ehc1pxg, ehc2px, ehc3px, ehc4px,igr14p1
#Welle 3-11: ehc27pxi2,
#Welle 1-11: job2, job7,pa2m, pa2y,pa3, sdp4, sdp5,sdp6,sdp11,hcp1i1, hcp1i2, hcp3h, hcp3m, pa9,hc13h1 inc

#Mobilitätsabsichten welle 7-11: hc29, hc30, hc31
#work-life-balance mit aufnehmwn? job23
#Pendeln Wohnung Arbeit: Welle 1,3,5 ab 7 jährlich: job16h, job16m
#strukturelle Merkmale des Partnermarktes (Welle 1-11): sin6i3,sin6i4,sin6i5
#Singles: Nutzung Internet/ sind 8

# load anchor data -------------------------------------------------------------

zipfile <- "./orig/ZA5678_v14-1_pairfam.zip"
anchor_files <-  
  unzip(zipfile, list = TRUE) %>% 
  filter(grepl("Stata/anchor\\d+(_DD|)\\.dta$",Name)) %>% 
  pull(Name)


df0 <- 
  map_dfr(anchor_files, function(anch_file){
    
    i <- str_extract(anch_file,"\\d+") %>% parse_number()
    col1 <- c("id", "wave", "cohort","migstatus",
              "bula", "sin6i3", "sin6i4", "sin6i5", "gkpol", 
              "school", "doby_gen", "sex_gen", "age", "nkidsbio", "isced", 
              "marstat", "relstat", "meetdur", "reldur", "homosex", "ethni", 
              "job2", "pa3", "sdp4", "sdp6", "sdp11", "hcp1i1", "hcp1i2", "hcp3h", 
              "hcp3m", "pa9", "inc2", "job19", "igr41p1", "sdp22", "job16h", 
              "job16m", "hc29")
    print(anch_file)
    read_dta(unz(zipfile, filename = anch_file)) %>%
      select(any_of(col1))

  })
  
saveRDS(df0, file = "./data/anchor_w1-11.RDS")
df1 <- readRDS(file = "./data/anchor_w1-11.RDS")

# create Variables -------------------------------------------------------------

## Info on ego------------------------------------------------------------------
# year
df1$year <- as.numeric(df1$wave) + 2008
df1 %>% count(wave,year)

### year of birth Geburtsjahr/Kohorte ------------------------------------------
df1 %>% count(doby_gen,cohort)


### Alter ----------------------------------------------------------------------
df1 %>% count(age)
df1$age_fct <- factor(df1$age)

### Geschlecht  ----------------------------------------------------------------
df1 %>% count(sex_gen)
class(df1$sex_gen)
df1$frau <- ifelse(df1$sex_gen == 2, 1, 0)
df1$frau <- as.factor(df1$frau)


### education: isced  ----------------------------------------------------------
df1 %>% count(isced)
df1$isced[df1$isced<0]<-NA
df1$isced_2 <- case_when(df1$isced >= 0 & df1$isced <= 3 ~ "niedrig",     
                         df1$isced >= 4 & df1$isced <= 6 ~ "mittel",
                         df1$isced >= 7 & df1$isced <= 8 ~ "hoch")
df1$isced_2 <- factor(df1$isced_2, levels = c("niedrig","mittel","hoch"))


### Migration background  ------------------------------------------------------
df1 %>% count(migstatus)# aus pairfam

### Bundesland  ----------------------------------------------------------------
table(df1$bula)
df1$age[df1$bula<0]<-NA
class(df1$bula)
df1$bula <- as.factor(df1$bula)


### Gemeindegröße (gkpol)  -----------------------------------------------------
df1$gkpol[df1$gkpol<0] <- NA
df1$gkpol_kat1 <- case_when(df1$gkpol >= 1 & df1$gkpol <= 3 ~ "Kleinstadt",     
                            df1$gkpol >= 4 & df1$gkpol <= 5 ~ "Stadt",
                            df1$gkpol >= 6 & df1$gkpol <= 7 ~ "Grossstadt")
df1$gkpol_kat1 <- factor(df1$gkpol_kat1, levels = c("Kleinstadt","Stadt","Grossstadt"))





## Info on partner & relationship ----------------------------------------------

### Lebt aktueller Partner in DE (sdp4)
table(df1$sdp4) # 75 Partner der Befragten leben im Ausland
df1$sdp4[df1$sdp4<0]<-NA
df1$p_wohnort <- factor(ifelse(df1$sdp4 == 2, 1, 0))


### Online/Offline Variable (based on pa3)---------------
table(df1$pa3)
df1$pa3[df1$pa3 < 0] <- NA

df1$online <- ifelse(df1$pa3==7|df1$pa3==12|df1$pa3==13|df1$pa3==14, 1, 0) %>% factor()
df1$online2 <- case_match(df1$pa3,
                          c(7,12,13,14) ~ 1, 
                          c(1:6,8:11) ~ 0)

# df1$offline <- factor(ifelse(df1$pa3=="1"|df1$pa3=="2"|df1$pa3=="3"|df1$pa3=="4"|df1$pa3=="5"|df1$pa3=="6"|df1$pa3=="8"|df1$pa3=="9"|df1$pa3=="10"|df1$pa3=="11", 0, df1$pa3))

df1 %>% count(pa3,online,online2)

### Wohnort Partner (hcp1i1)
df1 %>% count(hcp1i1) #Kategorie 7: wohnt im selben Wohnort wie ich (n=1088)
df1$wohnort_gleich <- case_match(df1$hcp1i1, 1 ~0,7 ~ 1)
df1 %>% count(hcp1i1,wohnort_gleich)


### Bundesland in dem Partner wohnt (hcp1i2)
table(df1$hcp1i2)
df1$hcp1i2[df1$hcp1i2<0] <-NA
df1$hcp1i2 <- as.factor(df1$hcp1i2)


### Distanz zum Partner in Min und Std (hcp3h & hcp3m)
table(df1$hcp3h) #Anmerkung: NA: -1 bis -5

df1$distanz_h <- df1$hcp3h
df1$distanz_h[df1$distanz_h < 0 ] <- NA
df1$distanz_h <- as.numeric(df1$distanz_h)
df1$distanz_min <- df1$distanz_h*60 #Variable zum weiterarbeiten

table(df1$hcp3m)
df1$distanz_m <- df1$hcp3m
df1$distanz_m[df1$distanz_m < 0 ] <- NA
df1$distanz_m <- as.numeric(df1$distanz_m)

# Variablen zu einer zusammenfügen (Addition)
df1$distanz_gesamt <- df1$distanz_min + df1$distanz_m
class(df1$distanz_gesamt)

df1$distanz_gesamt_a <- ifelse(df1$distanz_gesamt > 960, NA, df1$distanz_gesamt) # @Andreas: in der Arbeit habe ich die Ausreißer ausgeschlossen (orientiert habe ich mich an der längsten Strecke in Deutschland, es wurden nur Paare in DE untersucht), für Publikation wahrscheinlich etwas anders rangehen?
table(df1$distanz_gesamt_a)


### Fahrzeit binär unter/über 60 Minuten zum Partner (Kurz und Fernbeziehung)
df1$distance_under60 <- ifelse(df1$distanz_gesamt <= 60, "1", "0")
table(df1$distance_under60)





#################################################

saveRDS(df1, file = "./data/df1.RDS")

df2 <- df1 %>% select(id, wave, year, cohort, age, frau, isced, isced_2, migstatus, bula, gkpol_kat1,
                      pa3, online, offline, 
                      p_wohnort, wohnort_gleich, hcp1i2,
                      distanz_gesamt, distanz_gesamt_a, distance_under60)
saveRDS(df2, file = "./data/df2.RDS")


 


#ohne Welle 1
df3 <- df2[df2$wave!=1,]
saveRDS(df3, file = "./data/df3.RDS")

df4 <- df3 %>% select(id, wave, year, cohort, age, frau, isced_2, migstatus, bula, gkpol_kat1,
                      pa3, online, offline, p_wohnort, wohnort_gleich, hcp1i2,
                      distanz_gesamt, distanz_gesamt_a, distance_under60) 
saveRDS(df4, file = "./data/df4.RDS")


#fehlende Werte raus #ohne missings bei Kennenlernort & NA

df_finish <- df4[!is.na(df4$online),]
df_finish <- df_finish[!is.na(df_finish$distanz_gesamt_a),]
df_finish <- df_finish %>% rename(distanz_finish = distanz_gesamt_a)

saveRDS(df_finish, file = "./data/df_finish.RDS")
