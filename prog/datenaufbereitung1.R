####################  PAKETE  #################### 

pacman::p_load("readr", "tidyverse", "readstata13", "data.table")

setwd("C:/Users/HP/Documents/Datensätze/pairfam_Welle11/pairfam Welle11/Data/Stata")
list.files()
#rm(list=ls())

#################### Variablenübersicht   #################### 

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

####################  Einlesen Pairfam Anchor  #################### 

i <- 1
while (i<=11){
  
  if (i >= 1) col1 <- c("id","wave","bula","sin6i3","sin6i4","sin6i5","gkpol","school","doby_gen","sex_gen","age","nkidsbio","isced","marstat","relstat","meetdur","reldur","homosex","ethni","job2","pa3", "sdp4", "sdp6","sdp11","hcp1i1","hcp1i2","hcp3h","hcp3m","pa9","inc2")
  if (i > 1) col1 <- c(col1,"job19","igr41p1")
  if (i > 2) col1 <- c(col1,"sdp22")
  if (i == 3) col1 <- c(col1,"job16h","job16m")
  if (i == 1) col1 <- c(col1,"job16h","job16m")
  if (i == 5) col1 <- c(col1, "job16h","job16m")
  if (i >= 7) col1 <- c(col1,"job16h","job16m","hc29")
  
  temp <- read.dta13(paste0("A", i, ".dta"), select.cols = col1, convert.factors = F) 
  setDT(temp)
  assign(paste0("A",i),temp) 
  rm(temp)
  i <- i + 1
}

for (i in 1:11){
  temp <- get(paste0("A",i))   
  namtemp <- colnames(temp)   
  namtemp <- paste0(namtemp,"_a",i)
  namtemp[namtemp==paste0("id_a",i)] <- "id" 
  colnames(temp) <- namtemp 
  assign(paste0("A",i),temp)
  rm(temp)
}

temp <- copy(A1)
list <- c(2,3,4,5,6,7,8,9,10,11)
for (i in list){
  temp <- merge(temp,get(paste0("A",i)),by="id",all.x=FALSE)
}

long <- melt(temp,
             id.vars = "id",
             measure.vars = patterns("bula","sin6i3","sin6i4","sin6i5","gkpol","school","sex_gen","doby_gen","age","nkidsbio","isced","marstat","relstat","meetdur","reldur","homosex","ethni","job2","job19","pa3","sdp4", "sdp6","sdp11","inc2","hcp1i1", "hcp1i2", "hcp3h", "hcp3m","igr41p1","pa9","job16h","job16m","hc29"),  
             variable.name="wave",
             value.name=c("bula","sin6i3","sin6i4","sin6i5","gkpol","school","sex_gen","doby_gen","age","nkidsbio","isced","marstat","relstat","meetdur","reldur","homosex","ethni","job2","job19","pa3","sdp4", "sdp6","sdp11","inc2","hcp1i1", "hcp1i2", "hcp3h", "hcp3m","igr41p1","pa9","job16h","job16m","hc29")) 


setkey(long,id,wave)

##### mit df1 wird weitergearbeitet
df1 <- long 






############################################### Datenaufbereitung -----------

################# Items zur befragten Person

### Welle
df1$year <- as.numeric(df1$wave)


#### Geburtsjahr/Kohorte
df1$cohort <- as.factor(floor(df1$doby_gen/10)*10)
table(df1$cohort)


###Alter
table(df1$age,useNA = "always")
df1$age <- as.numeric(df1$age)


### Geschlecht
table(df1$sex_gen, useNA = "always")
class(df1$sex_gen)
df1$frau <- ifelse(df1$sex_gen == 2, 1, 0)
df1$frau <- as.factor(df1$frau)


### Bildungsniveau: isced
df1$isced[df1$isced<0]<-NA
df1$isced_2 <- case_when(df1$isced >= 0 & df1$isced <= 3 ~ "niedrig",     
                         df1$isced >= 4 & df1$isced <= 6 ~ "mittel",
                         df1$isced >= 7 & df1$isced <= 8 ~ "hoch")
df1$isced_2 <- factor(df1$isced_2, levels = c("niedrig","mittel","hoch"))


### Migrationshintergrund
df1$ethni[df1$ethni<0] <- NA
df1$migrationshintergrund <- factor(ifelse(df1$ethni == 1, 0, 1))


### Bundesland
table(df1$bula)
df1$age[df1$bula<0]<-NA
class(df1$bula)
df1$bula <- as.factor(df1$bula)


### Gemeindegröße (gkpol)
df1$gkpol[df1$gkpol<0] <- NA
df1$gkpol_kat1 <- case_when(df1$gkpol >= 1 & df1$gkpol <= 3 ~ "Kleinstadt",     
                            df1$gkpol >= 4 & df1$gkpol <= 5 ~ "Stadt",
                            df1$gkpol >= 6 & df1$gkpol <= 7 ~ "Grossstadt")
df1$gkpol_kat1 <- factor(df1$gkpol_kat1, levels = c("Kleinstadt","Stadt","Grossstadt"))





################# Items zum Partner / über die Partnerschaft

### Lebt aktueller Partner in DE (sdp4)
table(df1$sdp4) # 75 Partner der Befragten leben im Ausland
df1$sdp4[df1$sdp4<0]<-NA
df1$p_wohnort <- factor(ifelse(df1$sdp4 == 2, 1, 0))


### Kennenlernort (online und offline) (pa3) 
table(df1$pa3)
df1$pa3[df1$pa3 < 0] <- NA
df1$pa3 <- as.factor(df1$pa3)

# Online/Offline Variable
df1$online <- factor(ifelse(df1$pa3=="7"|df1$pa3=="12"|df1$pa3=="13"|df1$pa3=="14", 1, 0))
df1$offline <- factor(ifelse(df1$pa3=="1"|df1$pa3=="2"|df1$pa3=="3"|df1$pa3=="4"|df1$pa3=="5"|df1$pa3=="6"|df1$pa3=="8"|df1$pa3=="9"|df1$pa3=="10"|df1$pa3=="11", 0, df1$pa3))


### Wohnort Partner (hcp1i1)
table(df1$hcp1i1) #Kategorie 7: wohnt im selben Wohnort wie ich (n=1088)
class(df1$hcp1i1)
df1$hcp1i1[df1$hcp1i1<0] <-NA
df1$wohnort_gleich <- factor(df1$hcp1i1, levels = c(1,7), labels = c("0","1"))
table(df1$wohnort_gleich)


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

saveRDS(df1, file = "df1.RDS")

df2 <- df1 %>% select(id, wave, year, cohort, age, frau, isced, isced_2, migrationshintergrund, bula, gkpol_kat1,
                      pa3, online, offline, 
                      p_wohnort, wohnort_gleich, hcp1i2,
                      distanz_gesamt, distanz_gesamt_a, distance_under60)
saveRDS(df2, file = "df2.RDS")


 


#ohne Welle 1
df3 <- df2[df2$wave!="1",]
saveRDS(df3, file = "df3.RDS")

df4 <- df3 %>% select(id, wave, year, cohort, age, frau, isced_2, migrationshintergrund, bula, gkpol_kat1,
                      pa3, online, offline, p_wohnort, wohnort_gleich, hcp1i2,
                      distanz_gesamt, distanz_gesamt_a, distance_under60) 
saveRDS(df4, file = "df4.RDS")


#fehlende Werte raus #ohne missings bei Kennenlernort & NA

df_finish <- df4[!is.na(df4$online),]
df_finish <- df_finish[!is.na(df_finish$distanz_gesamt_a),]
df_finish <- df_finish %>% rename(distanz_finish = distanz_gesamt_a)

saveRDS(df_finish, file = "df_finish.RDS")
