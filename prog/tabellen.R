####################  packages  ####################  

pacman::p_load("tidyverse", "texreg", "psych", "ggplot2", "modelsummary", "flextable", "officer") #"ggfortify"

library(data.table)


####################  files/data  #################### 

setwd("C:/Users/HP/Documents/Datensätze/pairfam_Welle11/pairfam Welle11/Data/Stata")
list.files()

df_finish <- readRDS("df_finish.RDS")



########## t-test für bivariate Unterschiede ########## 

#Sind Online Beziehungen mit einer größeren Entfernung assoziert als Offline Beziehungen?
#  H0: Es gibt kein Unterschied in den mittleren Distanzen zwischen Online und Offline
#  H1: Die mittlere Distanz ist über die Zeit Online größer als Offline

#offline < online (alphabetische Sortierung)

ttest <- t.test(distanz_finish~online, data=df_finish,
                alternative="two.sided")
ttest


library(broom)
Tidy <- broom::tidy(ttest)
Tidy

class(Tidy)

Tidy1 <- data.frame(Tidy) %>% flextable() %>% 
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>% 
  colformat_double(decimal.mark=",",big.mark = "", digits = 1) %>% 
  autofit()










################# table ################# 

########################## table 1: Sample description
# metric vars: year, age, destanz_finish
metrische_uV <- df_finish %>%
  select(distance = distanz_finish, year, age) 

desc <- psych::describe(metrische_uV,skew = F, omit = T) %>% 
  select(Mean=mean,SD=sd,Min=min,Max=max)

desc$vars <- row.names(desc) # Variablen aus Zeilenname

tab1_metrisch <- data.frame(desc) %>%  
  select(vars,Mean,SD,Min,Max) %>% 
  mutate(Mean = round(Mean, 2), 
         SD = round(SD, 2)) %>%
  flextable() %>%  
  autofit() 

#categorial vars: online, cohort, frau,isced2, migrationshintergrund, gkpol_kat1
table(df_finish$migrationshintergrund)


df_finish$frau <- factor(df_finish$frau, levels = 0:1, 
                             labels = c("men","women"))
df_finish$migrationshintergrund <- factor(df_finish$migrationshintergrund, 
                                  levels = 0:1, 
                                  labels = c("kein Hinter","migration background"))
df_finish$online <- factor(df_finish$online, levels = 0:1, 
                         labels = c("offline","online"))


cat1 <- round(prop.table(table(df_finish$frau)),2) %>% 
  data.frame(.) %>% filter(Var1=="women")
cat2 <- round(prop.table(table(df_finish$migrationshintergrund)),2) %>% 
  data.frame(.) %>% filter(Var1=="migration background")
cat3 <- round(prop.table(table(df_finish$online)),2) %>% 
  data.frame(.) %>% filter(Var1=="online")

cat4 <- round(prop.table(table(df_finish$cohort)),2) %>% 
  data.frame(.)
cat5 <- round(prop.table(table(df_finish$isced_2)),2) %>% 
  data.frame(.)
cat6 <- round(prop.table(table(df_finish$gkpol_kat1)),2) %>% 
  data.frame(.)


tab1_cat <- flextable(bind_rows(cat3,cat1,cat2,cat4,cat5,cat6)) %>% 
  prepend_chunks(part="body",j = "Var1", i = 4, 
                 value = as_paragraph(as_i("Year \n "))) %>% #@Andreas, vllt hast du/gibt es dafür eine bessere Lösung als diesen Code?
  prepend_chunks(part="body",j = "Var1", i = 7, 
                 value = as_paragraph(as_i("Education \n"))) %>% 
  prepend_chunks(part="body",j = "Var1", i = 10, 
                 value = as_paragraph(as_i("City size \n"))) %>% 
  autofit() 



### To do: die beiden Tabellen müssen zu einer zusammengefügt werden



#---------------------


########################## table 2: temporal distances for the online and offline encounter context
distance2 <- describeBy(df_finish$distanz_finish, df_finish$online,mat = T)
distance2
data.frame(distance2) %>% select(-kurtosis,-mad,-skew,-trimmed,-min,-range,-vars,-item,-max, -se) %>% flextable() %>% 
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>% 
  colformat_double(decimal.mark=",",big.mark = "", digits = 1,j=-2) %>% 
  colformat_double(decimal.mark=",",big.mark = "", digits = 0,j=2) %>% 
  autofit()


########################## table 3: temporal distances for the size of the place of residence
distance3 <- describeBy(df_finish$distanz_finish, df_finish$gkpol_kat1,mat = T)
distance3
data.frame(distance3) %>% select(-n,-kurtosis,-mad,-skew,-trimmed,-min,-range,-vars,-item,-max,-se) %>% flextable() %>% 
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>% 
  colformat_double(decimal.mark=",",big.mark = "", digits = 1,j=-2) %>% 
  colformat_double(decimal.mark=",",big.mark = "", digits = 0,j=2) %>% 
  autofit()


########################## table 4: temporal distances for the education level
distance4 <- describeBy(df_finish$distanz_finish, df_finish$isced_2,mat = T)
distance4
data.frame(distance4) %>% select(-n,-kurtosis,-mad,-skew,-trimmed,-min,-range,-vars,-item,-max,-se) %>% rename(Education = group1) %>% 
  flextable() %>% 
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>% 
  colformat_double(decimal.mark=",",big.mark = "", digits = 1,j=-2) %>% 
  colformat_double(decimal.mark=",",big.mark = "", digits = 0,j=2) %>% 
  autofit()