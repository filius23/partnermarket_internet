# load packages ----------------------------------------------------------------

library(tidyverse)
library(haven)

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
              "bula", "sin6i3", "sin6i4", "sin6i5", "gkpol", "sdp4",
              "school", "doby_gen", "sex_gen", "age", "nkidsbio", "isced", 
              "marstat", "relstat", "meetdur", "reldur", "homosex",
              "pa3", "hcp1i1", "hcp1i2", "hcp3h", "hcp3m", "inc2")
    print(anch_file)
    read_dta(unz(zipfile, filename = anch_file)) %>%
      select(any_of(col1))

  })
  
saveRDS(df0, file = "./data/anchor_w1-11.RDS")
df1 <- readRDS(file = "./data/anchor_w1-11.RDS")



# create Variables -------------------------------------------------------------

## Info on ego------------------------------------------------------------------
### year of survey
df1$year <- as.numeric(df1$wave) + 2008
df1 %>% count(wave,year)
df1$year_fct <- factor(df1$year)

### year of birth and birth cohort ---------------------------------------------
df1 %>% count(doby_gen,cohort)

### age ------------------------------------------------------------------------
df1 %>% count(age)
df1$age_fct <- factor(df1$age)

### sex  -----------------------------------------------------------------------
df1 %>% count(sex_gen)
class(df1$sex_gen)
df1$female <- ifelse(df1$sex_gen == 2, 1, 0)
df1$female <- as.factor(df1$female)

### education: isced  ----------------------------------------------------------
df1 %>% count(isced)
df1$isced[df1$isced<0]<-NA
df1$isced_fct <- case_when(df1$isced >= 0 & df1$isced <= 3 ~ "low",     
                           df1$isced >= 4 & df1$isced <= 6 ~ "middle",
                           df1$isced >= 7 & df1$isced <= 8 ~ "high")
df1$isced_fct <- factor(df1$isced_fct, levels = c("low","middle","high"))

### migration background  ------------------------------------------------------
df1 %>% count(migstatus)

### municipality size: gkpol  --------------------------------------------------
df1 %>% count(gkpol)
df1$gkpol[df1$gkpol<0] <- NA
df1$gkpol_fct <- case_when(df1$gkpol >= 1 & df1$gkpol <= 3 ~ "small",     
                           df1$gkpol >= 4 & df1$gkpol <= 5 ~ "middle",
                           df1$gkpol >= 6 & df1$gkpol <= 7 ~ "big")
df1$gkpol_fct <- factor(df1$gkpol_fct, levels = c("small","middle","big"))




## Information on partner & relationship ---------------------------------------

### current partner lives in Germany: sdp4
df1 %>% count(sdp4)
df1$sdp4[df1$sdp4<0]<-NA
df1$p_abroad <- factor(ifelse(df1$sdp4 == 2, 1, 0))

### place of meeting online/offline: pa3 ---------------------------------------
df1 %>% count(pa3)
df1$pa3[df1$pa3 < 0] <- NA
df1$online <- case_match(df1$pa3,
                         c(7,12,13,14) ~ 1, 
                         c(1:6,8:11) ~ 0)
df1 %>% count(pa3,online)
df1$online <- factor(df1$online)


### place of residence Partner: hcp1i1
df1 %>% count(hcp1i1) 
df1$same_lp <- case_match(df1$hcp1i1, 1 ~ 0,7 ~ 1)
df1 %>% count(hcp1i1,same_lp)


### distance to partner in hours and minutes: hcp3h & hcp3m --------------------
df1 %>% count(hcp3h) 

df1$distance_h <- df1$hcp3h #hours
df1$distance_h[df1$distance_h < 0 ] <- NA
df1$distance_h <- as.numeric(df1$distance_h)
df1$distance_min <- df1$distance_h*60

df1 %>% count(hcp3m) #minutes
df1$distance_m <- df1$hcp3m
df1$distance_m[df1$distance_m < 0 ] <- NA
df1$distance_m <- as.numeric(df1$distance_m)

df1$distance_total <- df1$distance_min + df1$distance_m # merge variables
class(df1$distance_total)

df1$distance_total_960 <- ifelse(df1$distance_total > 960, NA, df1$distance_total)
table(df1$distance_total_960)





################################### new data ################################### 

saveRDS(df1, file = "./data/df1.RDS")

df2 <- df1 %>% select(id, wave, year, year_fct, cohort, age, female, isced, isced_fct, migstatus, gkpol_fct,
                      pa3, online, p_abroad, same_lp, hcp1i2,
                      distance_total, distance_total_960)
saveRDS(df2, file = "./data/df2.RDS")



# exclude wave 1
df3 <- df2[df2$wave!=1,]
saveRDS(df3, file = "./data/df3.RDS")

df4 <- df3 %>% select(id, wave, year, year_fct, cohort, age, female, isced_fct, migstatus, gkpol_fct,
                      pa3, online, p_abroad, same_lp, hcp1i2,
                      distance_total, distance_total_960) 
saveRDS(df4, file = "./data/df4.RDS")


# missing values removed (without missings for place of meeting & NAs)
df_finish <- df4[!is.na(df4$online),]
df_finish <- df_finish[!is.na(df_finish$distance_total_960),]
df_finish <- df_finish %>% rename(distance_finish = distance_total_960)
saveRDS(df_finish, file = "./data/df_finish.RDS")