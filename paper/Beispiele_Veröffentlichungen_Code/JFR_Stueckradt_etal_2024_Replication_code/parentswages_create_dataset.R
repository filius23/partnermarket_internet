#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# > [Infra] ParentsWages
#
# > Creates parent_time dataset
#
#
# Input:
# - GBAPERSOON2020TABV3.fst
# - HOOGSTEOPL2020TABV1.fst
# - GBAHUISHOUDENS2020BUSV1.fst
# - KINDOUDER2020TABV2.fst
# - 140909 ADOPTIEKINDEREN 1995-2012V1.SAV
# - SECMBUS2021V1.fst 
# - GBAVERBINTENISPARTNER2020BUSV1.fst
# - POLISBUSYYYYV2.fst (2006-2009)
# - SPOLISBUSYYYYVV.fst (2010-2016, always latest version available)
# - PERSOONINKYYYYTABVV.fst (2006-2010, always latest version available)
# - INPAYYYYTABVV.fst (2011-2016, always latest version available)
# - BETABYYYY.sav (2006-2016, always latest version available)
#
#   (Note: for more efficient loading, most of the original datasets were 
#    previously converted from .sav to .fst, using haven::read_spss and 
#    fst::write_fst)
#
#
# Output:
# parentswages/data/parent_time.fst: 
#    person-time file for all parents of selected couples (see section 3),
#    thus before individual-level selections
#    
#  
# Last modified: 2023-12-06 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# 0 - Setup ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

library(fst)
library(tidyverse)
library(haven)
library(janitor)
library(lubridate)
library(data.table)
library(dtplyr)
library(skimr)
library(labelled)
options(scipen=999)



# Faster %in% operator, using fastmatch package
`%fin%` <- function(x, table){
  fastmatch::fmatch(x, table, nomatch = 0L) > 0L
}



# *Much* faster alternative to lubridate::intersect, that might also
# be used to replace lubridate::int_overlaps 
# It gets the start and end dates
# of two intervals (s1 = starts of interval 1, e1 = end of int 1)
# and returns the number of days contained in their intersection,
# or zero if they don't intersect. If logical=TRUE, it returns
# a logical factor indicating if there is  overlap.
# Source of algo: scicomp.stackexchange.com/a/26260
intersect_days <- function(s1, e1, s2, e2, logical = FALSE) {
  
  days <- as.numeric((pmin(e1,e2) - pmax(s1,s2)) + 1)
  
  days <- if_else(s1 > e2 | s2 > e1 ,
                  0,
                  days)
  
  if (logical) {
    days > 0
  }
  else {
    days
  }
  
}


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# 1. Load data ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ...Persons ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
persoon <- read_fst("data/processed/GBAPERSOON2020TABV3.fst",
                    columns = c('RINPERSOONS','RINPERSOON', 
                                'GBAGEBOORTELAND', 'GBAGESLACHT',
                                'GBAGEBOORTEJAAR','GBAGEBOORTEMAAND',
                                'GBAGEBOORTEDAG', 'GBAGENERATIE',
                                'GBAHERKOMSTGROEPERING'),
                    as.data.table = TRUE)

persoon[, dob := ymd(paste(GBAGEBOORTEJAAR, GBAGEBOORTEMAAND, 
                           GBAGEBOORTEDAG, sep="-"))]
persoon[, female := GBAGESLACHT=="2"]



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ...Kindouder ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kindouder <- read_fst("data/processed/KINDOUDER2020TABV2.fst",
                      as.data.table = TRUE)
kindouder <- kindouder[RINPERSOONS=="R",] # Exclude people not in GBA (e.g. stillbirths)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ...Adoption ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
adopted <- haven::read_spss(
  "G:/VeiligheidRecht/ADOPTIEKINDEREN/1995-2012/140909 ADOPTIEKINDEREN 1995-2012V1.SAV"
)
# Note:
# When datumv is missing, the JrV variable indicates a year 
# (based on datumouder1 and/or datumouder2) that is, from what I've explored, 
# invariably later than the actual date the children started living with the
# legal parents. So here I leave date_adoption as missing when there's no datumv. 
# Later, in order to define transitions to parenthood related to these children, 
# I use the date when the child actually started living w/at least one legal parent.
adopted <- adopted %>% 
  mutate(date_adoption = ymd(datumv))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ...Households ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
households_fst <- fst("data/processed/GBAHUISHOUDENS2020BUSV1.fst")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ...Socioeconomic situation ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
secmbus_fst <- fst("data/processed/SECMBUS2021V1.fst")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ...Education ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hoogsteopltab_fst <- fst("data/processed/HOOGSTEOPL2020TABV1.fst")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ...Formal unions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
formal_unions_fst <- fst("data/processed/GBAVERBINTENISPARTNER2020BUSV1.fst")


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# 2. Merge basic info for sample selection   ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#


# Select born 2008-2014
children <- persoon[RINPERSOONS=="R"]
children <- children[GBAGEBOORTEJAAR %in% 2008:2014]


# Define time points of interest
children <- 
  children %>% 
  mutate(t1 = dob-months(24),
         t2 = dob+months(24))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ... Legal parents ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get legal parents and drop if only one is known
children <-  children %>% 
              left_join(kindouder) %>% 
              filter(RINPERSOONSpa == "R" & RINPERSOONSMa == "R")
            

# Check if it is the first transition for BOTH parents
# Get ids of legal parents
legal_parents_ids <- children %>% 
                        select(RINPERSOONpa, RINPERSOONMa) %>% 
                        pivot_longer(everything()) %>% 
                        pull(value)
                      

# All legal children from those parents
legal_children <- kindouder %>% 
                      filter(
                        RINPERSOONS=="R",
                        (
                          (RINPERSOONSpa =="R" & RINPERSOONpa %fin% legal_parents_ids) |
                            (RINPERSOONSMa =="R" & RINPERSOONMa %fin% legal_parents_ids)
                        )
                      ) %>% 
                      select(RINPERSOON, RINPERSOONpa, RINPERSOONMa) %>% 
                      as_tibble()


legal_children <- bind_rows(select(legal_children, id_parent = RINPERSOONpa, id_child = RINPERSOON),
                            select(legal_children, id_parent = RINPERSOONMa, id_child = RINPERSOON))  


legal_children <- legal_children %>% 
                     filter(id_parent %fin% legal_parents_ids)



# Get dob of each children
legal_children <-  left_join(legal_children,
                             persoon[, c("RINPERSOON", "dob")],
                             by = c("id_child" = "RINPERSOON"))



# Keep id of oldest child for each parent
# (this also excludes twins, i.e., only one child from a group of twins is kept)
oldest_child <- legal_children %>% 
                  filter(!is.na(dob)) %>% # very few children don't have valid dob
                  lazy_dt() %>% 
                  arrange(id_parent, dob) %>% 
                  group_by(id_parent) %>% 
                  summarise(id_oldest_child = id_child[1]) %>% 
                  as_tibble()



# Keep only children who are the oldest for both parents
children <- children %>% 
                left_join(rename(oldest_child,
                                 id_oldest_child_ma = id_oldest_child),
                          by = c("RINPERSOONMa" = "id_parent")) %>% 
                left_join(rename(oldest_child,
                                 id_oldest_child_pa = id_oldest_child),
                          by = c("RINPERSOONpa" = "id_parent")) %>% 
                filter(RINPERSOON == id_oldest_child_ma,
                       RINPERSOON == id_oldest_child_pa)

rm(legal_children, kindouder, oldest_child, legal_parents_ids)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ... Adopted ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
children <- children %>%  
              left_join(select(adopted,
                               RINPERSOON, 
                               datumouder1, datumouder2)) %>% 
              mutate(adopted_same_day = !is.na(datumouder1) & !is.na(datumouder2) & datumouder1==datumouder2)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ... Household records ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load HH records that for children and parents and that fall within the
# observation window (from earliest t1 until latest t2)
filter_hh_date <- intersect_days(households_fst$DATUMAANVANGHH, households_fst$DATUMEINDEHH,
                                 min(children$t1), max(children$t2),
                                 logical = T)

filter_hh_ids <- households_fst$RINPERSOON %fin% c(children$RINPERSOON,
                                                   children$RINPERSOONpa,
                                                   children$RINPERSOONMa)


households <-  households_fst[filter_hh_date & filter_hh_ids,
                              c("RINPERSOON", 
                                "DATUMAANVANGHH",
                                "DATUMEINDEHH",
                                "HUISHOUDNR",
                                "TYPHH",
                                "PLHH")]


rm(filter_hh_date, filter_hh_ids)


# Create df with one person (child and each parent) per line
# to find matching households
persons_to_hh <- 
              bind_rows(# child
                        select(children,
                               id = RINPERSOON,
                               t1, t2) %>%  
                        mutate(who = "child"),
                        # ma
                        select(children,
                               id = RINPERSOONMa,
                               t1, t2) %>%  
                          mutate(who = "ma"),
                        # pa
                        select(children,
                               id = RINPERSOONpa,
                               t1, t2) %>%  
                          mutate(who = "pa")
                        ) 


households <- households %>% 
                left_join(persons_to_hh,
                          by = c("RINPERSOON" = "id")) %>% 
                # flag if HH period included time points
                mutate(includes_t1 = intersect_days(DATUMAANVANGHH, DATUMEINDEHH,
                                                    t1, t1,
                                                    logical = T),
                       includes_t2 = intersect_days(DATUMAANVANGHH, DATUMEINDEHH,
                                                    t2, t2,
                                                    logical = T))



# Sanity test: no child should have a valid household at t1
sum(filter(households, who=="child")$includes_t1)==0


# keep only household records that corresponde to either time point
households <- households %>% 
                filter(includes_t1 | includes_t2) %>% 
                # create hhid = hh number + start date
                mutate(hhid = paste0(HUISHOUDNR, "_", DATUMAANVANGHH))


# copy hh number and start date to children df
children <- children %>% 
                
                #t1, ma
                left_join(households %>% filter(includes_t1) %>% select(RINPERSOON, hhid),
                          by = c("RINPERSOONMa" = "RINPERSOON")) %>% 
                rename(hhid_ma_t1 = hhid) %>% 
                
                #t1, pa
                left_join(households %>% filter(includes_t1) %>% select(RINPERSOON, hhid),
                          by = c("RINPERSOONpa" = "RINPERSOON")) %>% 
                rename(hhid_pa_t1 = hhid) %>% 
                
                #t2, ma
                left_join(households %>% filter(includes_t2) %>% select(RINPERSOON, hhid),
                          by = c("RINPERSOONMa" = "RINPERSOON")) %>% 
                rename(hhid_ma_t2 = hhid) %>% 
                
                #t2, pa
                left_join(households %>% filter(includes_t2) %>% select(RINPERSOON, hhid),
                          by = c("RINPERSOONpa" = "RINPERSOON")) %>% 
                rename(hhid_pa_t2 = hhid) %>% 
                
                #t2, child
                left_join(households %>% filter(includes_t2) %>% select(RINPERSOON, hhid),
                          by = c("RINPERSOON" = "RINPERSOON")) %>% 
                rename(hhid_child_t2 = hhid) 
  


children <- children %>% 
              mutate(all_present_t2 = !is.na(hhid_child_t2) & 
                                      !is.na(hhid_ma_t2)    & 
                                      !is.na(hhid_pa_t2),
                     
                     child_with_parents_t2 =  (hhid_child_t2 == hhid_ma_t2) & 
                                              (hhid_child_t2 == hhid_pa_t2)) 


rm(households)
gc()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ... Birth mother ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
prnl_files <- dir("G:/GezondheidWelzijn/PRNL",
                  pattern = "*.sav", recursive = T, full.names = T)

prnl_files <- grep(paste(2008:2014, collapse="|"), 
                   prnl_files, 
                   value = T)

prnl_all_years <- tibble()


for (file in prnl_files) {
  
  prnl_year <-  as.numeric(str_extract(file, "(?<=PRNL/)\\d{4}"))

  prnl <- read_spss(file, col_select = c("rinpersoon_moeder", "rinpersoon_kind")) %>% 
                      mutate(year = prnl_year )
  
  prnl_all_years <- bind_rows(prnl_all_years,
                              prnl)
  
  rm(prnl, prnl_year)
  
}


children <- children %>% 
              left_join(prnl_all_years,
                        by = c("RINPERSOON" = "rinpersoon_kind")) %>% 
              rename(id_birth_mother = rinpersoon_moeder)  %>% 
              mutate(birth_mother_present = (id_birth_mother == RINPERSOONMa | id_birth_mother == RINPERSOONpa))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ... Parents socioeconomic situation (SECM) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parents_ids <- children %>% 
                select(RINPERSOONMa, RINPERSOONpa, t1, t2) %>% 
                pivot_longer(c(RINPERSOONMa, RINPERSOONpa),
                             values_to = "id",
                             names_to =  "who")


# Load selected variables
filter_secmbus_ids <- (secmbus_fst$RINPERSOON %fin% parents_ids$id)


secmbus <-  secmbus_fst[filter_secmbus_ids,
                        c("RINPERSOON",  "RINPERSOONS",
                          "AANVSECM", "EINDSECM","SECM")]



# secmbus is a person-period file.
# for each parent, find periods that include t1 and t2
secmbus <- secmbus %>% 
            filter(RINPERSOONS == "R") %>% 
            left_join(parents_ids,
                      by = c("RINPERSOON" = "id")) %>% 
           
            mutate(# convert to date format
                    AANVSECM = ymd(AANVSECM),
                    EINDSECM = ymd(EINDSECM),
                    
                   # flag if period included time points
                    includes_t1 = intersect_days(AANVSECM, EINDSECM,
                                                t1, t1,
                                                logical = T),
                    includes_t2 = intersect_days(AANVSECM, EINDSECM,
                                                t2, t2,
                                               logical = T))

# check that each parent has at most one match for t1 and one for t2
secmbus %>% 
  group_by(RINPERSOON) %>% 
  summarise(includes_t1 = sum(includes_t1),
            includes_t2 = sum(includes_t2)) %>% 
  ungroup() %>% 
  tabyl(includes_t1, includes_t2)
  

# Merge
children<-   children %>% 
              left_join({secmbus %>% 
                         filter(includes_t1) %>% 
                         select(RINPERSOON,
                                ma_secm_t1 = SECM)},
                         by = c("RINPERSOONMa" = "RINPERSOON")) %>% 
              
              left_join({secmbus %>% 
                          filter(includes_t2) %>% 
                          select(RINPERSOON,
                                 ma_secm_t2 = SECM)},
                          by = c("RINPERSOONMa" = "RINPERSOON")) %>% 
              
              left_join({secmbus %>% 
                          filter(includes_t1) %>% 
                          select(RINPERSOON,
                                 pa_secm_t1 = SECM)},
                          by = c("RINPERSOONpa" = "RINPERSOON")) %>% 
              
              left_join({secmbus %>% 
                          filter(includes_t2) %>% 
                          select(RINPERSOON,
                                 pa_secm_t2 = SECM)},
                          by = c("RINPERSOONpa" = "RINPERSOON")) 
                  

rm(filter_secmbus_ids)



#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# 3. Selection of couples   ----
# (Table 1)
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# Couple type
children <- children %>% 
              left_join(select(persoon, RINPERSOON, ma_female = female),
                        by = c("RINPERSOONMa" = "RINPERSOON")) %>%
              left_join(select(persoon, RINPERSOON, pa_female = female),
                        by = c("RINPERSOONpa" = "RINPERSOON")) %>%
              mutate(ssc = ma_female == pa_female) %>% 
              # Exclude  MSSC and children with missing  sex for either parent 
              filter(!(!ma_female & !pa_female),
                     !is.na(ma_female),
                     !is.na(pa_female))


# Initial sample: DSC and FSSC couples with first legal child of both parents born in 2008-2014
children %>%  tabyl(ssc) %>% adorn_totals()


# Born in the NL
children <- children %>% 
              filter(GBAGEBOORTELAND == "6030")

children %>%  tabyl(ssc) %>% adorn_totals()



# Not adopted by both parents on same day
children <- children %>% 
              filter(!adopted_same_day)

children %>%  tabyl(ssc) %>% adorn_totals()



# Birth mother known and part of the couple
children <- children %>% 
  filter(!is.na(id_birth_mother),
          birth_mother_present)

children %>%  tabyl(ssc) %>% adorn_totals()


# All family members in NL in t2 
children <- children %>% 
              filter(all_present_t2)

children %>%  tabyl(ssc) %>% adorn_totals()



# Child living with both parents in t2
children <- children %>% 
             filter(child_with_parents_t2)

children %>%  tabyl(ssc) %>% adorn_totals()




# At least one parent is employed in t1
children <- children %>% 
            filter((ma_secm_t1=="11") |
                   (pa_secm_t1=="11")) 

children %>%  tabyl(ssc) %>% adorn_totals()


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# 4.  Individual-level dataset + non-time-varying attributes   ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

parents <- 
      children %>% 
        mutate(child_birth_year = year(dob)) %>% 
        select(child_id = RINPERSOON,
               child_female = female,
               child_dob = dob, 
               child_birth_year,
               t1, t2, 
               RINPERSOONpa, RINPERSOONMa,  
               starts_with("hhid"),
               ssc, id_birth_mother,
               pa_female, ma_female,
               contains("secm")) %>% 
  
        pivot_longer(cols = c(RINPERSOONpa, RINPERSOONMa),
                     names_to = "who",
                     values_to = "id") %>% 
  
        mutate(ma = who=="RINPERSOONMa",
               
               female = if_else(ma,
                                ma_female,
                                pa_female),
               
               hhid_t1 = if_else(ma,
                                 hhid_ma_t1,
                                 hhid_pa_t1),
               
               hhid_t2 = if_else(ma,
                                 hhid_ma_t2,
                                 hhid_pa_t2),
               
               secm_t1 = if_else(ma,
                                 ma_secm_t1,
                                 pa_secm_t1),
               
               secm_t2 = if_else(ma,
                                 ma_secm_t2,
                                 pa_secm_t2)) %>% 
  
          select(id, female, 
                  child_id, 
                  child_female,
                  child_dob,
                  child_birth_year,
                  id_birth_mother,
                  ssc,
                  hhid_t1, hhid_t2,
                  secm_t1, secm_t2,
                  t1, t2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ... Sociodemographic  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parents <- 
          parents %>% 
            left_join(select(persoon, 
                             RINPERSOON, 
                             dob,
                             generatie =  GBAGENERATIE,
                             country_birth = GBAGEBOORTELAND,
                             country_origin = GBAHERKOMSTGROEPERING),
                      by = c("id" = "RINPERSOON"))



hoogsteopltab <- hoogsteopltab_fst[hoogsteopltab_fst$RINPERSOON %in% c(parents$id),
                                   c("RINPERSOONS", "RINPERSOON", 
                                       "OPLNIVSOI2021AGG4HBmetNIRWO")] %>% 
                      filter(RINPERSOONS=="R") %>% 
                      select(-RINPERSOONS)



parents <-                   
          parents %>% 
            left_join(select(hoogsteopltab, 
                             RINPERSOON,
                             edu_code =  OPLNIVSOI2021AGG4HBmetNIRWO),
                      by = c("id" = "RINPERSOON"))


country_codes <- read_spss(dir("K:/Utilities/Code_Listings/SSBreferentiebestanden/", 
                               pattern = "LANDAKTUEELREFV.*SAV", full.names = T),
                               col_select = c("LAND", "ETNGRP")) %>% 
                               rename(ethnic_group = ETNGRP)

parents <- 
  parents %>% 
  left_join(country_codes,
            by = c("country_origin" = "LAND")) 


parents <- 
          parents %>% 
            mutate(age_at_childbirth = {difftime(child_dob, dob) %>% 
                                                 time_length("years") %>%
                                                 floor()},
                   edu = str_sub(edu_code, 1, 1) %>%  as.numeric(),
                   edu = factor(edu, 1:3, 
                                labels = c("Low", "Medium", "High")), 
                   edu = fct_na_value_to_level(edu, "(Missing)"),
                   dutch = ethnic_group=="0",
                   
                   ethnic_group = fct_drop(as_factor(ethnic_group)),
                   
                   parent_type = case_when(ssc & id==id_birth_mother ~ "fssc_birth",
                                           ssc & id!=id_birth_mother ~ "fssc_social",
                                           !ssc & female ~ "dsc_mother",
                                           !ssc & !female ~ "dsc_father"),
                   parent_type = factor(parent_type,
                                        levels = c("fssc_birth",
                                                   "fssc_social",
                                                   "dsc_mother", 
                                                   "dsc_father")))
                   
                   



#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# 5. Person-period dataset + time-varying attributes  ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# Reshape into person period
parent_time <- parents %>% 
  pivot_longer(cols = c(hhid_t1, hhid_t2, secm_t1, secm_t2)) %>% 
  
  mutate(time = if_else(str_ends(name, "t1"), 1, 2),
         time_date = if_else(time==1, t1, t2),
         name = str_remove(name, "_t\\d")) %>% 
  select(-t1, -t2) %>% 
  
  pivot_wider(names_from = name,
              values_from = value) %>% 
  
  mutate(time_year = year(time_date),
         time_month = month(time_date))



# For each parent, get years of t1 and t2 to load only the relevant 
# employment records
parents_ids <- parent_time %>%  
  select(id, time_year)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ... Marriage/reg partnership  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
formal_unions <- formal_unions_fst[formal_unions_fst$RINPERSOON %in% parents_ids$id,
                                   c("RINPERSOON", "AANVANGVERBINTENIS", 
                                     "EINDEVERBINTENIS", "RINPERSOONVERBINTENISP")] %>% 
                mutate(EINDEVERBINTENIS = if_else(EINDEVERBINTENIS=="88888888", "20500101", EINDEVERBINTENIS),
                       u_start_date = ymd(AANVANGVERBINTENIS),
                       u_end_date = ymd(EINDEVERBINTENIS))  %>% 
                select(id = RINPERSOON,
                       spouse_id = RINPERSOONVERBINTENISP,
                       u_start_date, u_end_date)

formal_unions <- 
            formal_unions %>% 
              # Using parents df here to have t1 and t2 in separate columns
              left_join(parents %>% select(id, t1, t2)) %>% 
              mutate(includes_t1 = intersect_days(u_start_date, u_end_date,
                                                  t1, t1,
                                                  logical = T),
                     includes_t2 = intersect_days(u_start_date, u_end_date,
                                                  t2, t2,
                                                  logical = T)) %>% 
              
              select(id, spouse_id, includes_t1, includes_t2) %>% 
              pivot_longer(cols = starts_with("includes")) %>% 
              mutate(time = str_sub(name, -1) %>%  as.numeric()) %>% 
              select(-name, 
                     married = value) %>% 
              filter(married)

parent_time <- 
              parent_time %>% 
                left_join(formal_unions) %>% 
                mutate(married = if_else(is.na(married),
                                         FALSE,
                                         married))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ... Employment info - (S)POLIS ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

empl_all_years <- tibble()

# ........................
# ...... 2006-2009 ----
# ........................
# Load each year, process and append
polis_files <-  dir("data/processed/", pattern = "^POLIS.*fst", full.names = T) 

polis_files <- grep(paste(2006:2009, collapse="|"), 
                    polis_files, 
                    value = T)


for (file in polis_files) {
  
  cli::cli_progress_step(file)
  
  # Load selected variables
  polis_fst <- fst(file)
  polis_ids <- polis_fst$RINPERSOON
  polis_year <- as.numeric(str_extract(file, "\\d{4}"))
  
  polis_filter <- (polis_ids %fin% filter(parents_ids, time_year==polis_year)$id) 
  
  
  polis <- polis_fst[polis_filter, c("RINPERSOONS", "RINPERSOON", "AANTVERLU", 
                                     "SRTIV", "AANVBUS",
                                     "BASISLOON", "BASISUREN", "LNINGLD",
                                     "REGULIEREUREN",  "POLISDIENSTVERBAND",
                                     "WEKARBDUURKLASSE", "BEID", "SOORTBAAN") ]
  polis <- as.data.table(polis)
  polis <- polis[RINPERSOONS=="R", ]
  polis <- polis[SRTIV %fin% c("11", "12", "13", "14", "15", "17"), ]
  
  polis[, year := year(AANVBUS)]
  polis[, month := month(AANVBUS)]
  
  
  polis <-   polis %>% 
    as_tibble() %>% 
    select(RINPERSOON, year, month,
           empl_paid_hours = AANTVERLU, 
           empl_base_hours = BASISUREN,
           empl_regular_hours =  REGULIEREUREN,
           empl_base_wage = BASISLOON,
           empl_gross_wage = LNINGLD,
           empl_business_id = BEID,
           empl_job_type = SOORTBAAN,
           empl_wkly_hours_range = WEKARBDUURKLASSE, 
           empl_full_time = POLISDIENSTVERBAND
    ) %>% 
    mutate(empl_full_time = abs(as.numeric(empl_full_time)-2)) # originally 1=Ja, 2=Nee,
  
  empl_all_years <- bind_rows(empl_all_years, polis)
  
  rm(polis, polis_filter, polis_ids, polis_year)
  cli::cli_progress_done()
  
  
}



#........................
# ...... 2010-2016 ----
#........................
# Load each year, process and append
spolis_files <-  dir("data/processed/", pattern = "SPOLIS.*fst", full.names = T) 

spolis_files <- grep(paste(2010:2016, collapse="|"), 
                     spolis_files, 
                     value = T)


for (file in spolis_files) {
  
  cli::cli_progress_step(file)
  
  
  # Load selected variables 
  spolis_fst <- fst(file)
  spolis_ids <- spolis_fst$RINPERSOON
  spolis_year <- as.numeric(str_extract(file, "\\d{4}"))
  
  spolis_filter <- (spolis_ids %fin% filter(parents_ids, time_year==spolis_year)$id) 
  
  spolis <- spolis_fst[spolis_filter, c("RINPERSOONS", "RINPERSOON", "SAANTVERLU",
                                        "SBASISLOON", "SBASISUREN", "SLNINGLD",
                                        "SREGULIEREUREN", "SOVERWERK", "SPOLISDIENSTVERBAND",
                                        "SSRTIV", "SWEKARBDUURKLASSE", "SBEID", "SSOORTBAAN",
                                        "SDATUMAANVANGIKO") ]
  
  spolis <- as.data.table(spolis)
  spolis <- spolis[RINPERSOONS=="R", ]
  spolis <- spolis[SSRTIV %fin% c("11", "12", "13", "14", "15", "17"), ]
  
  spolis[, year := year(SDATUMAANVANGIKO)]
  spolis[, month := month(SDATUMAANVANGIKO)]
  
  
  spolis <-   spolis %>% 
    as_tibble() %>% 
    select(RINPERSOON, year, month,
           empl_paid_hours = SAANTVERLU, 
           empl_base_hours = SBASISUREN,
           empl_regular_hours =  SREGULIEREUREN,
           empl_base_wage = SBASISLOON,
           empl_gross_wage = SLNINGLD,
           empl_business_id = SBEID,
           empl_wkly_hours_range = SWEKARBDUURKLASSE,
           empl_job_type = SSOORTBAAN,
           empl_full_time = SPOLISDIENSTVERBAND
    ) %>% 
    mutate(empl_full_time = abs(as.numeric(empl_full_time)-2))
  
  
  empl_all_years <- bind_rows(empl_all_years, spolis)
  
  rm(spolis, spolis_filter, spolis_ids, spolis_year)
  cli::cli_progress_done()
  
  
}


#........................
# ...... Prepare and merge ----
#........................

# Select only months corresponding to t1 and t2
employment <- empl_all_years %>% 
              left_join({parent_time %>% 
                          select(id, time_month, time_year) %>% 
                          mutate(keep = TRUE)},
                        by = c("RINPERSOON" = "id",
                               "year" = "time_year",
                               "month" = "time_month")) %>% 
              filter(keep)

rm(empl_all_years)
gc()

# Aggregate by person-month (relevant to people w/more than one record per month)
# - Sum continuous variables (wages, hours)
# - For categorical variables, get values for record with the most hours 
# in the month. Except for the full time employment indicator: 
# it gets 1 if at least one of the jobs was full time
employment <- 
          employment %>% 
          arrange(RINPERSOON, year, month, empl_paid_hours) %>% 
          group_by(RINPERSOON, year, month) %>% 
          summarise(
                    empl_paid_hours = sum(empl_paid_hours), 
                    empl_base_hours = sum(empl_base_hours),
                    empl_regular_hours =  sum(empl_regular_hours),
                    empl_base_wage = sum(empl_base_wage),
                    empl_gross_wage = sum(empl_gross_wage),
                    
                    empl_business_id = empl_business_id[1],
                    empl_wkly_hours_range = empl_wkly_hours_range[1],
                    empl_job_type = empl_job_type[1],
                    
                    empl_full_time = min(as.numeric(empl_full_time)==1)) %>% 
          ungroup()
                    
    
# Merge
parent_time <- 
          parent_time %>% 
            left_join(employment,
                      by = c("id" = "RINPERSOON",
                             "time_year" = "year",
                             "time_month" = "month"))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ... Work experience at t1  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This reproduces info from RAVTAB, which is computed from SECMBUS
# It refers to number of months spent in paid work in the four years ending
# in the reference month
# For efficiency, here I compute only for parents who are employed in t1, 
# since they are the focus of the analysis


parents_working_t1 <- parent_time %>% 
                          filter(time == 1,
                                 secm == "11") %>% 
                          pull(id)

# Define window for each parent
work_window <- parents %>%
                filter(id %fin% parents_working_t1) %>% 
                 mutate(end_work_window = t1, # ref month
                        start_work_window = end_work_window - months(47)) %>%
                 select(id, end_work_window, start_work_window)


# Keep SECM records falling within relevant time window
work_exp <- secmbus %>%
              left_join(work_window,
                        by = c("RINPERSOON" = "id")) %>%
              mutate(within_work_window = intersect_days(ymd(AANVSECM), ymd(EINDSECM),
                                                         start_work_window, end_work_window,
                                                         logical = T)) %>%
              filter(within_work_window) %>%
              select(-within_work_window)


# Identify records that span the entire work_window. 
# We don't need further computation for them: they are continuously in paid work
# (since we already selected only those who were working in t1)
continuous_work <- work_exp %>% 
                      filter(ymd(AANVSECM) <= start_work_window,
                             ymd(EINDSECM) >= end_work_window) %>% 
                      pull(RINPERSOON) %>% 
                      unique()
# Note that the above is only a subset  people continuously working:
# a person can be continuously working but change the secm (e.g. from empl
# to self-empl). 

# For people without a single secm record spanning the entire work_window,
# we need to create a person-month file.
# each SECM record is multiplied by the number of months it spans
work_exp <-
  work_exp %>%
  filter(!(RINPERSOON %fin% continuous_work)) %>% 
  mutate(n_months = {difftime(ymd(EINDSECM)+days(5), ymd(AANVSECM)) %>%
             time_length("months") %>%
             floor()}) %>%
  uncount(n_months,
          .remove=FALSE,
          .id = "month_index") %>%

  mutate(row_date =  ymd(AANVSECM) + months(month_index-1),
         year = year(row_date),
         month = month(row_date))


# Keep only person-months falling within relevant time window
work_exp <- work_exp %>%
            mutate(within_work_window = intersect_days(row_date, row_date,
                                                       start_work_window, end_work_window,
                                                       logical = T)) %>%
            filter(within_work_window)


#................................
# Most people should have 48 observations now.
# Some can have less if they were not in GBA
# (e.g. not livig in NL)
work_exp %>%
  group_by(RINPERSOON) %>%
  summarise(n = n()) %>%
  skim(n)
#................................

# Compute number of months in paid work
work_exp <-
  work_exp %>%
  mutate(working = SECM %in% as.character(11:15),
         not_working = !working) %>%
  arrange(RINPERSOON, desc(row_date)) %>% 
  group_by(RINPERSOON) %>%
  mutate(
         # We are counting backwards, starting from the most recent month
         # Cumulative sum of months not working 
         cumsum_notworking = cumsum(not_working),
         
         # once we hit 2 months without working, we start ignoring
         # the months in employment. 
         to_ignore  =  cumsum_notworking >=2,
         
         # Thus, we only count the working months before the interruption
         to_count = working & !to_ignore
         ) %>% 
  summarise(months_worked = sum(to_count)) %>% 
  # Include the continuously working
  bind_rows(data.frame(RINPERSOON = continuous_work,
                       months_worked = 48)) %>%
  mutate(work_exp_t1 =
           case_when(
                     months_worked %in% 1:11        ~   1,
                     months_worked %in% 12:23       ~   2,
                     months_worked %in% 24:35       ~   3,
                     months_worked %in% 36:47       ~   4,
                     months_worked %in% 48          ~   5
                    ),
         
          work_exp_t1 = factor(work_exp_t1, 
                               levels = 1:5,
                               labels =  c("Less than 1yr",
                                           "At least  1yr",
                                           "At least  2yrs",
                                           "At least  3yrs",
                                           "At least  4yrs"))
          )

# Merge
parent_time <-
  parent_time %>%
  left_join(select(work_exp, 
                   RINPERSOON, work_exp_t1, months_worked_t1 = months_worked),
            by = c("id" = "RINPERSOON")) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ... Employment sector ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We need BETAB for each year, because employers come and go and they can
# even change sectors

# Get employers ids that we need for each year
employers_ids <- parent_time %>% 
                  filter(!is.na(empl_business_id)) %>% 
                  select(empl_business_id, time_year) %>% 
                  distinct()


# Load each year, process and append
betab_files <- dir("G:/Arbeid/BETAB/",
                  pattern = "*.sav|*.SAV", recursive = T, full.names = T)


betab_files <- grep(paste(2006:2016, collapse="|"), 
                    betab_files, 
                    value = T)
      #Note: there's only file version per year in 2006-2016


betab_all_years <- tibble()

for (file in betab_files) {
  
  cli::cli_progress_step(file)
  
  # Load 
  betab_year <- as.numeric(str_extract(file, "\\d{4}"))
  
  betab <- read_spss(file,
                     col_select = c("BEID",
                                    starts_with("SBI2008"))) %>% 
           rename_with(~"sbi2008", starts_with("SBI2008")) %>% 
           # labels change a bit from year to year, 
           # so better to drop them
           mutate(sbi2008 = as.character(sbi2008),
                  year =  betab_year)
  
  # keep only the necessary employers
  betab<-  betab %>% 
           filter(BEID %fin% 
                   filter(employers_ids, time_year==betab_year)$empl_business_id)
        
  
  betab_all_years <-  bind_rows(betab_all_years,
                                betab)
  
  rm(betab, betab_year)
  cli::cli_progress_done()
  
  
}

# Get letters (based on 2 digits codes) and identify public a majority-female sectord
sbi_letters <- read_spss("K:/Utilities/Code_Listings/Bedrijfsindelingen/SBI2008/SBI2008 letter_2digit.sav") 

betab_all_years <- 
        betab_all_years %>% 
          mutate(sbi2008_2digits =  str_sub(sbi2008, 0, 2)) %>% 
          left_join(sbi_letters) %>% 
          mutate(public_sector = sbi2008_letter %in% c("O", "P", "Q"),
		  
				 # Sectors with >50% female in 2011. See:
				 # Source: https://opendata.cbs.nl/statline/#/CBS/nl/dataset/82807NED/table?dl=9C2C8
                 female_sector = sbi2008_letter %in% c("I", "P", "Q", "R", "S", "T"))

# Merge
parent_time <- 
  parent_time %>% 
  left_join(select(betab_all_years, BEID, year,
                   sbi2008, sbi2008_letter, public_sector, female_sector),
            by = c("empl_business_id" = "BEID",
                   "time_year" = "year"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ...Annual income ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


income_all_years <- tibble()


# ........................
# ..... 2006-2010 ----
# ........................

# Load each year, process and append
ipi_files <-  dir("data/processed/", pattern = "PERSOONINK.*fst", full.names = T) 
# Keep relevant years 
ipi_files <- ipi_files[str_detect(ipi_files, "2006|2007|2008|2009|2010")]

for (file in ipi_files) {
  
  cli::cli_progress_step(file)
  
  # Load selected variables only for parents in a given year
  ipi_fst <- fst(file)
  ipi_ids <- ipi_fst$RINPERSOON
  ipi_year <- as.numeric(str_extract(file, "20\\d\\d"))
  ipi_filter <- (ipi_ids %fin% filter(parents_ids, time_year==ipi_year)$id) 
  
  ipi <- ipi_fst[ipi_filter,
                 c('RINPERSOONS', 'RINPERSOON', 'PERSINK')]
  
  ipi <-  ipi %>% 
    mutate_at(vars(PERSINK),
              ~if_else(.==999999999, NA_real_, .)) %>% 
    as_tibble()
  
  
  
  ipi <- ipi %>% 
    filter(RINPERSOONS=="R") %>% 
    # Different from what CBS does for published estimates,
    # I don't exclude the income of people in institutional households here
    mutate(yr_inc_personal = PERSINK,
           year = ipi_year) %>% 
    select(RINPERSOON, year, yr_inc_personal)
  
  
  income_all_years <- bind_rows(income_all_years, ipi)
  
  rm(ipi, ipi_filter, ipi_ids, ipi_year)
  cli::cli_progress_done()
  
}



# ........................
# ..... 2011-2016 ----
# ........................

# Load each year, process and append
inpatab_files <-  dir("data/processed/", pattern = "INPA.*fst", full.names = T)


for (file in inpatab_files) {
  
  cli::cli_progress_step(file)
  
  # Load selected variables only for people in repartnered data 
  # (either ego or partner)
  inpatab_fst <- fst(file)
  inpatab_ids <- inpatab_fst$RINPERSOON
  inpatab_year <- as.numeric(str_extract(file, "20\\d\\d"))
  inpatab_filter <- (inpatab_ids %fin% filter(parents_ids, time_year==inpatab_year)$id) 
  
  inpatab <- inpatab_fst[inpatab_filter,
                         c('RINPERSOONS', 'RINPERSOON','INPPERSINK')]
  
  inpatab <-  inpatab %>% 
    mutate_at(vars(INPPERSINK), as.numeric) %>% 
    mutate_at(vars(INPPERSINK),
              ~if_else(.==9999999999, NA_real_, .)) %>% 
    as_tibble()
  
  
  inpatab <- inpatab %>% 
    filter(RINPERSOONS=="R") %>% 
    # Different from what CBS does for published estimates,
    # I don't exclude the income of people in institutional households here
    mutate(yr_inc_personal = INPPERSINK,
           year = inpatab_year) %>% 
    select(RINPERSOON, year, yr_inc_personal)
  
  
  
  income_all_years <- bind_rows(income_all_years, inpatab)
  
  rm(inpatab, inpatab_ids, inpatab_filter, inpatab_year)
  cli::cli_progress_done()
  
  
}

gc()



# Income vars in IPI are truncated at -500000 and 1000000, 
# so doing the same for the following years here
income_all_years <- income_all_years %>% 
  mutate_at(vars(starts_with("yr_inc")),
            ~if_else(. < -500000, -500000, .)) %>% 
  mutate_at(vars(starts_with("yr_inc")),
            ~if_else(. >  1000000, 1000000, .))


# Merge   
parent_time <- 
  parent_time %>% 
  left_join(income_all_years,
            by = c("id" = "RINPERSOON",
                   "time_year" = "year"))

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# 6. Save ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

write_fst(parent_time,
          "../parentswages/data/parent_time.fst",
          compress=100)