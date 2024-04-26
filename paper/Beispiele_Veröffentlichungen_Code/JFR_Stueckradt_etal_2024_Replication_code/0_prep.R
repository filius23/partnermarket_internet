#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# > ParentsWages 
# 
#  0. Master  
#  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# 0 - Setup ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

library(fst)
library(tidyverse)
library(janitor)
library(lubridate)
library(skimr)
library(modelsummary)
library(fixest)
library(marginaleffects)
library(sampleSelection)
library(gtsummary)
library(flextable)
library(sandwich)
library(writexl)
options(scipen=999)


parent_time <- read_fst("data/parent_time.fst")


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# 1 - Recodes ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# Employment indicators
parent_time <- 
  parent_time %>% 
  mutate(# Negative values into zero
         empl_base_wage = if_else(empl_base_wage<0,
                                  0,
                                  empl_base_wage),
         
         empl_base_hours = if_else(empl_base_hours<0,
                                   0,
                                   empl_base_hours),
         
         # Only define hourly wage for employees
         # with nonzero wages and hours
         hourly_wage = if_else((secm=="11" &  empl_base_wage != 0 & empl_base_hours != 0),
                                empl_base_wage/empl_base_hours,
                                NA_real_),
         
         ln_wage = log(hourly_wage),

         
         min_employment =   secm=="11" &
                            hourly_wage >= 7 &
                            empl_base_hours >= 48,

         min_employment = if_else(is.na(min_employment),
                                  FALSE, 
                                  min_employment),
         
         other_paid_work = secm %in% c("12", "13", "14", "15"),
         
         earner = (min_employment | other_paid_work),
         
         self_empl = secm %in% c("13", "14"),
         
         part_time = empl_base_hours<=136,
         
         wage_quantile = ntile(hourly_wage, 5),
         inc_quantile =  ntile(yr_inc_personal, 5),
         
         work_exp_t1_num = as.numeric(work_exp_t1)
         
         ) %>% 
  
  arrange(id, time) %>% 
  group_by(id) %>% 
  mutate(min_employment_t1 = min_employment[1],
         has_wage_t1 = !is.na(hourly_wage[1]),
         has_wage_t2 = !is.na(hourly_wage[2]),
         other_paid_work_t2 = other_paid_work[2],
         

         change_employer = empl_business_id[1] != empl_business_id[2],
         
         stop_employment = min_employment[1] & !min_employment[2],
         
         stop_working = min_employment[1] & !earner[2],
         
         change_to_selfempl = min_employment[1] & self_empl[2]
         
         ) %>% 
  ungroup()


# Get partners' characteristics at each time
parent_time <-
  parent_time %>%
  group_by(child_id, time) %>%
  # For parent in row 1, get info from partner in row 2, and vice versa
  mutate(across(c(hhid, earner, min_employment, inc_quantile),
                ~if_else(row_number()==1,
                        .x[2],
                        .x[1]),
                .names = "partner_{.col}")) %>%
  ungroup() %>%
  mutate(living_together = hhid==partner_hhid,
         living_together = if_else(is.na(living_together), 
                                   FALSE, 
                                   living_together),
         sole_earner = earner & !partner_earner)




#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# 2 - Samples  ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# Note that further filters are applied when computing the descriptive 
# stats and running models.
# At this step we are keeping all cases with valid hourly wages in t1, but
# main results only include people with min_employment_t1

df_crossec <- parent_time %>%
              pivot_wider(names_from = time,
                        values_from = c(time_date, time_year, time_month,
                                        hhid, secm, married, spouse_id,
                                        starts_with("empl"), other_paid_work,
                                        yr_inc_personal, sbi2008, sbi2008_letter,
                                        public_sector, female_sector,
                                        hourly_wage, ln_wage, part_time,
                                        self_empl, wage_quantile,
                                        earner, min_employment, inc_quantile,
                                        starts_with("partner"),
                                        living_together, sole_earner,
                        )) %>% 
            filter(!is.na(hourly_wage_1))
  

# Data frame for FE models
df_fe <-  parent_time %>%
             filter(id %in% df_crossec$id)





  