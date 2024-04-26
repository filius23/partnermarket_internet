#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# > ParentsWages 
# 
#  1. Descriptives   
#  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initial sample ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tbl_desc_initial <- 
          df_crossec %>% 
          
          filter(min_employment_t1) %>% 
          
          select(parent_type,
                 starts_with("hourly"),
                 change_employer,
                 stop_employment,
                 stop_working,
                 change_to_selfempl,
                 work_exp_t1_num,
                 work_exp_t1,
                 starts_with("sole"),
                 starts_with("part_time"),
                 starts_with("public"),
                 starts_with("female_sector"),
                 dutch,
                 ethnic_group,
                 starts_with("married"),
                 age_at_childbirth,
                 living_together_1,
                 edu) %>% 
          mutate(N = 1) %>% 
          tbl_summary(by=parent_type,
                      type = list(work_exp_t1_num ~ 'continuous'),
                      #otherwise work_exp_t1_num is treated as cat because of few levels
                      statistic = list(
                        all_continuous() ~ "{mean}  \n ({sd})",
                        all_categorical() ~ "{p}",
                        N ~ "{n}"
                      ),
                      digits= list(all_continuous() ~ 2,
                                   all_categorical() ~ function(x) style_number(x, digits = 2),
                                   N ~ 0),
                      missing ="no",
          )  %>% 
          modify_header(label = "**Variable**", all_stat_cols() ~"**{level}**") %>% 
          modify_footnote(all_stat_cols() ~NA) %>% 
          as_flex_table() %>% 
          set_caption("Initial sample: persons with hourly wage >= 7 and monthly hours >=48 in t1")


tbl_desc_initial

save_as_docx(tbl_desc_initial, path = "output/desc_initial.docx")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Main sample ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tbl_desc_main <- 
       df_crossec %>% 
  
         filter(min_employment_t1,
                !other_paid_work_2) %>% 
  
          select(parent_type,
                 starts_with("hourly"),
                 change_employer,
                 stop_employment,
                 stop_working,
                 change_to_selfempl,
                 work_exp_t1_num,
                 work_exp_t1,
                 starts_with("sole"),
                 starts_with("part_time"),
                 starts_with("public"),
                 starts_with("female_sector"),
                 dutch,
                 ethnic_group,
                 starts_with("married"),
                 age_at_childbirth,
                 living_together_1,
                 edu) %>% 
           mutate(N = 1) %>% 
           tbl_summary(by=parent_type,
                       type = list(work_exp_t1_num ~ 'continuous'),
                                 #otherwise work_exp_t1_num is treated as cat because of few levels
                       statistic = list(
                                          all_continuous() ~ "{mean}  \n ({sd})",
                                          all_categorical() ~ "{p}",
                                          N ~ "{n}"
                                          ),
                       digits= list(all_continuous() ~ 2,
                                    all_categorical() ~ function(x) style_number(x, digits = 2),
                                    N ~ 0),
                       missing ="no",
                       )  %>% 
           modify_header(label = "**Variable**", all_stat_cols() ~"**{level}**") %>% 
           modify_footnote(all_stat_cols() ~NA) %>% 
           as_flex_table() %>% 
          set_caption("Main sample: persons with hourly wage >= 7 and monthly hours >=48 in t1, employed or out of paid work in t2")

tbl_desc_main

save_as_docx(tbl_desc_main, path = "output/desc_main.docx")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extended sample ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tbl_desc_ext <- 
    df_crossec %>% 
        
        filter(has_wage_t1) %>% 
        
        select(parent_type,
               starts_with("hourly"),
               change_employer,
               stop_employment,
               stop_working,
               change_to_selfempl,
               work_exp_t1_num,
               work_exp_t1,
               starts_with("sole"),
               starts_with("part_time"),
               starts_with("public"),
               starts_with("female_sector"),
               dutch,
               ethnic_group,
               starts_with("married"),
               age_at_childbirth,
               living_together_1,
               edu,
               work_exp_t1) %>% 
        mutate(N = 1) %>% 
        tbl_summary(by=parent_type,
                    type = list(work_exp_t1_num ~ 'continuous'),
                    #otherwise work_exp_t1_num is treated as cat because of few levels
                    statistic = list(
                      all_continuous() ~ "{mean}  \n ({sd})",
                      all_categorical() ~ "{p}",
                      N ~ "{n}"
                    ),
                    digits= list(all_continuous() ~ 2,
                                 all_categorical() ~ function(x) style_number(x, digits = 2),
                                 N ~ 0),
                    missing ="no",
        )  %>% 
        modify_header(label = "**Variable**", all_stat_cols() ~"**{level}**") %>% 
        modify_footnote(all_stat_cols() ~NA) %>% 
        as_flex_table() %>% 
        set_caption("Extended sample: persons with nonzero hourly wage in t1")


tbl_desc_ext

save_as_docx(tbl_desc_ext, path = "output/desc_ext.docx")
