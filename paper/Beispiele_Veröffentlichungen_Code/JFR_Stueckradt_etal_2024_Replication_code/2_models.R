#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# > ParentsWages 
# 
#  2. Models   
#  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# 0. Equations ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# OLS equation without interaction
equation <- formula("ln_wage_2 ~ 
                            parent_type + 
                            change_employer + 
                            sole_earner_1 + sole_earner_2 + 
                            factor(work_exp_t1) +
                            part_time_1 + part_time_2 +
                            public_sector_1 + public_sector_2 +
                            female_sector_1 + female_sector_2 +
                            married_1 + married_2 +
                            living_together_1 + 
                            age_at_childbirth + edu +
                            ethnic_group + 
                            factor(wage_quantile_1) + factor(child_birth_year)")


# With interaction
equation_int <- update(equation, 
                            . ~ . + parent_type:change_employer)


# Selection equation for Heckman model
equation_selection <- formula("min_employment_2 ~
                                      parent_type +
                                      married_1 + married_2 + 
                                      factor(work_exp_t1) +
                                      factor(inc_quantile_1) + 
                                      factor(partner_inc_quantile_1)")

# Selection for additional analysis
equation_selection_ext <- update(equation_selection,
                                   has_wage_t2 ~.)

# FE
equation_fe <- formula("ln_wage ~ 
                         time + 
                         time:parent_type +
                         time:change_employer + 
                         sole_earner +  part_time + 
                         public_sector + female_sector +
                         married | id")
  
equation_fe_int <- formula("ln_wage ~ 
                             time + 
                             time:parent_type +
                             time:change_employer + 
                             time:parent_type:change_employer +
                             sole_earner +  part_time + 
                             public_sector + female_sector +
                             married | id")



#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# 1. Main analysis ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ...OLS ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# without interaction
m_ols_1 <- df_crossec %>% 
            filter(min_employment_1,
                   min_employment_2) %>% 
            feols(equation,
                  cluster = ~child_id,
                  data = .)

# with interaction
m_ols_2 <- df_crossec %>% 
            filter(min_employment_1,
                   min_employment_2) %>% 
              update(m_ols_1, 
                     equation_int,
                     data=.)



#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ...Heckman ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#


m_heck_1 <- df_crossec %>% 
            filter(min_employment_1, 
                   !other_paid_work_2) %>% 
            selection(selection = equation_selection,
                      outcome = equation,
                      data = .)


m_heck_2 <- df_crossec %>% 
             filter(min_employment_1,
                    !other_paid_work_2) %>% 
             selection(selection = equation_selection,
                       outcome = equation_int,
                       data = .)



# Cluster SEs
heck_clusters <- df_crossec %>%  
                  filter(min_employment_1,
                         !other_paid_work_2) %>%  
                  # Heckman model drops cases with missing income quantiles in t1
                  # (parents or their partners not registered in the NL in the ref date
                  # for tax stats in year of t1). So we need to drop them here for clustering
                  filter(if_all(c(inc_quantile_1, 
                                  partner_inc_quantile_1), 
                                ~!is.na(.))) %>% 
                  pull(child_id)

m_heck_1[["vcovAll"]] <- vcovCL(m_heck_1, heck_clusters)
m_heck_2[["vcovAll"]] <- vcovCL(m_heck_2, heck_clusters)




#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ...FE ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
m_fe_1 <- df_fe %>% 
            filter(min_employment_t1,
                   !other_paid_work_t2) %>% 
            feols(equation_fe,
                  cluster = ~child_id,
                  data = .)

m_fe_2 <- df_fe %>% 
            filter(min_employment_t1,
                   !other_paid_work_t2) %>% 
            feols(equation_fe_int,
                  cluster = ~child_id,
                  data = .)


modelsummary(list(m_fe_1, m_fe_2),
             stars = TRUE)


ame <- avg_slopes(m_fe_1,
                     variables = "time",
                     by = "parent_type")




# Changing ref categories
m_fe_1_fssc_social <- df_fe %>% 
                          filter(min_employment_t1,
                                 !other_paid_work_t2) %>% 
                          mutate(parent_type = fct_relevel(parent_type, "fssc_social")) %>% 
                          feols(equation_fe,
                                cluster = ~child_id,
                                data = .)
                        
m_fe_1_dsc_mother <- df_fe %>% 
                        filter(min_employment_t1,
                               !other_paid_work_t2) %>% 
                        mutate(parent_type = fct_relevel(parent_type, "dsc_mother")) %>% 
                        feols(equation_fe,
                              cluster = ~child_id,
                              data = .)

m_fe_1_dsc_father <- df_fe %>% 
                        filter(min_employment_t1,
                               !other_paid_work_t2) %>% 
                        mutate(parent_type = fct_relevel(parent_type, "dsc_father")) %>% 
                        feols(equation_fe,
                              cluster = ~child_id,
                              data = .)



#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# 2. Extended sample  ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ...OLS ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# without interaction
m_ols_ext_1 <- df_crossec %>% 
                    filter(has_wage_t1,
                           has_wage_t2) %>% 
                    feols(equation,
                          cluster = ~child_id,
                          data = .)

# with interaction
m_ols_ext_2 <- df_crossec %>% 
                    filter(has_wage_t1,
                           has_wage_t2) %>% 
                    update(m_ols_1, 
                           equation_int,
                           data=.)




#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ...Heckman ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#


m_heck_ext_1 <- df_crossec %>% 
              filter(has_wage_t1,
                     !other_paid_work_2) %>% 
              selection(selection = equation_selection_ext,
                        outcome = equation,
                        data = .)


m_heck_ext_2 <- df_crossec %>% 
              filter(has_wage_t1,
                     !other_paid_work_2) %>% 
              selection(selection = equation_selection_ext,
                        outcome = equation_int,
                        data = .)
            

# Cluster SEs
heck_clusters_ext <- df_crossec %>%  
                      filter(has_wage_t1,
                             !other_paid_work_2) %>%  
                      # Heckman model drops cases with missing income quantiles in t1
                      # (parents or their partners not registered in the NL in the ref date
                      # for tax stats in year of t1). So we need to drop them here for clustring
                      filter(if_all(c(inc_quantile_1, 
                                      partner_inc_quantile_1), 
                                    ~!is.na(.))) %>% 
                      pull(child_id)

m_heck_ext_1[["vcovAll"]] <- vcovCL(m_heck_ext_1, heck_clusters_ext)
m_heck_ext_2[["vcovAll"]] <- vcovCL(m_heck_ext_2, heck_clusters_ext)




#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ...FE ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
m_fe_ext_1 <- df_fe %>% 
            filter(has_wage_t1,
                   !other_paid_work_t2) %>% 
            feols(equation_fe,
                  cluster = ~child_id,
                  data = .)

m_fe_ext_2 <- df_fe %>% 
            filter(has_wage_t1,
                   !other_paid_work_t2) %>% 
            feols(equation_fe_int,
                  cluster = ~child_id,
                  data = .)

ame_ext <- avg_slopes(m_fe_ext_1,
                  variables = "time",
                  by = "parent_type")


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# 3. Save output ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ...Tables ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# OLS models
table_ols <- modelsummary(list(m_ols_1, 
                               m_ols_2,
                               m_ols_ext_1,
                               m_ols_ext_2),
             stars = TRUE,
             output = "flextable") %>% 
  add_header_row(values = c("", "Main", "Extended"),
                 colwidths = c(1,2,2))

table_ols
save_as_docx(table_ols, path = "output/models_ols.docx")


# Heckman models
table_heck <- modelsummary(list(m_heck_1, 
                                m_heck_2,
                                m_heck_ext_1,
                                m_heck_ext_2),
             stars = TRUE,
             shape =  term + statistic ~ model + component,
             output = "flextable") %>% 
              add_header_row(values = c("",  "Main", "Extended"),
                             colwidths = c(1,6,6))
table_heck
save_as_docx(table_heck, path = "output/models_heckman.docx")



# FE models
table_fe <- modelsummary(list(m_fe_1, 
                                m_fe_2,
                                m_fe_ext_1,
                                m_fe_ext_2),
                           stars = TRUE,
                           output = "flextable") %>% 
  add_header_row(values = c("",  "Main", "Extended"),
                 colwidths = c(1,2,2))

table_fe
save_as_docx(table_fe, path = "output/models_fe.docx")


table_fe_refcats <- modelsummary(list(`Ref: FSSC birth mother` = m_fe_1, 
                                      `Ref: FSSC social mother` = m_fe_1_fssc_social,
                                      `Ref: DSC  mother` = m_fe_1_dsc_mother,
                                      `Ref: DSC  father` = m_fe_1_dsc_father),
                                 stars = TRUE,
                                 output = "flextable") 
table_fe_refcats
save_as_docx(table_fe_refcats, path = "output/models_fe_refcats.docx")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ...Figures ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p <-
ame %>%  
  mutate(parent_type = factor(parent_type,
                              levels = c("fssc_birth",
                                         "fssc_social",
                                         "dsc_mother",
                                         "dsc_father"),
                              labels = c("FSSC birth mother",
                                         "FSSC non-birth mother",
                                         "DSC mother",
                                         "DSC father"))) %>% 
  ggplot(aes(parent_type,
             estimate,
             ymax= conf.high,
             ymin= conf.low)) + 
  geom_point() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits=c(.1, .2),
                     labels = function(x) format(round(x, 2), nsmall = 2)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  theme_light() +
  labs(x="", y="Avg. marginal effect on ln(wage)")


p 

ggsave("ame.png", p,
       width = 7, height=5)

