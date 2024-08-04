# load packages ----------------------------------------------------------------
pacman::p_load("tidyverse", "marginaleffects", "systemfonts", 
               "modelsummary", "flextable", "fixest") #"ggfortify"


# load data --------------------------------------------------------------------
df_finish <- readRDS("./data/df_finish.RDS")


# separate data sets by gender -------------------------------------------------
sample_f <- df_finish[df_finish$female=="1",]
sample_m <- df_finish[df_finish$female=="0",]


# plot settings ----------------------------------------------------------------
# font_fam1 <- "Open Sans" # r
font_fam1 <- "Times New Roman"

theme_set(
  theme_minimal(base_family = font_fam1,base_size = 9) + 
    theme(legend.position = "top",
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 20),
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

# FE models --------------------------------------------------------------------

### Panel regression by gender with interaction 

# women ------------------------------------------------------------------------
fixest::feols(same_lp ~ online + factor(age) |id, data = sample_f)
fixest::feglm(same_lp ~ online + factor(age) |id, data = sample_f,family = "binomial")


fixest::feols(distance_finish ~ online |id, data = sample_f)
fixest::feols(distance_finish ~ online + factor(age) |id, data = sample_f)

f_mod0 <- fixest::feols(distance_finish ~ gkpol_fct + age + online*year , data = sample_f)
f_mod1 <- fixest::feols(distance_finish ~ gkpol_fct + age + online*year | id, data = sample_f)
#f_mod1_split <- fixest::feols(distance_finish ~ gkpol_fct + age + online*year | id, data = sample_f, split = ~ isced_fct)


f_mod2 <- fixest::feols(distance_finish ~ gkpol_fct + age + online*year_fct | id, data = sample_f)
#f_mod2_split <- fixest::feols(distance_finish ~ gkpol_fct + age + online*year_fct | id, data = sample_f, split = ~ isced_fct)

class(df_finish$online)

# men --------------------------------------------------------------------------
fixest::feols(distance_finish ~ online|id , data = sample_m)

fixest::feols(distance_finish ~ online , data = sample_m %>% filter(wave==2))

m_mod0 <- fixest::feols(distance_finish ~ gkpol_fct + age + online*year , data = sample_m)
m_mod1 <- fixest::feols(distance_finish ~ gkpol_fct + age + online*year | id, data = sample_m)
#m_mod1_split <- fixest::feols(distance_finish ~ gkpol_fct + age + online*year | id, data = sample_m, split = ~ isced_fct)


m_mod2 <- fixest::feols(distance_finish ~ gkpol_fct + age + online*year_fct | id, data = sample_m)


## avg marginal effects -----
  marginaleffects::avg_slopes(f_mod1, variables = "online", by = "year")


### ame at year -----------
ame_at_year <- function(mod){
  
  if (any(grepl("year_fct",names(coefficients(mod))))) marginaleffects::avg_slopes(mod, variables = "online", by ="year_fct")
  if (any(grepl("year",    names(coefficients(mod))))) marginaleffects::avg_slopes(mod, variables = "online", by ="year")
}

fixest::feols(distance_finish ~ online*year + age| id, data = sample_f) %>% 
  ame_at_year()


## apply to models

ame_df <- 
    list(
         "F_pool"  = f_mod0,
         "F_fe"    = f_mod1,
         "F_fe.fct"= f_mod2,
         "M_pool"  = m_mod0,
         "M_fe"    = m_mod1,
         "M_fe.fct"= m_mod2
         ) |>
      map(ame_at_year) |>
      list_rbind(names_to = "model") |>
      separate_wider_delim(cols = model,delim = "_",names = c("gender","specification"))
   
  
# Plot ----
ame_plt <- 
  ame_df %>% 
    ggplot(aes(x=year,y=estimate, ymin= conf.low, ymax = conf.high, color = p.value < .05))  +
      geom_hline(aes(yintercept = 0), linetype = 2, color = "navy") +
      geom_errorbar() + 
      geom_point()+
      facet_grid(gender~specification) +
      scale_color_manual(values = c("grey40","orange")) + 
      scale_x_continuous(breaks = seq(2,11,2), labels = seq(2009,2017,2)) + 
      labs(color = "p value for difference vs. offline",
        x = "year", y = "average marginal effect") +
      theme(legend.position="bottom",
            strip.text.y = element_text(angle = 0,size=rel(2)),
            strip.text.x = element_text(angle = 0,size=rel(2)))
ame_plt                 
# ggsave("bb.png", width = 12, height = 12, units = "cm")