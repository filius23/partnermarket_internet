# load packages ----------------------------------------------------------------
pacman::p_load("tidyverse", "texreg", "psych", "ggplot2", "modelsummary", "flextable", "officer", "data.table")

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




### t-test bivariate differences -----------------------------------------------

ttest <- t.test(distance_finish~factor(online), data=df_finish,
                alternative="two.sided")
ttest


library(broom)
Tidy <- broom::tidy(ttest)
class(Tidy)

Tidy1 <- data.frame(Tidy) %>% flextable() %>% 
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>% 
  colformat_double(decimal.mark=",",big.mark = "", digits = 1) %>% 
  autofit()








### tables ---------------------------------------------------------------------

### table 1: Sample description ------------------------------------------------
# metric vars: year, age, destanz_finish
metrische_uV <- df_finish %>%
  select(distance = distance_finish, year, age) 

desc <- psych::describe(metrische_uV,skew = F, omit = T) %>% 
  select(Mean=mean,SD=sd,Min=min,Max=max)

desc$vars <- row.names(desc) # Variablen aus Zeilenname

tab1_metrisch <- data.frame(desc) %>%  
  select(vars,Mean,SD,Min,Max) %>% 
  mutate(Mean = round(Mean, 2), 
         SD = round(SD, 2)) %>%
  flextable() %>%  
  autofit() 

#categorial vars: online, cohort, female ,isced_fct, migrationstatus, gkpol_fct
table(df_finish$migstatus)


df_finish$female <- factor(df_finish$female, levels = 0:1, 
                             labels = c("men","women"))
#df_finish$migstatus_lab <- factor(df_finish$migstatus, 
#                                  levels = 0:1, 
#                                  labels = c("no migration background","migration background"))
df_finish$online <- factor(df_finish$online, levels = 0:1, 
                         labels = c("offline","online"))


cat1 <- round(prop.table(table(df_finish$female)),2) %>% 
  data.frame(.) %>% filter(Var1=="women")
#cat2 <- round(prop.table(table(df_finish$migstatus_lab)),2) %>% 
#  data.frame(.) %>% filter(Var1=="migration background")
cat3 <- round(prop.table(table(df_finish$online)),2) %>% 
  data.frame(.) %>% filter(Var1=="online")

cat4 <- round(prop.table(table(df_finish$cohort)),2) %>% 
  data.frame(.)
cat5 <- round(prop.table(table(df_finish$isced_fct)),2) %>% 
  data.frame(.)
cat6 <- round(prop.table(table(df_finish$gkpol_fct)),2) %>% 
  data.frame(.)


tab1_cat <- flextable(bind_rows(cat3,cat1,cat4,cat5,cat6)) %>% #cat2,
  prepend_chunks(part="body",j = "Var1", i = 6, 
                 value = as_paragraph(as_i("Education \n"))) %>% 
  prepend_chunks(part="body",j = "Var1", i = 9, 
                 value = as_paragraph(as_i("City size \n"))) %>% 
  autofit() 



### To do: die beiden Tabellen müssen zu einer zusammengefügt werden



#---------------------


########################## table 2: temporal distances for the online and offline encounter context
distance2 <- describeBy(df_finish$distance_finish, df_finish$online,mat = T)
distance2
data.frame(distance2) %>% select(-kurtosis,-mad,-skew,-trimmed,-min,-range,-vars,-item,-max, -se) %>% flextable() %>% 
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>% 
  colformat_double(decimal.mark=",",big.mark = "", digits = 1,j=-2) %>% 
  colformat_double(decimal.mark=",",big.mark = "", digits = 0,j=2) %>% 
  autofit()


########################## table 3: temporal distances for the size of the place of residence
distance3 <- describeBy(df_finish$distance_finish, df_finish$gkpol_kat1,mat = T)
distance3
data.frame(distance3) %>% select(-n,-kurtosis,-mad,-skew,-trimmed,-min,-range,-vars,-item,-max,-se) %>% flextable() %>% 
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>% 
  colformat_double(decimal.mark=",",big.mark = "", digits = 1,j=-2) %>% 
  colformat_double(decimal.mark=",",big.mark = "", digits = 0,j=2) %>% 
  autofit()


########################## table 4: temporal distances for the education level
distance4 <- describeBy(df_finish$distance_finish, df_finish$isced_2,mat = T)
distance4
data.frame(distance4) %>% select(-n,-kurtosis,-mad,-skew,-trimmed,-min,-range,-vars,-item,-max,-se) %>% rename(Education = group1) %>% 
  flextable() %>% 
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part="header") %>% 
  colformat_double(decimal.mark=",",big.mark = "", digits = 1,j=-2) %>% 
  colformat_double(decimal.mark=",",big.mark = "", digits = 0,j=2) %>% 
  autofit()
