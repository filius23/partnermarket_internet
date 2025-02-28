# load packages ----------------------------------------------------------------
pacman::p_load("tidyverse", "marginaleffects", "systemfonts", 
               "modelsummary", "flextable", "fixest")


# load data --------------------------------------------------------------------
df_finish <- readRDS("./data/df_finish.RDS")


# separate data sets by gender -------------------------------------------------
sample_f <- df_finish[df_finish$female=="1",]
sample_m <- df_finish[df_finish$female=="0",]



# Modellschätzung und Berechnung der Predictions
#ame_m4 <- marginaleffects::avg_slopes(m_f)

### Frauen
m_f <- fixest::feols(distance_finish ~ online + isced_fct + gkpol_fct + cohort | id,
                     data = sample_f) 

preds_f <- avg_predictions(m_f, by = "online")
head(preds_f)

head(data.frame(preds_f))
preds_f$rowid <- "Female"

### Männer
m_m <- fixest::feols(distance_finish ~ online + isced_fct + gkpol_fct + cohort | id,
                     data = sample_m) 

preds_m <- avg_predictions(m_m, by = "online")
head(preds_m)

head(data.frame(preds_m))
preds_m$rowid <- "Male"

#pred_fe_m2$group <- as.numeric(as.character(pred_fe_m2$group))


preds_all <- bind_rows(preds_f, preds_m)
plot_predictions(preds_m, condition = "online")

# Visualisierung der Average Marginal Effects
plot_preds <- preds_all %>%
  ggplot(aes(x = as.factor(online), y = estimate, color = rowid)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "blue") +
  labs(title = "Vorhergesagte Distanz zum Partner nach Kennenlernort",
       x = "Online-Dating (0 = Offline, 1 = Online)",
       y = "Vorhergesagte Distanz") +
  theme_minimal()
