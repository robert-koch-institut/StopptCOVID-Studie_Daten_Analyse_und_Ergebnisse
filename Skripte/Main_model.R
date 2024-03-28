library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(lubridate)
library(ggtext)
library(multcomp)
library(RColorBrewer)

rm(list = ls())

source("Skripte/helper_main.R")


load(file = file.path("Daten",paste0("modeling_bl.rData")))

summary(modeling_bl$date)
summary(modeling_bl$per_vacc_1)

##  Data preparation ---------------

names_all <- names(modeling_bl %>% dplyr::select(contains("measures_")))

modeling_bl <- lag_data(clag = vlag, varn = c("per_vacc_1"), data = modeling_bl)
modeling_bl <- lag_data(clag = chosen_lag, varn = names_all, data = modeling_bl)

data_main <- finish_prep(data = modeling_bl)


summary(data_main$logR)
mean(data_main$N_onset_norm)






##  Main model --------------

names_npi <- names(data_main %>% dplyr::select(starts_with(params)))

formula_main <-
  gen_formula_bl_log(names_npi = names_npi,
                     vaccination = TRUE,
                     seasonality = TRUE,
                     offset = off)


main_model <-
  lm(formula = formula_main,
     data = data_main,
     weights = N_onset_norm)
summary(main_model)$sigma

model_noW0 <-
  lm(formula = formula_main,
     data = data_main)
summary(model_noW0)$sigma

# adjusted R²
r2_main <- summary(main_model)$r.squared


summary(main_model)$r.squared %>% print()
str(main_model$coefficients)
nrow(as.array(main_model$coefficients))
aic_main <- AIC(main_model) 
aic_main %>% print()
res_err_main <- summary(main_model)$sigma
main_model$deviance %>% print()


save(main_model, file = file.path("Daten",
                                  "main_model.rData"))




##  Coefficients model ----------------------

tidy_main_model <- tidy_model(main_model)

tidy_main_model %<>% dplyr::mutate(model = "Hauptmodell")

tidy_main_model %<>% mutate(var_e = factor(variable, 
                                      levels = rev(unique(tidy_main_model$variable))),
                       var_e2 = factor(var_e, 
                                       labels = label_model),
                       protect = (estimate < 1)
)

levels(tidy_main_model$var_f)

save(tidy_main_model, file = file.path("Daten","coef_main_model.rData"))




sink(file.path("output","coef_main_model.txt"))

r2_main %>% print()

aic_main %>% print()

tidy_main_model %>% dplyr::select(var_f, estimate, conf.low, conf.high) %>% print(n = 100)

tidy_main_model %>% dplyr::group_by(var_f) %>% dplyr::summarise(
  reduction = round(100*(1 - estimate), digits = 1), 
  lb = round(100*(1 - conf.high), digits = 1),
  ub = round(100*(1 - conf.low), digits = 1)
) %>% arrange(rev(var_f)) %>% print(n = 100)

sink()




ggplot(data = tidy_main_model, aes(y = var_f)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(0.9), color = wes_palette("Zissou1", 1)) + 
  geom_point(aes(x = estimate),
             position = position_dodge(0.9), color = wes_palette("Zissou1", 1)) +
  geom_hline(yintercept = lines_model, color = "gray80")  +
  geom_vline(aes(xintercept = 1), color = "black") +
  labs(x = "Effekt auf R-Wert", 
       y = "", 
       subtitle = bquote('Hauptmodell:'~R^{2}==.(round(r2_main*100, 1))*'%')) +
  themeMod(11) +
  theme(panel.grid.major.y = element_blank(),
        plot.caption=element_text(hjust = 0),
        plot.title.position = "plot")
ggsave(file.path("output",paste0("estimates_main_model.pdf")), 
       width = 9, height = 12, dpi = 1200)
ggsave(file.path("output",
                 paste0("estimates_main_model.png")), 
       width = 9, height = 12, dpi = 1200)





## Model for age groups ---------------

Impfung_unter18 <- 0
cross_vacc <- 0




res_ag <- list()
tm_bl_ag <- list()

for (a in 1:4) {
  
  if (a==1) ag = "0 bis 17 Jahre"
  if (a==2) ag = "18 bis 59 Jahre"
  if (a==3) ag = "60 und mehr Jahre"
  
  if (a<=3) {
    load(file = file.path("Daten",paste0("modeling_bl_ag.rData")))  
    modeling_bl <- modeling_bl_ag %>% dplyr::filter(AG == ag)
  }
  
  if (a==4) {
    load(file = file.path("Daten",paste0("modeling_bl.rData")))
  }
  
  
  modeling_bl <- lag_data(clag = vlag, varn = c("per_vacc_1"), data = modeling_bl)
  
  
  names_npi <- names(modeling_bl %>% dplyr::select(starts_with(params)))
  

  # lag für AG 1 um -1 erhöhen
  if (a %in% c(1, 4)) {
    chosen_lag = -1L    
    modeling_bl <- lag_data(clag = chosen_lag, varn = names_npi, data = modeling_bl)
  }
  
  # lag für AG 2 und 4 um -2 erhöhen
  if (a %in% c(3)) {
    
    chosen_lag = 0L
    modeling_bl <- lag_data(clag = chosen_lag, varn = names_npi, data = modeling_bl)
  }
  
  if (a %in% c(2)) {
    
    chosen_lag = -2L  
    modeling_bl <- lag_data(clag = chosen_lag, varn = names_npi, data = modeling_bl)
  }
  
  
  modeling_bl <- finish_prep(data = modeling_bl)
  mean(modeling_bl$N_onset_norm)
  
  
  names_npi <- names(modeling_bl %>% dplyr::select(  starts_with(params)))
  
  
  if (Impfung_unter18 == 0)  vacc <- (a > 1)
  if (Impfung_unter18 == 1)  vacc <- T
  
  formula_bl <-
    gen_formula_bl_log(names_npi = names_npi,
                       cross_vacc = cross_vacc,
                       vaccination = vacc,
                       seasonality = TRUE,
                       offset = off)
  formula_bl
  
  model_ag <-  lm(formula = formula_bl,
                  data = modeling_bl, 
                  weights = N_onset_norm)
  
  res_ag[[a]] <- modeling_bl %>% dplyr::select(BL, date, R, logR, Alpha, Delta, 
                                               per_vacc_1, per_vacc_2, N_smooth, Bev)
  res_ag[[a]]$predict <- predict(model_ag, newdata = modeling_bl)
  res_ag[[a]]$se_predict <- predict(model_ag, newdata = modeling_bl, se.fit = T, 
                                    intervall = c("prediction"))$se.fit
  res_ag[[a]] %<>% dplyr::mutate(lb_predict = predict - 1.96 * se_predict,
                                 ub_predict = predict + 1.96 * se_predict)
  
  
  tm_bl_ag[[a]] <- model_fit_ag(data_bl = modeling_bl, params = params, 
                                vacc = vacc, vlag = vlag, cross_vacc = cross_vacc, 
                                spatial = "BL",
                                offset = off)
}



results_ag <- bind_rows(res_ag[[1]] %>% 
                          dplyr::mutate(AG = "0 bis 17 Jahre"),
                        res_ag[[2]] %>% 
                          dplyr::mutate(AG = "18 bis 59 Jahre"),
                        res_ag[[3]] %>% 
                          dplyr::mutate(AG = "60 und mehr Jahre"),
                        res_ag[[4]] %>% 
                          dplyr::mutate(AG = "alle Altersgruppen"))

save(results_ag, file = file.path("Daten",
                                  paste0("results_main_model_ag.rData")))



tm_comp <- bind_rows(tm_bl_ag[[4]] %>% 
                       dplyr::mutate(AG = "alle Altersgruppen"),
                     tm_bl_ag[[1]] %>% 
                       dplyr::mutate(AG = "0 bis 17 Jahre"),
                     tm_bl_ag[[2]] %>% 
                       dplyr::mutate(AG = "18 bis 59 Jahre"),
                     tm_bl_ag[[3]] %>% 
                       dplyr::mutate(AG = "60 und mehr Jahre"))

tm_comp %<>% mutate(var_e = factor(variable, 
                                   levels = rev(unique(tm_comp$variable))),
                    var_e2 = factor(var_e, levels = levels(var_e),
                                    labels = label_model),
                    AG_f = factor(AG, levels = c("0 bis 17 Jahre", "18 bis 59 Jahre", 
                                                 "60 und mehr Jahre", "alle Altersgruppen")),
                    protect = (estimate < 1),
)





rev(levels(tm_comp$var_f))



ggplot(data = tm_comp %>% dplyr::filter(variable != "(Intercept)"), 
       aes(y = var_e2)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = AG_f),
                 position = position_dodge(0.9)) + 
  geom_point(aes(x = estimate, color = AG_f),
             position = position_dodge(0.9)) +
  geom_hline(yintercept = lines_model, color = "gray80")  +
  geom_vline(aes(xintercept = 1), color = "black") +
  themeMod(11) +
  theme(panel.grid.major.y = element_blank(),
        plot.title.position = "plot") +
  labs(y = "", x = "Effekt auf R-Wert") +
  scale_color_manual(
    values = rev(wes_palette("Zissou1", 
                             length(unique(tm_comp$AG_f)), 
                             type = "continuous")),
    breaks = c("0 bis 17 Jahre", "18 bis 59 Jahre", "60 und mehr Jahre", "alle Altersgruppen"),
    name = "Altersgruppe") +
  scale_x_continuous(breaks = c(seq(.7, 1.3, by = .1)))
ggsave(file.path("output",
                 paste0("estimates_main_model_AG.pdf")), 
       width = 9, height = 12, dpi = 1200)
ggsave(file.path("output",
                 paste0("estimates_main_model_AG.png")), 
       width = 9, height = 12, dpi = 1200)


## ranking ---------

ranking <- tm_comp %>% dplyr::arrange(AG_f, estimate) %>% 
  dplyr::filter(variable!=gsub("measures", "", variable) & 
                  variable==gsub("test", "", variable)) %>% 
  dplyr::select(AG_f, var_e2, estimate, conf.low, conf.high) %>%
  dplyr::arrange(AG_f, estimate)

ranking %>% dplyr::filter(AG_f=="0 bis 17 Jahre") %>% print(n = 42*4)

write.csv2(ranking, file = file.path("output",
                                     paste0("ranking_main_model_AG.csv")))






p <- list()
for (a in 1:4) {
  
  
  if (a==1) {
    ag = "0 bis 17 Jahre"
  }
  if (a==2) {
    ag = "18 bis 59 Jahre"
  }
  if (a==3) {
    ag = "60 und mehr Jahre"
  }
  if (a==4) {
    ag = "alle Altersgruppen"
  }
  df <- ranking %>% dplyr::filter(AG_f == ag) %>%
    dplyr::arrange(AG_f, estimate) %>%
    dplyr::mutate(var = as.character(var_e2))
  
  df %<>% dplyr::mutate(var_r = factor(var, levels = rev(unique(df$var)) )) 
  levels(df$var_r)
  
  p[[a]] <- ggplot(data = df, 
                   aes(y = var_r)) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = AG_f),
                   position = position_dodge(0.9)) + 
    geom_point(aes(x = estimate, color = AG_f),
               position = position_dodge(0.9)) +
    geom_vline(aes(xintercept = 1), color = "black") +
    themeMod(11) +
    theme(panel.grid.major.y = element_blank(),
          plot.title.position = "plot") +
    labs(y = "", x = "Effect on R-value", 
         subtitle = paste0("Altersgruppe ", ag)) +
    scale_color_manual(
      values = rev(wes_palette("Zissou1", 
                               4, 
                               type = "continuous")),
      breaks = c("0 bis 17 Jahre", "18 bis 59 Jahre", "60 und mehr Jahre", "alle Altersgruppen"),
      name = "Altersgruppe",
      guide= "none") +
    scale_x_continuous(breaks = seq(0.5, 1.3, by = 0.1),
                       limits = c(0.5, 1.3))
}  



cowplot::plot_grid(p[[1]], p[[2]], p[[3]], p[[4]], labels = "", nrow = 2)  
ggsave(file.path("output",paste0("ranking_main_model_AG.pdf")), 
       width = 9, height = 12, dpi = 1200)
ggsave(file.path("output",paste0("ranking_main_model_AG.png")), 
       width = 9, height = 12, dpi = 1200)


