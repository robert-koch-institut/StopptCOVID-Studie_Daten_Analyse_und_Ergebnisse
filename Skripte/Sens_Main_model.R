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


load(file = file.path("Daten", paste0("modeling_bl.rData")))

names_all <- names(modeling_bl %>% dplyr::select(contains("measures_")))

modeling_bl <- lag_data(clag = vlag, varn = c("per_vacc_1"), data = modeling_bl)
modeling_bl <- lag_data(clag = chosen_lag, varn = names_all, data = modeling_bl)

data_main <- finish_prep(data = modeling_bl)

names_npi <- names(modeling_bl %>% dplyr::select(starts_with(params)))

summary(data_main$logR)
mean(data_main$N_onset_norm)



##  Main model --------------


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
aic_main <- AIC(main_model) 

tidy_main_model <- tidy_model(main_model)


##  Gamma model --------------

formula_gamma <-
  gen_formula_bl(names_npi = names_npi,
                      vaccination = TRUE,
                      seasonality = TRUE,
                      offset = off)

model_gamma <-
  glm(formula = formula_gamma,
      family = Gamma(link = "log"),
      data = data_main,
      weights = N_onset_norm)

summary(model_gamma)
# adjusted R²
r2_gamma <- summary(model_gamma)$r.squared
# R²
summary(model_gamma)$r.squared
disp_gamma <- summary(model_gamma)$dispersion
str(model_gamma$coefficients)
nrow(as.array(model_gamma$coefficients))
aic_gamma <- AIC(model_gamma)
model_gamma$deviance
tidy_model_gamma <- tidy_model(model_gamma)


##  Model Alpha and Delta --------------

formula_bl <-
  gen_formula_bl_log(names_npi = names_npi,
                          vaccination = TRUE,
                          seasonality = TRUE,
                          offset = "No")

model_bl <-
  lm(formula = formula_bl,
     data = data_main,
     weights = N_onset_norm
  )

# adjusted R²
r2_AD <- summary(model_bl)$r.squared
# R²
summary(model_bl)$adj.r.squared
summary(model_bl)$r.squared
str(model_bl$coefficients)
nrow(as.array(model_bl$coefficients))
aic_AD <- AIC(model_bl)
model_bl$deviance
model_AD <- tidy_model(model_bl)



##  Unweighted model --------------

formula_bl <-
  gen_formula_bl_log(names_npi = names_npi,
                          vaccination = TRUE,
                          seasonality = TRUE,
                          offset = off)

model_bl <-
  lm(formula = formula_bl,
     data = data_main
  )

# adjusted R²
r2_noW <- summary(model_bl)$r.squared
# R²
summary(model_bl)$r.squared
str(model_bl$coefficients)
nrow(as.array(model_bl$coefficients))
aic_noW <- AIC(model_bl)
model_bl$deviance
model_noW <- tidy_model(model_bl)



##  Model for different time periods ----------

aic_waves <- list()
r2_waves <- list()
model_waves <- list()


for (j in 1:4) {

  names_npi <- names(data_main %>% dplyr::select(starts_with(params)))
  if (j == 1) names_npi <- names_npi[!names_npi =="services_measures_02"]
  
  
  vacc <- dates[[j]] > as.Date("2021/03/01", "%Y/%m/%d")
  season <- dates[[j]] > as.Date("2020/09/01", "%Y/%m/%d")
  
  formula_bl <-
    gen_formula_bl_log(names_npi = names_npi,
                            vaccination = vacc,
                            seasonality = season,
                            offset = off)

  model_bl <-
    lm(formula = formula_bl,
       data = data_main %>% dplyr::filter(date < dates[[j]]),
       weights = N_onset_norm)
  
  # adjusted R²
  r2_waves[[j]] <- summary(model_bl)$r.squared
  # R²
  summary(model_bl)$r.squared
  nrow(as.array(model_bl$coefficients))
  aic_waves[[j]] <- AIC(model_bl)
  model_waves[[j]] <- tidy_model(model_bl)
}




##  (2.1) Evaluate Model ----------------------



main_model <- bind_rows(tidy_main_model %>% 
                          dplyr::mutate(model = "Hauptmodell"))

main_model %<>% mutate(var_e = factor(variable, 
                                      levels = rev(unique(main_model$variable))),
                       var_f = factor(var_e, 
                                      labels = label_model),
                       protect = (estimate < 1)
)

levels(main_model$var_f)



# exit

sink(file.path("output", "main_model_npi_vaccine_effect.txt"))

r2_main %>% print()

aic_main %>% print()

main_model %>% dplyr::select(var_f, estimate, conf.low, conf.high) %>% print(n = 100)

main_model %>% dplyr::group_by(var_f) %>% dplyr::summarise(
  reduction = round(100*(1 - estimate), digits = 1), 
  lb = round(100*(1 - conf.high), digits = 1),
  ub = round(100*(1 - conf.low), digits = 1)
) %>% arrange(rev(var_f)) %>% print(n = 100)

sink()

# exit

ggplot(data = main_model, aes(y = var_f)) +
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
ggsave(file.path("output", paste0("estimates_model_all.pdf")), 
       width = 11, height = 15, dpi = 1200)
ggsave(file.path("output", 
                 paste0("estimates_model_all.png")), 
       width = 11, height = 15, dpi = 1200)


# exit

## (2.2) Gamma Model  -------------------

model_comp <- bind_rows(tidy_model_gamma %>% 
                          dplyr::mutate(model = "Gamma-Modell"),
                        tidy_main_model %>% 
                          dplyr::mutate(model = "Hauptmodell"))

length(tidy_model_gamma$variable)
length(unique(tidy_main_model$variable))
length(unique(model_comp$variable))

model_comp %<>% mutate(var_e = factor(variable, 
                                      levels = c(rev(unique(tidy_main_model$variable)))),
                       var_f = factor(var_e, 
                                      labels = c(label_model)),
                       model_f = factor(model, levels = c("Gamma-Modell", "Hauptmodell")),
                       protect = (estimate < 1)
)

length(levels(model_comp$var_e))
length(unique(model_comp$var_e))
length(rev(unique(tidy_main_model$variable)))
levels(model_comp$var_f)

ggplot(data = model_comp, aes(y = var_f)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = model_f),
                 position = position_dodge(0.9)) + 
  geom_point(aes(x = estimate, color = model_f),
             position = position_dodge(0.9)) +
  geom_vline(aes(xintercept = 1), color = "black") +
  labs(x = "Effekt auf R-Wert", y = "") +
  themeMod(11) +
  scale_color_manual(
    values = rev(wes_palette("Zissou1", 
                             length(unique(model_comp$model_f)), type = "continuous")),
    breaks = c("Gamma-Modell", "Hauptmodell"),
    name = "Modell")
ggsave(file.path("output", paste0("estimates_model_gamma.pdf")), 
       width = 11, height = 15, dpi = 1200)
ggsave(file.path("output", paste0("estimates_model_gamma.png")), 
       width = 11, height = 15, dpi = 1200)
ggsave(file.path("output", paste0("estimates_model_gamma.emf")), 
       width = 11, height = 15, dpi = 1200)




## (2.3) all vs. offset  -------------------

model_comp <- bind_rows(tidy_main_model %>% 
                          dplyr::mutate(model = "Hauptmodell"),
                        model_AD %>% 
                          dplyr::mutate(model = "Modell mit\nAlpha, Delta"))

length(unique(tidy_main_model$variable))
length(unique(model_AD$variable))
length(unique(model_comp$variable))

model_comp %<>% mutate(var_e = factor(variable, 
                                      levels = c("Delta", rev(unique(tidy_main_model$variable)))),
                       var_f = factor(var_e, 
                                      labels = c("Delta", label_model)),
                       model_f = factor(model, levels = c("Modell mit\nAlpha, Delta", "Hauptmodell")),
                       protect = (estimate < 1)
)

length(levels(model_comp$var_f))

ggplot(data = model_comp, aes(y = var_f)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = model_f),
                 position = position_dodge(0.9)) + 
  geom_point(aes(x = estimate, color = model_f),
             position = position_dodge(0.9)) +
  geom_hline(yintercept = c(2.5, 2 + lines_model), color = "gray80")  +
  geom_vline(aes(xintercept = 1), color = "black") +
  labs(x = "Effekt auf R-Wert", y = "",
       subtitle = bquote('Hauptmodell:'~R^{2}==.(round(r2_main*100, 1))*'%,'
                         ~'Modell mit Alpha, Delta:'~R^{2}==.(round(r2_AD*100, 1))*'%'
       )) +
  themeMod(11) +
  theme(panel.grid.major.y = element_blank(),
        plot.title.position = "plot") +
  scale_color_manual(
    values = rev(wes_palette("Zissou1", 
                             length(unique(model_comp$model_f)), type = "continuous")),
    breaks = c("Modell mit\nAlpha, Delta", "Hauptmodell"),
    name = "Modell") +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5))
ggsave(file.path("output", paste0("estimates_model_offset.pdf")), 
       width = 11, height = 15, dpi = 1200)
ggsave(file.path("output", paste0("estimates_model_offset.png")), 
       width = 11, height = 15, dpi = 1200)
ggsave(file.path("output", paste0("estimates_model_offset.emf")), 
       width = 11, height = 15, dpi = 1200)


ggplot(data = model_comp, aes(y = var_f)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = model_f),
                 position = position_dodge(0.9)) + 
  geom_point(aes(x = estimate, color = model_f),
             position = position_dodge(0.9)) +
  geom_hline(yintercept = c(2.5, 2 + lines_model), color = "gray80")  +
  geom_vline(aes(xintercept = 1), color = "black") +
  labs(x = "Effekt auf R-Wert", y = "",
       subtitle = bquote('Hauptmodell:'~R^{2}==.(round(r2_main*100, 1))*'%,'
                         ~'Modell mit Alpha, Delta:'~R^{2}==.(round(r2_AD*100, 1))*'%'
       )) +
  themeMod(11) +
  theme(panel.grid.major.y = element_blank(),
        plot.title.position = "plot") +
  scale_color_manual(
    values = rev(wes_palette("Zissou1", 
                             length(unique(model_comp$model_f)), type = "continuous")),
    breaks = c("Modell mit\nAlpha, Delta", "Hauptmodell"),
    name = "Modell") +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5), trans='log')
ggsave(file.path("output", paste0("estimates_model_offset_log.pdf")), 
       width = 11, height = 15, dpi = 1200)
ggsave(file.path("output", paste0("estimates_model_offset_log.png")), 
       width = 11, height = 15, dpi = 1200)




ggplot(data = model_comp %>% dplyr::filter(var_f != "Alpha" & var_f != "Delta"), aes(y = var_f)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = model_f),
                 position = position_dodge(0.9)) + 
  geom_point(aes(x = estimate, color = model_f),
             position = position_dodge(0.9)) +
  geom_hline(yintercept = lines_model, color = "gray80")  +
  geom_vline(aes(xintercept = 1), color = "black") +
  labs(x = "Effekt auf R-Wert", y = "",
       subtitle = bquote('Hauptmodell:'~R^{2}==.(round(r2_main*100, 1))*'%,'
                         ~'Modell mit Alpha, Delta:'~R^{2}==.(round(r2_AD*100, 1))*'%'
       )) +
  themeMod(11) +
  theme(panel.grid.major.y = element_blank(),
        plot.title.position = "plot") +
  scale_color_manual(
    values = rev(wes_palette("Zissou1", 
                             length(unique(model_comp$model_f)), type = "continuous")),
    breaks = c("Modell mit\nAlpha, Delta", "Hauptmodell"),
    name = "Modell") +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5))
ggsave(file.path("output", paste0("estimates_model_offset_oAD.pdf")), 
       width = 11, height = 15, dpi = 1200)
ggsave(file.path("output", paste0("estimates_model_offset_oAD.png")), 
       width = 11, height = 15, dpi = 1200)
ggsave(file.path("output", paste0("estimates_model_offset_oAD.emf")), 
       width = 11, height = 15, dpi = 1200)


## (2.4) all vs. unweighted  -------------------

model_comp <- bind_rows(tidy_main_model %>% 
                          dplyr::mutate(model = "Hauptmodell"),
                        model_noW %>% 
                          dplyr::mutate(model = "Modell\nohne Gewichtung"))

length(unique(tidy_main_model$variable))
length(unique(model_noW$variable))
length(unique(model_comp$variable))

model_comp %<>% mutate(var_e = factor(variable, 
                                      levels = rev(unique(model_comp$variable))),
                       var_f = factor(var_e, 
                                      labels = label_model),
                       model_f = factor(model, levels = c("Modell\nohne Gewichtung", "Hauptmodell")),
                       protect = (estimate < 1)
)

length(levels(model_comp$var_f))

ggplot(data = model_comp, aes(y = var_f)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = model_f),
                 position = position_dodge(0.9)) + 
  geom_point(aes(x = estimate, color = model_f),
             position = position_dodge(0.9)) +
  geom_hline(yintercept = lines_model, color = "gray80")  +
  geom_vline(aes(xintercept = 1), color = "black") +
  labs(x = "Effekt auf R-Wert", y = "",
       subtitle = bquote('Hauptmodell:'~R^{2}==.(round(r2_main*100, 1))*'%,'
                         ~'Modell ohne Gewichtung:'~R^{2}==.(round(r2_noW*100, 1))*'%'
       )) +
  themeMod(11) +
  theme(panel.grid.major.y = element_blank(),
        plot.title.position = "plot") +
  scale_color_manual(
    values = rev(wes_palette("Zissou1", 
                             length(unique(model_comp$model_f)), type = "continuous")),
    breaks = c("Modell\nohne Gewichtung", "Hauptmodell"),
    name = "Modell") +
  scale_x_continuous(breaks = seq(0, 2, by = 0.1))
ggsave(file.path("output", paste0("estimates_model_noW.pdf")), 
       width = 11, height = 15, dpi = 1200)
ggsave(file.path("output", paste0("estimates_model_noW.png")), 
       width = 11, height = 15, dpi = 1200)
ggsave(file.path("output", paste0("estimates_model_noW.emf")), 
       width = 11, height = 15, dpi = 1200)



## (2.5) all vs. waves  -------------------

model_comp <- bind_rows(tidy_main_model %>% 
                          dplyr::mutate(model = "Hauptmodell"),
                        model_waves[[1]] %>% 
                          dplyr::mutate(model = "Zeitraum 1"),
                        model_waves[[2]] %>% 
                          dplyr::mutate(model = "Zeitraum 2"),
                        model_waves[[3]] %>% 
                          dplyr::mutate(model = "Zeitraum 3"),
                        model_waves[[4]] %>% 
                          dplyr::mutate(model = "Zeitraum 4"))

length(unique(tidy_main_model$variable))
length(unique(model_waves[[1]]$variable))
length(unique(model_comp$variable))

model_comp %<>% mutate(var_e = factor(variable, 
                                      levels = rev(unique(model_comp$variable))),
                       var_f = factor(var_e, 
                                      labels = label_model),
                       model_f = factor(model, levels = rev(c("Zeitraum 1",
                                                              "Zeitraum 2",
                                                              "Zeitraum 3",
                                                              "Zeitraum 4", 
                                                              "Hauptmodell"))),
                       protect = (estimate < 1)
)

with(model_comp, table(model, model_f))

length(levels(model_comp$var_f))

ggplot(data = model_comp, aes(y = var_f)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = model_f),
                 position = position_dodge(0.9)) + 
  geom_point(aes(x = estimate, color = model_f),
             position = position_dodge(0.9)) +
  geom_hline(yintercept = lines_model, color = "gray80")  +
  geom_vline(aes(xintercept = 1), color = "black") +
  labs(x = "Effekt auf R-Wert", y = "",
       subtitle = bquote(atop('Hauptmodell:'~R^{2}==.(round(r2_main*100, 1))*'%,'
                              ~'Zeitraum 4:'~R^{2}==.(round(r2_waves[[4]]*100, 1))*'%,'
                              ~'Zeitraum 3:'~R^{2}==.(round(r2_waves[[3]]*100, 1))*'%,',
                              ~'Zeitraum 2:'~R^{2}==.(round(r2_waves[[2]]*100, 1))*'%,'
                              ~'Zeitraum 1:'~R^{2}==.(round(r2_waves[[1]]*100, 1))*'%,')
       )) +
  themeMod(11) +
  theme(panel.grid.major.y = element_blank(),
        plot.title.position = "plot") +
  scale_color_manual(values = (wes_palette("Darjeeling1", 5, type = "continuous")
  ),
  breaks = c("Zeitraum 1",
             "Zeitraum 2",
             "Zeitraum 3",
             "Zeitraum 4", 
             "Hauptmodell"),
  name = "Modell") +
  scale_x_continuous(breaks = seq(0, 2, by = 0.1))
#, limits = c(0.5, 2))
ggsave(file.path("output", paste0("estimates_model_waves.pdf")), 
       width = 11, height = 15, dpi = 1200)
ggsave(file.path("output", paste0("estimates_model_waves.png")), 
       width = 11, height = 15, dpi = 1200)
ggsave(file.path("output", paste0("estimates_model_waves.emf")), 
       width = 11, height = 15, dpi = 1200)



model_comp %<>% mutate(var_e = factor(variable, 
                                      levels = rev(unique(model_comp$variable))),
                       var_f = factor(var_e, 
                                      labels = label_model),
                       model_f = factor(model, levels = rev(c("Zeitraum 2",
                                                              "Zeitraum 3",
                                                              "Zeitraum 4", 
                                                              "Hauptmodell"))),
                       protect = (estimate < 1)
)

with(model_comp, table(model, model_f))

ggplot(data = model_comp %>% dplyr::filter(model != "Zeitraum 1"), aes(y = var_f)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = model_f),
                 position = position_dodge(0.9)) + 
  geom_point(aes(x = estimate, color = model_f),
             position = position_dodge(0.9)) +
  geom_hline(yintercept = lines_model, color = "gray80")  +
  geom_vline(aes(xintercept = 1), color = "black") +
  labs(x = "Effekt auf R-Wert", y = "",
       subtitle = bquote(atop('Hauptmodell:'~R^{2}==.(round(r2_main*100, 1))*'%,'
                              ~'Zeitraum 4:'~R^{2}==.(round(r2_waves[[4]]*100, 1))*'%,'
                              ~'Zeitraum 3:'~R^{2}==.(round(r2_waves[[3]]*100, 1))*'%,',
                              ~'Zeitraum 2:'~R^{2}==.(round(r2_waves[[2]]*100, 1))*'%,')
       )) +
  themeMod(11) +
  theme(panel.grid.major.y = element_blank(),
        plot.title.position = "plot") +
  scale_color_manual(
    values = (rev(wes_palette("Darjeeling1", 5, type = "continuous"))[2:5]),
    breaks = c("Zeitraum 2",
               "Zeitraum 3",
               "Zeitraum 4", 
               "Hauptmodell"),
    name = "Modell") +
  scale_x_continuous(breaks = seq(0, 2, by = 0.1))
#, limits = c(0.5, 2))
ggsave(file.path("output", paste0("estimates_model_waves2.pdf")), 
       width = 11, height = 15, dpi = 1200)
ggsave(file.path("output", paste0("estimates_model_waves2.png")), 
       width = 11, height = 15, dpi = 1200)
ggsave(file.path("output", paste0("estimates_model_waves2.emf")), 
       width = 11, height = 15, dpi = 1200)




