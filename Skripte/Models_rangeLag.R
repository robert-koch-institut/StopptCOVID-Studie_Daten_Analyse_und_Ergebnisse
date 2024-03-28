library(dplyr)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(lubridate)
library(zoo)
library(data.table)
library(multcomp)
library(stringr)

rm(list = ls())

source("Skripte/helper_main.R")

min <- -3L
max <- 3L

##  Fitting LM for different lags of NPI impact  -----------------

load(file = file.path("Daten", paste0("modeling_bl.rData")))  

modeling_bl <- lag_data(clag = vlag, varn = c("per_vacc_1"), data = modeling_bl)


## (1) Main model ------------------------


num_op <- list()
if (length(num_op) >0) { op = params[num_op] } else op = list()
other = params[!(params %in% op)]

length(op)
length(other)


  tm <- list()
  df <- list()
  for (olag in min:max) {
    
    cat( olag )
    # Sys.sleep(0.01)
    # cat("\014")
    
    ind <- olag + 1L -min
    
    fit <- model_fit_lag_tidy(vlag = vlag, chosen_lag = l, data_bl = modeling_bl, 
                              other = other, op = op, olag = olag,
                              offset = off)
    tm[[ind]] <- fit$tidy
    tm[[ind]] %<>% dplyr::mutate(lag = olag)
    
    df[[ind]] <- fit$model
    
    rm(fit)
    
  }
  
tm_df <- bind_rows(tm)
(max - min +1) * nrow(tm[[1]])




###  Plot results -------------

tm_df %<>% dplyr::mutate(lag_f = factor(lag))
tm_df %<>% mutate(var_e = factor(variable, 
                                      levels = rev(unique(tm_df$variable))),
                       var_e2 = factor(var_e, 
                                       labels = label_model),
                       protect = (estimate < 1))





ggplot(data = tm_df, aes(y = var_e2)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = lag_f),
                 position = position_dodge(0.9)) + 
  geom_point(aes(x = estimate, color = lag_f),
             position = position_dodge(0.9)) +
  geom_hline(yintercept = lines_model, color = "gray80")  +
  geom_vline(aes(xintercept = 1), color = "black") +
  labs(x = "Effect on R-value", y = "") +
  themeMod(9) +
  theme(panel.grid.major.y = element_blank(),
        plot.title.position = "plot") +
  scale_color_manual(
    values = rev(wes_palette("FantasticFox1", max-min+1, type = "continuous")),
    breaks = levels(tm_df$lag_f),
    name = "Chosen delay") +
  scale_x_continuous(breaks = seq(0, 2, by = 0.1))

ggsave(file.path("output", paste0("estimates_main_model_rangeLag.pdf")), 
       width = 9, height = 12, dpi = 1200)
ggsave(file.path("output", paste0("estimates_main_model_rangeLag.png")), 
       width = 9, height = 12, dpi = 1200)


## nur : daycare Stufe 2 und 4, public space measures Stufe 2 und 5. 

tm_df_short <- tm_df %>% dplyr::filter(stringr::str_detect(var_f, 
                      'daycare_measures_02|daycare_measures_04|public_space_measures_02|public_space_measures_05'))

ggplot(data = tm_df_short, aes(y = var_e2)) +
  geom_errorbarh(aes(xmax = 1-conf.low, xmin = 1-conf.high, color = lag_f),
                 position = position_dodge(0.9)) + 
  geom_point(aes(x = 1-estimate, color = lag_f),
             position = position_dodge(0.9)) +
  geom_hline(yintercept = c(2.5, 4.5), color = "gray80")  +
#  geom_vline(aes(xintercept = 1), color = "black") +
  labs(x = "Reduction of R-value", y = "") +
  themeMod(11) +
  theme(panel.grid.major.y = element_blank(),
        plot.title.position = "plot") +
  scale_color_manual(
    values = rev(wes_palette("FantasticFox1", max-min+1, type = "continuous")),
    breaks = rev(levels(tm_df$lag_f)),
    name = "Chosen delay") +
  scale_x_continuous(breaks = seq(0, 2, by = 0.1), limits=c(0, .5))
#, limits = c(0.5, 2))
ggsave(file.path("output", paste0("estimates_main_model_rangeLag_short.pdf")), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", paste0("estimates_main_model_rangeLag_short.png")), 
       width = 11, height = 5, dpi = 1200)



rangeLag <- tm_df %>% dplyr::arrange(lag_f, estimate) %>% 
  dplyr::filter(variable!=gsub("measures", "", variable) & 
                variable==gsub("test", "", variable)) %>% 
  dplyr::select(lag_f, term, estimate, conf.low, conf.high) %>%
  dplyr::arrange(lag_f, estimate)

write.csv2(rangeLag, file = file.path("output", "coef_main_model_rangeLag.csv"))




## (2) Coarser model ------------------------

params2 <- c(
  "schools_measures",
  "private_space_measures",
  "workplace_measures",
  "daycare_measures",
  "public_space_measures",
  "public_event_measures",
  "curfew_measures",
#  "retail_measures",
  "nightlife_measures",
#  "services_measures",
  "ceshgrs_max_measures",
  "ceshgrs_max2_measures",
  "test_measures",
  "abstand_measures",
  "mask_measures"
) 
length(params2)


num_op <- list()
if (length(num_op) >0) { op = params[num_op] } else op = list()
other = params2[!(params2 %in% op)]

length(op)
length(other)


tm <- list()
df <- list()
for (olag in min:max) {
  
  cat( olag )
  # Sys.sleep(0.01)
  # cat("\014")
  
  ind <- olag + 1L -min
  
  fit <- model_fit_lag_tidy(vlag = vlag, chosen_lag = l, data_bl = modeling_bl, 
                            other = other, op = op, olag = olag,
                            offset = off)
  tm[[ind]] <- fit$tidy
  tm[[ind]] %<>% dplyr::mutate(lag = olag)
  
  df[[ind]] <- fit$model
  
  rm(fit)
  
}

tm_df <- bind_rows(tm)
(max - min +1) * nrow(tm[[1]])




###  Plot results of coarser model -------------

lines_model2 <- c(1.5, 3.5, 7.5, 11.5, 12.5, 15.5, 18.5, 24.5, 26.5, 
                  27.5, 32.5, 36.5, 39.5, 40.5, 44.5, 49.5)


label_model2 <- c("Impf-Index", "Saisonalität Cosinus", "Saisonalität Sinus", 
                 "Ostern, Weihnachten",                
                 "Schulferien, zweite Hälfte", "Nach den Ferien", "Schulferien", 
                 "Masken, Stufe 5",
                 "Masken, Stufe 4", "Masken, Stufe 3", "Masken, Stufe 2",
                 "Abstandsregeln, Stufe 2",
                 "Teststrategie, Stufe 4", "Teststrategie, Stufe 3", "Teststrategie, Stufe 2", 
                 "KSGBDH max2, Anzahl 3", 
                 "KSGBDH max2, Anzahl 2", 
                 "KSGBDH max2, Anzahl 1", 
                 "KSGBDH max, Anzahl 6",
                 "KSGBDH max, Anzahl 5",
                 "KSGBDH max, Anzahl 4",
                 "KSGBDH max, Anzahl 3",
                 "KSGBDH max, Anzahl 2",
                 "KSGBDH max, Anzahl 1",
                 # "Dienstleistungen, Stufe 5",
                 # "Dienstleistungen, Stufe 4",
                 # "Dienstleistungen, Stufe 3",
                 # "Dienstleistungen, Stufe 2",
                 "Nachtleben, Stufe 3",
                 "Nachleben, Stufe 2",
                 # "Groß- und Einzelhandel, Stufe 5",
                 # "Groß- und Einzelhandel, Stufe 4",
                 # "Groß- und Einzelhandel, Stufe 3",
                 # "Groß- und Einzelhandel, Stufe 2",
                 "Ausgangsbeschränkungen, Stufe 2", 
                 "Öffentliche Veranstaltungen, Stufe 6", 
                 "Öffentliche Veranstaltungen, Stufe 5", 
                 "Öffentliche Veranstaltungen, Stufe 4", 
                 "Öffentliche Veranstaltungen, Stufe 3", 
                 "Öffentliche Veranstaltungen, Stufe 2", 
                 "Öffentliche Räume, Stufe 5",
                 "Öffentliche Räume, Stufe 4",
                 "Öffentliche Räume, Stufe 3", "Öffentliche Räume, Stufe 2",
                 "Kindertagesstätten, Stufe 4",
                 "Kindertagesstätten, Stufe 3", "Kindertagesstätten, Stufe 2", 
                 "Arbeitsplätze, Stufe 2", 
                 "private Räume, Stufe 5", "private Räume, Stufe 4",
                 "private Räume, Stufe 3", "private Räume, Stufe 2",
                 "Schulen, Stufe 6", "Schulen, Stufe 5", 
                 "Schulen, Stufe 4", "Schulen, Stufe 3",
                 "Schulen, Stufe 2"
)
length(label_model2)


tm_df %<>% dplyr::mutate(lag_f = factor(lag))
tm_df %<>% mutate(var_e = factor(variable, 
                                 levels = rev(unique(tm_df$variable))),
                  var_e2 = factor(var_e, 
                                  labels = label_model2),
                  protect = (estimate < 1))





ggplot(data = tm_df, aes(y = var_e2)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = lag_f),
                 position = position_dodge(0.9)) + 
  geom_point(aes(x = estimate, color = lag_f),
             position = position_dodge(0.9)) +
  geom_hline(yintercept = lines_model2, color = "gray80")  +
  geom_vline(aes(xintercept = 1), color = "black") +
  labs(x = "Effect on R-value", y = "") +
  themeMod(9) +
  theme(panel.grid.major.y = element_blank(),
        plot.title.position = "plot") +
  scale_color_manual(
    values = rev(wes_palette("FantasticFox1", max-min+1, type = "continuous")),
    breaks = levels(tm_df$lag_f),
    name = "Chosen delay") +
  scale_x_continuous(breaks = seq(0, 2, by = 0.1))

ggsave(file.path("output", paste0("estimates_coarser_model_rangeLag.pdf")), 
       width = 9, height = 12, dpi = 1200)
ggsave(file.path("output", paste0("estimates_coarser_model_rangeLag.png")), 
       width = 9, height = 12, dpi = 1200)


## nur : daycare Stufe 2 und 4, public space measures Stufe 2 und 5. 

tm_df_short <- tm_df %>% dplyr::filter(stringr::str_detect(var_f, 
                                                           'daycare_measures_02|daycare_measures_04|public_space_measures_02|public_space_measures_05'))

ggplot(data = tm_df_short, aes(y = var_e2)) +
  geom_errorbarh(aes(xmax = 1-conf.low, xmin = 1-conf.high, color = lag_f),
                 position = position_dodge(0.9)) + 
  geom_point(aes(x = 1-estimate, color = lag_f),
             position = position_dodge(0.9)) +
  geom_hline(yintercept = c(2.5, 4.5), color = "gray80")  +
  #  geom_vline(aes(xintercept = 1), color = "black") +
  labs(x = "Reduction of R-value", y = "") +
  themeMod(11) +
  theme(panel.grid.major.y = element_blank(),
        plot.title.position = "plot") +
  scale_color_manual(
    values = rev(wes_palette("FantasticFox1", max-min+1, type = "continuous")),
    breaks = rev(levels(tm_df$lag_f)),
    name = "Chosen delay") +
  scale_x_continuous(breaks = seq(0, 2, by = 0.1), limits=c(0, .5))
#, limits = c(0.5, 2))
ggsave(file.path("output", paste0("estimates_coarser_model_rangeLag_short.pdf")), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", paste0("estimates_coarser_model_rangeLag_short.png")), 
       width = 11, height = 5, dpi = 1200)



rangeLag <- tm_df %>% dplyr::arrange(lag_f, estimate) %>% 
  dplyr::filter(variable!=gsub("measures", "", variable) & 
                  variable==gsub("test", "", variable)) %>% 
  dplyr::select(lag_f, term, estimate, conf.low, conf.high) %>%
  dplyr::arrange(lag_f, estimate)

write.csv2(rangeLag, file = file.path("output", "coef_coarser_model_rangeLag.csv"))




## (3) Third model ------------------------

params3 <- c(
# "schools_measures",
#  "private_space_measures",
  "workplace_measures",
  "daycare_measures",
  "public_space_measures",
  # "public_event_measures",
  "curfew_measures",
  #  "retail_measures",
  "nightlife_measures",
  #  "services_measures",
  "ceshgrs_max_measures",
  "ceshgrs_max2_measures",
  "test_measures",
  "abstand_measures" #,
#  "mask_measures"
) 
length(params3)


num_op <- list()
if (length(num_op) >0) { op = params[num_op] } else op = list()
other = params3[!(params3 %in% op)]

length(op)
length(other)


tm <- list()
df <- list()
for (olag in min:max) {
  
  cat( olag )
  # Sys.sleep(0.01)
  # cat("\014")
  
  ind <- olag + 1L -min
  
  fit <- model_fit_lag_tidy(vlag = vlag, chosen_lag = l, data_bl = modeling_bl, 
                            other = other, op = op, olag = olag,
                            offset = off)
  tm[[ind]] <- fit$tidy
  tm[[ind]] %<>% dplyr::mutate(lag = olag)
  
  df[[ind]] <- fit$model
  
  rm(fit)
  
}

tm_df <- bind_rows(tm)
(max - min +1) * nrow(tm[[1]])




###  Plot results of thrid model -------------

lines_model3 <- c(1.5, 3.5, 7.5, 8.5, 11.5, 14.5, 20.5, 22.5, 
                  23.5, 27.5, 30.5, 31.5)


label_model3 <- c("Impf-Index", "Saisonalität Cosinus", "Saisonalität Sinus", 
                  "Ostern, Weihnachten",                
                  "Schulferien, zweite Hälfte", "Nach den Ferien", "Schulferien", 
                  # "Masken, Stufe 5",
                  # "Masken, Stufe 4", "Masken, Stufe 3", "Masken, Stufe 2",
                  "Abstandsregeln, Stufe 2",
                  "Teststrategie, Stufe 4", "Teststrategie, Stufe 3", "Teststrategie, Stufe 2", 
                  "KSGBDH max2, Anzahl 3", 
                  "KSGBDH max2, Anzahl 2", 
                  "KSGBDH max2, Anzahl 1", 
                  "KSGBDH max, Anzahl 6",
                  "KSGBDH max, Anzahl 5",
                  "KSGBDH max, Anzahl 4",
                  "KSGBDH max, Anzahl 3",
                  "KSGBDH max, Anzahl 2",
                  "KSGBDH max, Anzahl 1",
                  # "Dienstleistungen, Stufe 5",
                  # "Dienstleistungen, Stufe 4",
                  # "Dienstleistungen, Stufe 3",
                  # "Dienstleistungen, Stufe 2",
                  "Nachtleben, Stufe 3",
                  "Nachleben, Stufe 2",
                  # "Groß- und Einzelhandel, Stufe 5",
                  # "Groß- und Einzelhandel, Stufe 4",
                  # "Groß- und Einzelhandel, Stufe 3",
                  # "Groß- und Einzelhandel, Stufe 2",
                  "Ausgangsbeschränkungen, Stufe 2", 
                  # "Öffentliche Veranstaltungen, Stufe 6", 
                  # "Öffentliche Veranstaltungen, Stufe 5", 
                  # "Öffentliche Veranstaltungen, Stufe 4", 
                  # "Öffentliche Veranstaltungen, Stufe 3", 
                  # "Öffentliche Veranstaltungen, Stufe 2", 
                  "Öffentliche Räume, Stufe 5",
                  "Öffentliche Räume, Stufe 4",
                  "Öffentliche Räume, Stufe 3", "Öffentliche Räume, Stufe 2",
                  "Kindertagesstätten, Stufe 4",
                  "Kindertagesstätten, Stufe 3", "Kindertagesstätten, Stufe 2", 
                  "Arbeitsplätze, Stufe 2" #, 
                  # "private Räume, Stufe 5", "private Räume, Stufe 4",
                  # "private Räume, Stufe 3", "private Räume, Stufe 2",
                  # "Schulen, Stufe 6", "Schulen, Stufe 5", 
                  # "Schulen, Stufe 4", "Schulen, Stufe 3",
                  # "Schulen, Stufe 2"
)
length(label_model2)


tm_df %<>% dplyr::mutate(lag_f = factor(lag))
tm_df %<>% mutate(var_e = factor(variable, 
                                 levels = rev(unique(tm_df$variable))),
                  var_e2 = factor(var_e, 
                                  labels = label_model3),
                  protect = (estimate < 1))





ggplot(data = tm_df, aes(y = var_e2)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = lag_f),
                 position = position_dodge(0.9)) + 
  geom_point(aes(x = estimate, color = lag_f),
             position = position_dodge(0.9)) +
  geom_hline(yintercept = lines_model3, color = "gray80")  +
  geom_vline(aes(xintercept = 1), color = "black") +
  labs(x = "Effect on R-value", y = "") +
  themeMod(9) +
  theme(panel.grid.major.y = element_blank(),
        plot.title.position = "plot") +
  scale_color_manual(
    values = rev(wes_palette("FantasticFox1", max-min+1, type = "continuous")),
    breaks = levels(tm_df$lag_f),
    name = "Chosen delay") +
  scale_x_continuous(breaks = seq(0, 2, by = 0.1))

ggsave(file.path("output", paste0("estimates_third_model_rangeLag.pdf")), 
       width = 9, height = 12, dpi = 1200)
ggsave(file.path("output", paste0("estimates_third_model_rangeLag.png")), 
       width = 9, height = 12, dpi = 1200)


## nur : daycare Stufe 2 und 4, public space measures Stufe 2 und 5. 

tm_df_short <- tm_df %>% dplyr::filter(stringr::str_detect(var_f, 
                  'daycare_measures_02|daycare_measures_04|public_space_measures_02|public_space_measures_05'))

ggplot(data = tm_df_short, aes(y = var_e2)) +
  geom_errorbarh(aes(xmax = 1-conf.low, xmin = 1-conf.high, color = lag_f),
                 position = position_dodge(0.9)) + 
  geom_point(aes(x = 1-estimate, color = lag_f),
             position = position_dodge(0.9)) +
  geom_hline(yintercept = c(2.5, 4.5), color = "gray80")  +
  #  geom_vline(aes(xintercept = 1), color = "black") +
  labs(x = "Reduction of R-value", y = "") +
  themeMod(11) +
  theme(panel.grid.major.y = element_blank(),
        plot.title.position = "plot") +
  scale_color_manual(
    values = rev(wes_palette("FantasticFox1", max-min+1, type = "continuous")),
    breaks = rev(levels(tm_df$lag_f)),
    name = "Chosen delay") +
  scale_x_continuous(breaks = seq(0, 2, by = 0.1), limits=c(0, .5))
#, limits = c(0.5, 2))
ggsave(file.path("output", paste0("estimates_third_model_rangeLag_short.pdf")), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", paste0("estimates_third_model_rangeLag_short.png")), 
       width = 11, height = 5, dpi = 1200)



rangeLag <- tm_df %>% dplyr::arrange(lag_f, estimate) %>% 
  dplyr::filter(variable!=gsub("measures", "", variable) & 
                  variable==gsub("test", "", variable)) %>% 
  dplyr::select(lag_f, term, estimate, conf.low, conf.high) %>%
  dplyr::arrange(lag_f, estimate)

write.csv2(rangeLag, file = file.path("output", "coef_third_model_rangeLag.csv"))



