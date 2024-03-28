library(dplyr)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(lubridate)
library(ggtext)
library(MASS)
library(RColorBrewer)
library(scales)

rm(list = ls())

source("Skripte/helper_main.R")


load(file = file.path("Daten", paste0("modeling_bl_ag.rData")))
load(file = file.path("Daten", paste0("modeling_bl.rData")))



## Data preparation --------------


data_main <- finish_prep(data = modeling_bl)
data_main_ag <- finish_prep(data = modeling_bl_ag)

summary(data_main$per_vacc_1)
modeling_bl %<>% dplyr::mutate(Inz7T = N_onset_smooth *7 / Bev * 1e5)
data_main %<>% dplyr::mutate(Inz7T = N_onset_smooth *7 / Bev * 1e5)


## Describe vaccination ---------------


###   Vaccination by federal states --------------

ggplot(data = modeling_bl, aes(x = date)) +
  geom_line(aes(y = Inz7T, color = "Daten")) + #,
  geom_line(aes(y = per_vacc_1*max(Inz7T, na.rm = T)/4, color = "Vacc_index_1")) +
  geom_line(aes(y = per_vacc_2*max(Inz7T, na.rm = T)/4, color = "Vacc_index_2")) +
  geom_vline(xintercept = as.numeric(dmy("01-01-2021"))-.5, 
             color = "gray30", size = .01) +
  facet_wrap(~ BL, nrow = 4) +
  labs(y = "7 Tages-Inzidenz",
       x = "Monate der Jahre 2020 und 2021") + 
  scale_color_manual(
    values = c((wes_palette("Darjeeling1", 
                            2, type = "continuous")), "gray"),
    breaks = c("Vacc_index_1", "Vacc_index_2", "Daten"),
    labels = c("Impfindex,\n1. Impfung", 
               "Impfindex,\n2. Impfung", 
               "7 Tages-\nInzidenz"),
    name = "") +
  scale_x_date(date_breaks = "1 months",
               limits = c(as.Date("2020/03/01", "%Y/%m/%d"), 
                          as.Date("2021/08/31", "%Y/%m/%d")),
               date_labels = "%b") +
  scale_y_continuous(sec.axis = sec_axis(~ . /max(modeling_bl$Inz7T)*4, 
                                         name = "Impfindex"),
                     limits = c(0, max(modeling_bl$Inz7T, na.rm = T)), 
                     labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  themeMod(11) +
  theme(axis.text.x  = element_text(angle=90, vjust=.5, hjust=1),
        panel.grid.major.x = element_line(),
        plot.caption=element_text(hjust = 0),
        plot.title.position = "plot") 

ggsave(file.path("output", "Describe_vacc_fs.pdf"), 
       width = 11, height = 8, dpi = 1200)
ggsave(file.path("output", "Describe_vacc_fs.png"), 
       width = 11, height = 8, dpi = 1200)
ggsave(file.path("output", "Describe_vacc_fs.emf"), 
       width = 11, height = 8, dpi = 1200)


for (a in 1:3) {
  
  if (a==1) {
    ag = "0 bis 17 Jahre" 
  }
  if (a==2) {
    ag = "18 bis 59 Jahre"
  }
  if (a==3) {
    ag = "60 und mehr Jahre"
  }
  
  data_a <- modeling_bl_ag %>% dplyr::filter(AG == ag)
  data_a %<>% dplyr::mutate(Inz7T = N_onset_smooth *7 / Bev * 1e5)

ggplot(data = data_a, aes(x = date)) +
  geom_line(aes(y = Inz7T, color = "Daten")) + #,
  geom_line(aes(y = per_vacc_1*max(Inz7T, na.rm = T)/4, color = "Vacc_index_1")) +
  geom_line(aes(y = per_vacc_2*max(Inz7T, na.rm = T)/4, color = "Vacc_index_2")) +
  geom_vline(xintercept = as.numeric(dmy("01-01-2021"))-.5, 
             color = "gray30", size = .01) +
  facet_wrap(~ BL, nrow = 4) +
  labs(y = "7 Tages-Inzidenz",
       x = "Monate der Jahre 2020 und 2021",
       title = paste0("Altersgruppe ", ag) ) + 
  scale_color_manual(
    values = c((wes_palette("Darjeeling1", 
                            2, type = "continuous")), "gray"),
    breaks = c("Vacc_index_1", "Vacc_index_2", "Daten"),
    labels = c("Impfindex,\n1. Impfung", 
               "Impfindex,\n2. Impfung", 
               "7 Tages-\nInzidenz"),
    name = "") +
  scale_x_date(date_breaks = "1 months",
               limits = c(as.Date("2020/03/01", "%Y/%m/%d"), 
                          as.Date("2021/08/31", "%Y/%m/%d")),
               date_labels = "%b") +
  scale_y_continuous(sec.axis = sec_axis(~ . /max(data_a$Inz7T)*4, 
                                         name = "Impfindex"),
                     limits = c(0, max(data_a$Inz7T, na.rm = T)), 
                     labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  themeMod(11) +
  theme(axis.text.x  = element_text(angle=90, vjust=.5, hjust=1),
        panel.grid.major.x = element_line(),
        plot.caption=element_text(hjust = 0),
        plot.title.position = "plot") 

ggsave(file.path("output", paste0("Describe_vacc_fs_ag", a, ".pdf")), 
       width = 11, height = 8, dpi = 1200)
ggsave(file.path("output", paste0("Describe_vacc_fs_ag", a, ".png")), 
       width = 11, height = 8, dpi = 1200)
ggsave(file.path("output", paste0("Describe_vacc_fs_ag", a, ".emf")), 
       width = 11, height = 8, dpi = 1200)

}


###   Vaccination by age groups --------------




data_D_ag <- modeling_bl_ag %>% dplyr::group_by(date, AG) %>%
  dplyr::summarise(N_onset_smooth = sum(N_onset_smooth, na.rm = T),
                   lag4_N_onset = sum(lag4_N_onset, na.rm = T),
                   cum_Anzahl_1 = sum(cum_Anzahl_1, na.rm = T),
                   cum_Anzahl_2 = sum(cum_Anzahl_2, na.rm = T),
                   Bev = sum(Bev, na.rm = T),
                   Alpha = mean(Alpha),
                   Delta = mean(Delta)
  ) %>%
  dplyr::mutate(Impfquote_1 = cum_Anzahl_1 / Bev *100,
                Impfquote_2 = cum_Anzahl_2 / Bev *100,
                per_vacc_1 = -log(1 - Impfquote_1/100)/log(2),
                per_vacc_2 = -log(1 - Impfquote_2/100)/log(2),
                Inz7T = N_onset_smooth *7 / Bev * 1e5,
                Inz7T_lag4 = lag4_N_onset *7 / Bev * 1e5,
                R_all = Inz7T / Inz7T_lag4,
                R = case_when((N_onset_smooth<1) ~ NA_real_,  
                              TRUE ~ R_all),
                Wildtyp = 1 - Alpha - Delta
  )




ggplot(data = data_D_ag, aes(x = date)) +
  geom_line(aes(y = Inz7T, color = "Daten", fill = "Daten")) + #,
  geom_line(aes(y = Impfquote_1*max(Inz7T, na.rm = T)/100, color = "Vacc_rate_1")) +
  geom_line(aes(y = Impfquote_2*max(Inz7T, na.rm = T)/100, color = "Vacc_rate_2")) +
  geom_vline(xintercept = as.numeric(dmy("01-01-2021"))-.5, 
             color = "gray30", size = .01) +
  facet_wrap(~ AG, nrow = 3) +
  labs(y = "7-Tages-Inzidenz",
       x = "Monat in 2020 und 2021") + 
  scale_color_manual(
    values = c((wes_palette("Darjeeling1", 
                            2, type = "continuous")), "gray"),
    breaks = c("Vacc_rate_1", "Vacc_rate_2", "Daten"),
    labels = c("Impfquote\n1. Impfung", "Impfquote\n2. Impfung", 
               "7-Tages-\nInzidenz"),
    name = "") +
  scale_x_date(date_breaks = "1 months",
               limits = c(as.Date("2020/03/01", "%Y/%m/%d"), 
                          as.Date("2021/08/31", "%Y/%m/%d")),
               date_labels = "%b") +
  scale_y_continuous(sec.axis = sec_axis(~ . /max(data_D_ag$Inz7T)*100, 
                                         name = "Impfquote in %",
                                         breaks = seq(0, 100, by=20)),
                     limits = c(0, max(data_D_ag$Inz7T))) + 
  themeMod(11) +
  theme(axis.text.x  = element_text(angle=90, vjust=.5, hjust=1),
        panel.grid.major.x = element_line(),
        plot.caption=element_text(hjust = 0),
        plot.title.position = "plot") 

ggsave(file.path("output", "Describe_vacc_rate.pdf"), 
       width = 11, height = 8, dpi = 1200)
ggsave(file.path("output", "Describe_vacc_rate.png"), 
       width = 11, height = 8, dpi = 1200)


ggplot(data = data_D_ag, aes(x = date)) +
  geom_line(aes(y = Inz7T, color = "Daten", fill = "Daten")) + #,
  geom_line(aes(y = Impfquote_1*max(Inz7T, na.rm = T)/100, 
                color = "Vacc_rate_1")) +
  geom_line(aes(y = Impfquote_2*max(Inz7T, na.rm = T)/100, 
                color = "Vacc_rate_2")) +
  geom_vline(xintercept = as.numeric(dmy("01-01-2021"))-.5, 
             color = "gray30", size = .01) +
  facet_wrap(~ AG, nrow = 3) +
  labs(y = "7-Tages-Inzidenz",
       x = "Monate der Jahre 2020 und 2021") + 
  scale_color_manual(
    values = c((wes_palette("Darjeeling1", 
                            2, type = "continuous")), "gray40"),
    breaks = c("Vacc_rate_1", "Vacc_rate_2", "Daten"),
    labels = c("Impfquote\n1. Impfung", "Impfquote\n2. Impfung", 
               "7-Tages-\nInzidenz"),
    name = "") +
  scale_x_date(date_breaks = "1 months",
               limits = c(as.Date("2020/03/01", "%Y/%m/%d"), 
                          as.Date("2021/08/31", "%Y/%m/%d")),
               date_labels = "%b") +
  scale_y_continuous(sec.axis = sec_axis(~ . /max(data_D_ag$Inz7T)*100, 
                                         name = "Impfquote in %",
                                         breaks = seq(0, 100, by=20)),
                     limits = c(0, max(data_D_ag$Inz7T))) +
  themeMod(20) +
  theme(axis.text.x  = element_text(angle=90, vjust=.5, hjust=1),
        panel.grid.major.x = element_line(),
        plot.caption=element_text(hjust = 0),
        plot.title.position = "plot") 

ggsave(file.path("output", "Describe_vacc_rate_pptx.pdf"), 
       width = 11, height = 8, dpi = 1200)
ggsave(file.path("output", "Describe_vacc_rate_pptx.png"), 
       width = 11, height = 8, dpi = 1200)




ggplot(data = data_D_ag, aes(x = date)) +
  #  geom_ribbon(aes(ymin = lb_predict, ymax = ub_predict, fill = "Main model"), alpha = .2) +
  geom_line(aes(y = Inz7T, color = "Daten", fill = "Daten")) + #,
  geom_line(aes(y = per_vacc_1*max(Inz7T, na.rm = T)/4, color = "Vacc_index_1")) +
  geom_line(aes(y = per_vacc_2*max(Inz7T, na.rm = T)/4, color = "Vacc_index_2")) +
  geom_vline(xintercept = as.numeric(dmy("01-01-2021"))-.5, 
             color = "gray30", size = .01) +
  facet_wrap(~ AG, nrow = 3) +
  labs(y = "7-Tages-Inzidenz",
       x = "Monate der Jahre 2020 und 2021") + 
  scale_color_manual(
    values = c((wes_palette("Darjeeling1", 
                            2, type = "continuous")), "gray"),
    breaks = c("Vacc_index_1", "Vacc_index_2", "Daten"),
    labels = c("Impfindex,\n1. Impfung", "Impfindex,\n2. Impfung", "7-Tages-\nInzidenz"),
    name = "") +
  scale_x_date(date_breaks = "1 months",
               limits = c(as.Date("2020/03/01", "%Y/%m/%d"), 
                          as.Date("2021/08/31", "%Y/%m/%d")),
               date_labels = "%b") +
  scale_y_continuous(sec.axis = sec_axis(~ . /max(data_D_ag$Inz7T)*4, 
                                         name = "Impfindex"),
                     limits = c(0, max(data_D_ag$Inz7T, na.rm = T))) + # labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  themeMod(11) +
  theme(axis.text.x  = element_text(angle=90, vjust=.5, hjust=1),
        panel.grid.major.x = element_line(),
        plot.caption=element_text(hjust = 0),
        plot.title.position = "plot") 

ggsave(file.path("output", "Describe_vacc_country.pdf"), 
       width = 11, height = 8, dpi = 1200)
ggsave(file.path("output", "Describe_vacc_country.png"), 
       width = 11, height = 8, dpi = 1200)
ggsave(file.path("output", "Describe_vacc_country.emf"), 
       width = 11, height = 8, dpi = 1200)




## Describe Variants -------------------


load(file = file.path("Daten", paste0("modeling_bl.rData")))

modeling_bl %<>% dplyr::select(-c(Bev, bev_weight))

modeling_bl %>% group_by(BL) %>% dplyr::summarise(Alpha = mean(Alpha),
                                                          Delta = mean(Delta))

variants <- modeling_bl %>% group_by(date) %>% dplyr::summarise(Alpha = mean(Alpha),
                                                                Delta = mean(Delta),
                                                                min_Alpha = min(Alpha))

with(variants, stopifnot(Alpha == min_Alpha))
variants$min_Alpha <- NULL
variants %<>% dplyr::mutate(Wildtyp = 1 - Alpha - Delta)

var_long <- variants %>% pivot_longer(cols = !date, 
                                      names_to = "variant", 
                                      values_to = "Anteil") %>%
  dplyr::mutate(variant_f = factor(variant, 
                                   levels = c("Wildtyp", "Alpha", "Delta")))

with(var_long, table(variant, variant_f))

ggplot(data = var_long, aes(x = date)) +
  geom_line(aes(y = Anteil*100, color = variant_f)) +
  themeMod(11) +
  labs(y = "Anteile in %",
       x = "Datum der Jahre 2020 und 2021") +
  themeMod(11) + 
  scale_color_discrete(
    name = "Virusvariante") +
  scale_x_date(date_breaks = "1 months",
               date_minor_breaks = "1 months",
               minor_breaks = waiver(),
               limits = c(as.Date("2020/03/01", "%Y/%m/%d"), 
                          as.Date("2021/08/31", "%Y/%m/%d")),
               date_labels = "%m-%d") +
  theme(axis.text.x  = element_text(angle=90, vjust=1))
ggsave(file.path("output", "Varianten_date.pdf"), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", "Varianten_date.png"), 
       width = 11, height = 5, dpi = 1200)





summary(data_D_ag$Alpha)


ggplot(data = data_D_ag, aes(x = date)) +
  #  geom_ribbon(aes(ymin = lb_predict, ymax = ub_predict, fill = "Main model"), alpha = .2) +
  geom_line(aes(y = Inz7T, color = "Daten", fill = "Daten")) + #,
  geom_line(aes(y = Alpha*max(Inz7T, na.rm = T), color = "Alpha")) +
  geom_line(aes(y = Delta*max(Inz7T, na.rm = T), color = "Delta")) +
  geom_vline(xintercept = as.numeric(dmy("01-01-2021"))-.5, 
             color = "gray30", size = .01) +
  facet_wrap(~ AG, nrow = 3) +
  labs(y = "7-Tages-Inzidenz",
       x = "Monat in 2020 und 2021") + 
  scale_color_manual(
    values = c((wes_palette("Darjeeling1", 2, type = "continuous")), "gray"),
    breaks = c("Alpha", "Delta", "Daten"),
    labels = c("Anteil Alpha in %", "Anteil Delta in %", "7 Tage-Inzidenz"),
    name = "") +
  scale_x_date(date_breaks = "1 months",
               limits = c(as.Date("2020/03/01", "%Y/%m/%d"), 
                          as.Date("2021/08/31", "%Y/%m/%d")),
               date_labels = "%b") +
  scale_y_continuous(sec.axis = sec_axis(~ . /max(data_D_ag$Inz7T)*100, 
                                         name = "Anteil Varianten"),
                     limits = c(0, max(data_D_ag$Inz7T, na.rm = T))) + # labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  themeMod(11) +
  theme(axis.text.x  = element_text(angle=90, vjust=.5, hjust=1),
        panel.grid.major.x = element_line(),
        plot.caption=element_text(hjust = 0),
        plot.title.position = "plot") 

ggsave(file.path("output", "Describe_data_onset_varianten.pdf"), 
       width = 11, height = 8, dpi = 1200)
ggsave(file.path("output", "Describe_data_onset_varianten.png"), 
       width = 11, height = 8, dpi = 1200)
ggsave(file.path("output", "Describe_data_onset_varianten.emf"), 
       width = 11, height = 8, dpi = 1200)






##  Describe level of severity ----------------

data_m <- data_main %>% dplyr::filter(date <= as.Date("2021/08/31", "%Y/%m/%d")) %>%
  dplyr::select(contains("measures_0"), contains("measures_1"),
                BL, date, Bev) %>% as_tibble()
data_m %<>% dplyr::group_by(date) %>% dplyr::mutate(ges_Bev = sum(Bev))
data_m %>% dplyr::group_by(date) %>% dplyr::summarise(ges_Bev = mean(ges_Bev),
                                                      check_Bev = min(ges_Bev))

data_m %<>% dplyr::mutate(bev_weight = Bev / ges_Bev)

data_m <- data_m %>% ungroup() %>%
  dplyr::select(contains("measures_0"), contains("measures_1"), 
                bev_weight, date) %>%
  group_by(date) %>% 
  dplyr::summarise(across(.cols = everything(),
                          .fns = ~ weighted.mean(., w = bev_weight, na.rm = TRUE)))

data_m <- data_m %>% ungroup() %>%
  dplyr::select(contains("measures_0"), contains("measures_1")) %>%
  dplyr::summarise(across(.cols = everything(),
                          .fns = ~ mean(., na.rm = TRUE)))

data_m <- data_m %>% pivot_longer(cols = everything()) 
data_m %<>% rowwise() %>% dplyr::mutate(name = gsub("public_event", "pEvent", name),
                                        name = gsub("_comb_", "C_", name),
                                        area = paste(str_split(name, pattern = "_")[[1]][1],
                                                     str_split(name, pattern = "_")[[1]][2],
                                                     sep = "_"),
                                        area = gsub("_measures", "", area),
                                        stufe = rev(str_split(name, pattern = "_")[[1]])[1] %>% as.numeric()) %>%
  as_tibble()
data_m %<>% group_by(area) %>% dplyr::mutate(check = sum(value)) %>% 
  arrange(area, stufe) %>%  as_tibble()
summary(data_m$check)



##  Plot level of severity -------------

data_m <- data_m %>% dplyr::filter(area %in% c(main_measures, "ceshgrs_max", "ceshgrs_max2"))
data_m %<>% dplyr::mutate(area_f = factor(area, levels = c(main_measures[1:12], 
                                                           "ceshgrs_max", "ceshgrs_max2", 
                                                           main_measures[13:15])),
                          area_e = factor(area_f, labels = c(label_measures[1:12], 
                                                             "KSGBDH - Anzahl \n maximale Stufe", "KSGBDH - Anzahl \n zweitstärkste Stufe",
                                                             label_measures[13:15])) )



ggplot(data = data_m, 
       aes(x = area_e)) +
  geom_bar(aes(y = value*100, fill = as.factor(stufe)), 
           stat = "identity", 
           position = "stack", 
           color = "black") + 
  themeMod(11) + theme(axis.text.x  = element_text(angle=90, vjust=1),
                       legend.text  = element_text(size = 8),
                       legend.key.height = unit(1,"line"),
                       legend.key.size = unit(1,"line")) +
  labs(x = "", y = "Proportion of time in %\n(country wide mean)") +
  scale_fill_manual(values = brewer_pal(palette = "YlOrBr", type = "seq", direction = 1)(6),
                      name = "Severity")
ggsave(file.path("output", "Describe_severity_main.pdf"), 
       width = 11, height = 5, dpi = 1200)  
ggsave(file.path("output", "Describe_severity_main.png"), 
       width = 11, height = 5, dpi = 1200)  




## Measure score  --------------------

names_npi <- names(modeling_bl %>% dplyr::select(starts_with(params)))

data_main %<>% dplyr::mutate(measure_score = 0L)
for (i in names_npi) {
  stufe = as.integer(strsplit(stringi::stri_reverse(i), "\\_", 
                              perl=TRUE)[[1]][[1]])/10 - 1
  cat(paste0(stufe, " "))
  data_main %<>% rowwise() %>% dplyr::mutate(
    measure_score = sum(measure_score, stufe * get(i), na.rm = T))
}


data_main %<>% dplyr::mutate(round_score = round(measure_score))

length(names_npi)
summary(data_main$measure_score)


data_score <- data_main %>% dplyr::group_by(date) %>% dplyr::mutate(ges_Bev = sum(Bev)) %>%
  dplyr::select(BL, date, measure_score, contains("_logR"), everything())
data_score %<>% dplyr::mutate(Inz7T = N_onset_smooth *7 / Bev * 1e5,
                               R = R_ons,
                               logR = log(R_ons),
                               logRm = case_when((logR <= 0) ~ logR,
                                                 TRUE ~ NA_real_),
                               bev_weight = Bev / ges_Bev)


data_score %>% dplyr::filter(date == as.Date("2021-03-01", "%Y-%m-%d")) %>% 
  group_by(BL) %>% dplyr::summarise(Alpha = mean(Alpha))

model_d <- data_score %>% dplyr::group_by(date) %>% 
  dplyr::summarise(N_onset = sum(N_onset),
                   N_onset_smooth = sum(N_onset_smooth),
                   lag4_N_onset = sum(lag4_N_onset),
                   Alpha = min(Alpha),
                   Delta = min(Delta),
                   Bev = sum(Bev),
                   mean_Inz7T = mean(Inz7T),
                   wmean_Inz7T = weighted.mean(Inz7T, w = bev_weight),
                   min_Inz7T = min(Inz7T),
                   max_Inz7T = max(Inz7T),
                   mean_mscore = mean(measure_score, na.rm = T),
                   wmean_mscore = weighted.mean(measure_score, w = bev_weight, na.rm = T),
                   min_mscore = min(measure_score, na.rm = T),
                   max_mscore = max(measure_score, na.rm = T)
  ) %>%
  dplyr::mutate(R_all = N_onset_smooth / lag4_N_onset,
                R = case_when((N_onset_smooth<1) 
                              ~ NA_real_,
                              TRUE ~ R_all),
                logR = log(R),
                lag7_N_onset = lag(N_onset_smooth, n = 7L),
                R7_all = N_onset_smooth / lag7_N_onset,
                R7 = case_when((N_onset_smooth<1) 
                               ~ NA_real_,
                               TRUE ~ R7_all),
                logR7 = log(R7),
                logRm = case_when((logR <= 0) ~ logR,
                                  TRUE ~ NA_real_),
                Inz7T = N_onset_smooth *7 / Bev * 1e5)

max_msc = ceiling(max(model_d$max_mscore))

ggplot(data = model_d, aes(x = date)) +
  geom_ribbon(aes(ymin = min_mscore*max(Inz7T, na.rm = T)/max_msc, 
                  ymax = max_mscore*max(Inz7T, na.rm = T)/max_msc, 
                  fill = "Maßnahmen-\nscore"), 
              alpha = .2, color = NA) +
  geom_line(aes(y = Inz7T, fill = "7-Tages\nInzidenz", color = "7-Tages\nInzidenz")) +
  geom_line(aes(y = mean_mscore*max(Inz7T, na.rm = T)/max_msc, color = "Maßnahmen-\nscore")) +
  #  geom_hline(yintercept = max(model_d$Inz7T)/4, color = "black") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
                     sec.axis = sec_axis(~ . /max(model_d$Inz7T, na.rm = T)*max_msc, 
                                         breaks = seq(0, 50, by = 10),
                                         name = "Maßnahmen-score"),
                     limits = c(0, max(model_d$Inz7T)),
                     breaks = seq(0, 250, by = 50)) +
  labs(y = "7-Tage Inzidenz",
       x = "Datum der Jahre 2020 und 2021") + 
  scale_color_manual(
    values = c("deepskyblue4", "green3"),
    breaks = c("7-Tages\nInzidenz", "Maßnahmen-\nscore"),
    name = "") +
  scale_fill_manual(
    values = c("white", "green3"),
    breaks = c("7-Tages\nInzidenz", "Maßnahmen-\nscore"),
    name = "") +
  scale_x_date(date_breaks = "1 months",
               limits = c(as.Date("2020/03/01", "%Y/%m/%d"), as.Date("2021/08/31", "%Y/%m/%d")),
               date_labels = "%m-%d") +
  themeMod(11) +
  theme(axis.text.x  = element_text(angle=90, vjust=.5),
        panel.grid.major.y = element_blank(),
        plot.caption=element_text(hjust = 0),
        plot.title.position = "plot")

ggsave(file.path("output", "Describe_data_onset_score.pdf"), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", "Describe_data_onset_score.png"), 
       width = 11, height = 5, dpi = 1200)



ggplot(data = model_d, aes(x = date)) +
  geom_ribbon(aes(ymin = min_mscore*max(max_Inz7T, na.rm = T)/max_msc, 
                  ymax = max_mscore*max(max_Inz7T, na.rm = T)/max_msc, 
                  fill = "Maßnahmen-\nscore"), 
              alpha = .2, color = NA) +
  geom_ribbon(aes(ymin = min_Inz7T, 
                  ymax = max_Inz7T, 
                  fill = "7-Tages\nInzidenz"), 
              alpha = .2, color = NA) +
  geom_line(aes(y = wmean_Inz7T, fill = "7-Tages\nInzidenz", color = "7-Tages\nInzidenz")) +
  geom_line(aes(y = wmean_mscore*max(max_Inz7T, na.rm = T)/max_msc, color = "Maßnahmen-\nscore")) +
  #  geom_hline(yintercept = max(model_d$max_Inz7T)/4, color = "black") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
                     sec.axis = sec_axis(~ . /max(model_d$max_Inz7T, na.rm = T)*max_msc, 
                                         breaks = seq(0, 50, by = 10),
                                         name = "Maßnahmen-score"),
                     limits = c(0, max(model_d$max_Inz7T)),
                     breaks = seq(0, 500, by = 100)) +
  labs(y = "7-Tage Inzidenz",
       x = "Datum der Jahre 2020 und 2021") + 
  scale_color_manual(
    values = c("deepskyblue4", "green3"),
    breaks = c("7-Tages\nInzidenz", "Maßnahmen-\nscore"),
    name = "") +
  scale_fill_manual(
    values = c("deepskyblue4", "green3"),
    breaks = c("7-Tages\nInzidenz", "Maßnahmen-\nscore"),
    name = "") +
  scale_x_date(date_breaks = "1 months",
               limits = c(as.Date("2020/03/01", "%Y/%m/%d"), as.Date("2021/08/31", "%Y/%m/%d")),
               date_labels = "%m-%d") +
  themeMod(11) +
  theme(axis.text.x  = element_text(angle=90, vjust=.5),
        panel.grid.major.y = element_blank(),
        plot.caption=element_text(hjust = 0),
        plot.title.position = "plot")

ggsave(file.path("output", "Describe_data_onset_score_wmean.pdf"), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", "Describe_data_onset_score_wmean.png"), 
       width = 11, height = 5, dpi = 1200)




wes_palette("FantasticFox1", 7, type = "continuous")

ggplot(data = model_d, aes(x = date)) +
  geom_ribbon(aes(ymin = min_mscore*max(max_Inz7T, na.rm = T)/max_msc, 
                  ymax = max_mscore*max(max_Inz7T, na.rm = T)/max_msc, 
                  fill = "Maßnahmen-\nscore"), 
              alpha = .2, color = NA) +
  geom_ribbon(aes(ymin = min_Inz7T, 
                  ymax = max_Inz7T, 
                  fill = "7-Tages\nInzidenz"), 
              alpha = .2, color = NA) +
  geom_line(aes(y = mean_Inz7T, fill = "7-Tages\nInzidenz", color = "7-Tages\nInzidenz")) +
  geom_line(aes(y = mean_mscore*max(max_Inz7T, na.rm = T)/max_msc, color = "Maßnahmen-\nscore")) +
  #  geom_hline(yintercept = max(model_d$max_Inz7T)/4, color = "black") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
                     sec.axis = sec_axis(~ . /max(model_d$max_Inz7T, 
                                                  na.rm = T)*max_msc, 
                                         breaks = seq(0, 50, by = 10),
                                         name = "Maßnahmen-score"),
                     limits = c(0, max(model_d$max_Inz7T)),
                     breaks = seq(0, 500, by = 100)) +
  labs(y = "7-Tages-Inzidenz",
       x = "Datum der Jahre 2020 und 2021") + 
  scale_color_manual(
    values = # wes_palette("FantasticFox1", 7, type = "continuous")[c(4,1)],
      brewer.pal(4, "Set1")[c(2, 4)],
    breaks = c("7-Tages\nInzidenz", "Maßnahmen-\nscore"),
    labels = c("7-Tages-\nInzidenz", "Maßnahmen-\nscore"),
    name = "") +
  scale_fill_manual(
    values = # wes_palette("FantasticFox1", 7, type = "continuous")[c(4,1)],
      brewer.pal(4, "Set1")[c(2, 4)],
    breaks = c("7-Tages\nInzidenz", "Maßnahmen-\nscore"),
    labels = c("7-Tages-\nInzidenz", "Maßnahmen-\nscore"),
    name = "") +
  scale_x_date(date_breaks = "1 months",
               limits = c(as.Date("2020/03/01", "%Y/%m/%d"), as.Date("2021/08/31", "%Y/%m/%d")),
               date_labels = "%m-%d") +
  themeMod(11) +
  theme(axis.text.x  = element_text(angle=90, vjust=.5),
        panel.grid.major.y = element_blank(),
        plot.caption=element_text(hjust = 0),
        plot.title.position = "plot")

ggsave(file.path("output", "Describe_data_onset_score_mean.pdf"), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", "Describe_data_onset_score_mean.png"), 
       width = 11, height = 5, dpi = 1200)



ggplot(data = data_score, aes(x = date)) +
  geom_line(aes(y = Inz7T, color = "7-Tages\nInzidenz")) +
  geom_line(aes(y = measure_score*max(Inz7T, na.rm = T)/max_msc, color = "Maßnahmen-\nscore")) +
  #  geom_hline(yintercept = max(model_d$Inz7T)/4, color = "black") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
                     sec.axis = sec_axis(~ . /max(data_score$Inz7T, na.rm = T)*max_msc, 
                                         breaks = seq(0, 50, by = 10),
                                         name = "Maßnahmen-score"),
                     limits = c(0, max(data_score$Inz7T))
                     # , breaks = seq(0, 250, by = 50)
  ) +
  facet_wrap(~ BL, nrow = 4) +
  labs(y = "7-Tages-Inzidenz",
       x = "Datum der Jahre 2020 und 2021") + 
  scale_color_manual(
    values = c("deepskyblue4", "green3"),
    breaks = c("7-Tages\nInzidenz", "Maßnahmen-\nscore"),
    labels = c("7-Tages-\nInzidenz", "Maßnahmen-\nscore"),
    name = "") +
  scale_fill_manual(
    values = c("white", "green3"),
    breaks = c("7-Tages\nInzidenz", "Maßnahmen-\nscore"),
    labels = c("7-Tages-\nInzidenz", "Maßnahmen-\nscore"),
    name = "") +
  scale_x_date(date_breaks = "1 months",
               limits = c(as.Date("2020/03/01", "%Y/%m/%d"), as.Date("2021/08/31", "%Y/%m/%d")),
               date_labels = "%m-%d") +
  themeMod(11) +
  theme(axis.text.x  = element_text(angle=90, vjust=.5),
        # panel.grid.major.y = element_blank(),
        plot.caption=element_text(hjust = 0),
        plot.title.position = "plot")

ggsave(file.path("output", paste0("Describe_data_onset_score_BL.pdf")), 
       width = 11, height = 8, dpi = 1200)
ggsave(file.path("output", paste0("Describe_data_onset_score_BL.png")), 
       width = 11, height = 8, dpi = 1200)




ggplot(data = model_d, aes(x = date)) +
  geom_line(aes(y = Inz7T, color = "7-Tages\nInzidenz")) +
  geom_line(aes(y = R *max(Inz7T)/4, color = "R-Wert")) +
  geom_hline(yintercept = max(model_d$Inz7T)/4, color = "black") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
                     sec.axis = sec_axis(~ . /max(model_d$Inz7T)*4, 
                                         name = "R-Wert")) +
  labs(y = "7-Tage Inzidenz",
       x = "Datum der Jahre 2020 und 2021") + 
  scale_color_manual(
    values = rev(wes_palette("Darjeeling1", 
                             2, type = "continuous")),
    breaks = c("7-Tages\nInzidenz", "R-Wert"),
    name = "") +
  scale_x_date(date_breaks = "1 months",
               limits = c(as.Date("2020/03/01", "%Y/%m/%d"), as.Date("2021/08/31", "%Y/%m/%d")),
               date_labels = "%m-%d") +
  themeMod(11) +
  theme(axis.text.x  = element_text(angle=90, vjust=.5, hjust=1),
        panel.grid.major.y = element_blank(),
        plot.caption=element_text(hjust = 0),
        plot.title.position = "plot") 

ggsave(file.path("output", paste0("Describe_data_onset_R.pdf")), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", paste0("Describe_data_onset_R.png")), 
       width = 11, height = 5, dpi = 1200)




ggplot(data = model_d, aes(x = date)) +
  geom_line(aes(y = Inz7T, color = "7-Tages\nInzidenz")) +
  geom_line(aes(y = logR *max(Inz7T)/1.5, color = "logarithmischer\nR-Wert")) +
  geom_hline(yintercept = 0, color = "black") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
                     sec.axis = sec_axis(~ . /max(model_d$Inz7T)*1.5, 
                                         name = "logarithmischer R-Wert")) +
  labs(y = "7-Tage Inzidenz",
       x = "Datum der Jahre 2020 und 2021") + 
  scale_color_manual(
    values = rev(wes_palette("Darjeeling1", 
                             2, type = "continuous")),
    breaks = c("7-Tages\nInzidenz", "logarithmischer\nR-Wert"),
    name = "") +
  scale_x_date(date_breaks = "1 months",
               limits = c(as.Date("2020/03/01", "%Y/%m/%d"), as.Date("2021/08/31", "%Y/%m/%d")),
               date_labels = "%m-%d") +
  themeMod(11) +
  theme(axis.text.x  = element_text(angle=90, vjust=.5, hjust=1),
        panel.grid.major.y = element_blank(),
        plot.caption=element_text(hjust = 0),
        plot.title.position = "plot") 

ggsave(file.path("output", paste0("Describe_data_onset_logR.pdf")), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", paste0("Describe_data_onset_logR.png")), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", paste0("Describe_data_onset_logR.emf")), 
       width = 11, height = 5, dpi = 1200)


ggplot(data = model_d, aes(x = date)) +
  geom_line(aes(y = Inz7T, color = "7-Tages\nInzidenz")) +
  geom_line(aes(y = logR *max(Inz7T)/1.5, color = "logarithmischer\nR-Wert")) +
  geom_hline(yintercept = 0, color = "black") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
                     sec.axis = sec_axis(~ . /max(model_d$Inz7T)*1.5, 
                                         name = "logarithmischer R-Wert")) +
  labs(y = "7-Tages-Inzidenz",
       x = "Datum der Jahre 2020 und 2021") + 
  scale_color_manual(
    values = rev(wes_palette("Darjeeling1", 
                             2, type = "continuous")),
    breaks = c("7-Tages\nInzidenz", "logarithmischer\nR-Wert"),
    labels = c("7-Tages-\nInzidenz", "logarithmischer\n R-Wert"),
    name = "") +
  scale_x_date(date_breaks = "1 months",
               limits = c(as.Date("2020/03/01", "%Y/%m/%d"), as.Date("2021/08/31", "%Y/%m/%d")),
               date_labels = "%m-%d") +
  themeMod(11) +
  theme(axis.text.x  = element_text(angle=90, vjust=.5, hjust=1),
        panel.grid.major.y = element_blank(),
        plot.caption=element_text(hjust = 0),
        plot.title.position = "plot") 

ggsave(file.path("output", paste0("Describe_data_onset_logR_e.pdf")), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", paste0("Describe_data_onset_logR_e.png")), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", paste0("Describe_data_onset_logR_e.emf")), 
       width = 11, height = 5, dpi = 1200)



ggplot(data = model_d, aes(x = date)) +
  geom_line(aes(y = Inz7T, color = "7-Tages\nInzidenz"), size = 1.5) +
  geom_line(aes(y = logR *max(Inz7T)/1.5, color = "logarithmischer\nR-Wert"), size = 1.5) +
  geom_hline(yintercept = 0, color = "black") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
                     sec.axis = sec_axis(~ . /max(model_d$Inz7T)*1.5, 
                                         name = "logarithmischer R-Wert")) +
  labs(y = "7-Tage Inzidenz",
       x = "Datum der Jahre 2020 und 2021") + 
  scale_color_manual(
    values = rev(wes_palette("Darjeeling1", 
                             2, type = "continuous")),
    breaks = c("7-Tages\nInzidenz", "logarithmischer\nR-Wert"),
    name = "") +
  scale_x_date(date_breaks = "1 months",
               limits = c(as.Date("2020/03/01", "%Y/%m/%d"), as.Date("2021/08/31", "%Y/%m/%d")),
               date_labels = "%m-%d") +
  themeMod(16) +
  theme(axis.text.x  = element_text(angle=90, vjust=.5, hjust=1),
        panel.grid.major.y = element_blank(),
        plot.caption=element_text(hjust = 0),
        plot.title.position = "plot") 

ggsave(file.path("output", paste0("Describe_data_onset_logR_pptx.pdf")), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", paste0("Describe_data_onset_logR_pptx.png")), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", paste0("Describe_data_onset_logR_pptx.emf")), 
       width = 11, height = 5, dpi = 1200)



ggplot(data = model_d, aes(x = date)) +
  geom_line(aes(y = Inz7T, color = "7-Tages\nInzidenz")) +
  geom_line(aes(y = logR *max(Inz7T)/1.5, color = "logarithmischer\nR-Wert\n(steigend)"))+ #,
  # data = model_d %>% dplyr::filter(logR>=0)) +
  geom_line(aes(y = logRm *max(Inz7T)/1.5, color = "logarithmischer\nR-Wert\n(sinkend)")) +
  geom_hline(yintercept = 0, color = "black") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
                     sec.axis = sec_axis(~ . /max(model_d$Inz7T)*1.5, 
                                         name = "logarithmischer R-Wert")) +
  labs(y = "7-Tage Inzidenz",
       x = "Datum der Jahre 2020 und 2021") + 
  scale_color_manual(
    values = c(rev(wes_palette("Darjeeling1", 
                               2, type = "continuous")), "gray"),
    breaks = c("7-Tages\nInzidenz", "logarithmischer\nR-Wert\n(steigend)", 
               "logarithmischer\nR-Wert\n(sinkend)"),
    name = "") +
  scale_x_date(date_breaks = "1 months",
               limits = c(as.Date("2020/03/01", "%Y/%m/%d"), as.Date("2021/08/31", "%Y/%m/%d")),
               date_labels = "%m-%d") +
  themeMod(11) +
  theme(axis.text.x  = element_text(angle=90, vjust=.5, hjust=1),
        panel.grid.major.y = element_blank(),
        plot.caption=element_text(hjust = 0),
        plot.title.position = "plot") 

ggsave(file.path("output", paste0("Describe_data_onset_logR_dir.pdf")), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", paste0("Describe_data_onset_logR_dir.png")), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", paste0("Describe_data_onset_logR_dir.emf")), 
       width = 11, height = 5, dpi = 1200)


ggplot(data = model_d, aes(x = date)) +
  geom_line(aes(y = Inz7T, color = "7-Tages\nInzidenz"), linewidth = 1.5) +
  geom_line(aes(y = logR *max(Inz7T)/1.5, color = "logarithmischer\nR-Wert\n(steigend)"), linewidth = 1.5)+ #,
  # data = model_d %>% dplyr::filter(logR>=0)) +
  geom_line(aes(y = logRm *max(Inz7T)/1.5, color = "logarithmischer\nR-Wert\n(sinkend)"), linewidth = 1.5) +
  geom_hline(yintercept = 0, color = "black") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
                     sec.axis = sec_axis(~ . /max(model_d$Inz7T)*1.5, 
                                         name = "logarithmischer R-Wert")) +
  labs(y = "7-Tage Inzidenz",
       x = "Datum der Jahre 2020 und 2021") + 
  scale_color_manual(
    values = c(rev(wes_palette("Darjeeling1", 
                               2, type = "continuous")), "orange"),
    breaks = c("7-Tages\nInzidenz", "logarithmischer\nR-Wert\n(steigend)", 
               "logarithmischer\nR-Wert\n(sinkend)"),
    name = "") +
  scale_x_date(date_breaks = "1 months",
               limits = c(as.Date("2020/03/01", "%Y/%m/%d"), 
                          as.Date("2021/08/31", "%Y/%m/%d")),
               date_labels = "%m-%d") +
  themeMod(16) +
  theme(axis.text.x  = element_text(angle=90, vjust=.5, hjust=1),
        panel.grid.major.y = element_blank(),
        plot.caption=element_text(hjust = 0),
        plot.title.position = "plot") 

ggsave(file.path("output", paste0("Describe_data_onset_logR_dir_pptx.pdf")), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", paste0("Describe_data_onset_logR_dir_pptx.png")), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", paste0("Describe_data_onset_logR_dir_pptx.emf")), 
       width = 11, height = 5, dpi = 1200)





ggplot(data = data_main, aes(x = date)) +
  geom_line(aes(y = Inz7T, color = "7-Tages\nInzidenz")) +
  geom_line(aes(y = R *max(Inz7T)/5, color = "R-Wert")) +
  geom_hline(yintercept = max(data_main$Inz7T)/5, color = "black") +
  facet_wrap(~ BL, nrow = 4) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", 
                                                 decimal.mark = ",", 
                                                 scientific = FALSE),
                     sec.axis = sec_axis(~ . /max(data_main$Inz7T)*5, 
                                         name = "R-Wert"),
                     limits = c(0, max(data_main$Inz7T))) +
  labs(y = "7-Tage Inzidenz",
       x = "Monat in 2020 und 2021") + 
  themeMod(11) +
  theme(panel.grid.major.y = element_blank(),
        plot.caption=element_text(hjust = 0),
        plot.title.position = "plot") +
  scale_color_manual(
    values = rev(wes_palette("Darjeeling1", 
                             2, type = "continuous")),
    breaks = c("7-Tages\nInzidenz", "R-Wert"),
    name = "") +
  scale_x_date(date_breaks = "1 months",
               limits = c(as.Date("2020/03/01", "%Y/%m/%d"), 
                          as.Date("2021/08/31", "%Y/%m/%d")),
               date_labels = "%b") +
  theme(axis.text.x  = element_text(angle=90, vjust=1))

ggsave(file.path("output", paste0("Describe_data_onset_bl.pdf")), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", paste0("Describe_data_onset_bl.png")), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", paste0("Describe_data_onset_bl.emf")), 
       width = 11, height = 5, dpi = 1200)




ggplot(data = data_main, aes(x = date)) +
  geom_line(aes(y = logR *max(Inz7T)/1.5, color = "logarithmischer\nR-Wert")) +
  geom_line(aes(y = Inz7T, color = "7-Tages\nInzidenz")) +
  geom_hline(yintercept = 0, color = "black") +
  facet_wrap(~ BL, nrow = 4) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", 
                                                 decimal.mark = ",", 
                                                 scientific = FALSE),
                     sec.axis = sec_axis(~ . /max(data_main$Inz7T)*1.5, 
                                         name = "logarithmischer R-Wert"),
                     limits = c(-max(data_main$Inz7T)/3, max(data_main$Inz7T))) +
  labs(y = "7-Tage Inzidenz",
       x = "Monat in 2020 und 2021") + 
  scale_color_manual(
    values = rev(wes_palette("Darjeeling1", 
                             2, type = "continuous")),
    breaks = c("7-Tages\nInzidenz", "logarithmischer\nR-Wert"),
    name = "") +
  scale_x_date(date_breaks = "1 months",
               limits = c(as.Date("2020/03/01", "%Y/%m/%d"), 
                          as.Date("2021/08/31", "%Y/%m/%d")),
               date_labels = "%b") +
  themeMod(11) +
  theme(axis.text.x  = element_text(angle=90, vjust=.5, hjust=1),
        panel.grid.major.y = element_blank(),
        plot.caption=element_text(hjust = 0),
        plot.title.position = "plot") 

ggsave(file.path("output", "Describe_data_onset_bl_logR.pdf"), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", "Describe_data_onset_bl_logR.png"), 
       width = 11, height = 5, dpi = 1200)
ggsave(file.path("output", "Describe_data_onset_bl_logR.emf"), 
       width = 11, height = 5, dpi = 1200)


