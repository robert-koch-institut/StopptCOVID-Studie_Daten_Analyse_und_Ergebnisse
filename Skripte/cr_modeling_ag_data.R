library(dplyr)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(lubridate)
library(zoo)

source("Skripte/helper_main.R")


load(file = file.path("Daten", paste0("modeling_bl.rData")))

16 * (max(modeling_bl$date) - min(modeling_bl$date) + 1)


##   (1) Add case data by age group ---------------

mean_onset_AG <- read_csv2(file.path("Daten", 
                                     "IfSG_COVID-19_Erkrankungsbeginn_Erwartungswert.csv"))


#  Assumption that the dynamic of N_onset in age group 00-19 is a good proxy for the one in 00-17
#   and the the dynamic of N_onset in age group 20-59 is a good proxy for the one in 18-59.
mean_onset_AG %<>%
  dplyr::select(BL = Bundesland, 
                date = Datum,
                AG = Altersgrupppe,
                N_onset = EW_Fallzahl) %>% 
  dplyr::mutate(N_onset = as.numeric(N_onset),
                AG = recode(AG, "00-19"="00-17", "20-59"="18-59", "60+"="60+")) %>% 
  as_tibble()

mean_onset_AG %<>% dplyr::filter(date >= as.Date("2020/03/01", "%Y/%m/%d") &
                                   date <= as.Date("2021/09/15", "%Y/%m/%d"))


mean_onset_AG %<>% group_by(BL, AG) %>% arrange(BL, AG, date) %>% 
  dplyr::mutate(
    N_onset_smooth = slider::slide_dbl(N_onset, mean, .before = 6, .after = 0),
    lag4_N_onset = lag(N_onset_smooth, 4),
    R_ons_all = N_onset_smooth / lag4_N_onset,
    R_ons = case_when((N_onset_smooth<1) 
                      ~ NA_real_,
                      TRUE ~ R_ons_all),
    
  ) %>% ungroup()

mean_onset_AG %<>% dplyr::mutate(AG = factor(AG, labels = c("0 bis 17 Jahre", "18 bis 59 Jahre", "60 und mehr Jahre")))

16 * (max(modeling_bl$date) - min(modeling_bl$date) + 1) * 3

modeling_bl_ag <- left_join(mean_onset_AG, 
                            modeling_bl %>% dplyr::select(-c(Bev, bev_weight, contains("N_onset"), contains("R_ons"), 
                                                             starts_with("cum_Anzahl"), starts_with("Impfquote_"), starts_with("per_vacc_")) ), 
                            by = c("date", "BL"))



##  (2) Add vaccination data by age group  -----------------

load(file = file.path("Daten", "bl_impf_ag.rData"))

bl_impf_ag %<>% dplyr::mutate(ag = factor(ag, labels = c("0 bis 17 Jahre", 
                                                         "18 bis 59 Jahre", 
                                                         "60 und mehr Jahre")))


with(bl_impf_ag, stopifnot(Impfquote == cum_Anzahl / Bev *100))


bl_impf_ag %<>% dplyr::mutate(date = Impfdatum) 
bl_impf_ag %<>% dplyr::select(BL, AG = ag, date, everything()) %>% dplyr::filter(Impfschutz<=2)
bl_impf_agw <- bl_impf_ag %>% pivot_wider(id_cols = c(BL, AG, date, Bev),
                                          names_from = Impfschutz,
                                          values_from = c("Anzahl", "cum_Anzahl", 
                                                          "Impfquote"))




bl_impf_agw %<>% dplyr::filter(date <= as.Date("2021/09/15", "%Y/%m/%d"))

modeling_bl_ag <- left_join(modeling_bl_ag, 
                            bl_impf_agw %>% dplyr::select(BL, AG, date, Bev,
                                                          starts_with("cum_Anzahl"), starts_with("Impfquote_")), 
                            by = c("BL", "AG", "date"))

modeling_bl_ag %<>% dplyr::group_by(AG, BL) %>%
  dplyr::arrange(AG, BL, date) %>%
  dplyr::mutate(Bev = case_when((is.na(Bev) | Bev==0) ~ max(Bev, na.rm = T),
                                TRUE ~ Bev))

check <- modeling_bl_ag %>% dplyr::group_by(AG, BL) %>%
  dplyr::summarise(min_bev = min(Bev),
                   max_bev = max(Bev)
                   ) %>% as.data.frame()

with(check, stopifnot(min_bev == max_bev))
rm(check)



for (i in names(bl_impf_agw %>% dplyr::select(starts_with("cum_Anzahl"), starts_with("Impfquote_")))) {
  modeling_bl_ag[[i]][is.na(modeling_bl_ag[[i]])] <- 0
}

modeling_bl_ag %<>% dplyr::select(BL, AG, starts_with("cum_Anzahl"), starts_with("Impfquote_"), everything())


names(modeling_bl_ag) %<>%  str_replace_all("_categorical", "")

names(modeling_bl_ag) %<>% str_replace_all("_mean", "")



modeling_bl_ag %<>% dplyr::group_by(BL, AG) %>%
  dplyr::arrange(BL, AG, date) %>%
  dplyr::mutate(per_vacc_1 = -log(1 - Impfquote_1/100)/log(2), 
                per_vacc_2 = -log(1 - Impfquote_2/100)/log(2)) %>%
  as_tibble()


save(modeling_bl_ag, file = file.path("Daten", paste0("modeling_bl_ag.rData")))


