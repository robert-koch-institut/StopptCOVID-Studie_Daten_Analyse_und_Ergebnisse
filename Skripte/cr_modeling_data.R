library(ggplot2)
library(tidyverse)
library(magrittr)
library(lubridate)
library(slider)

rm(list = ls())

source("Skripte/helper_main.R")


##   (1) Data Preparation -------------

load(file = file.path("Daten", "measures_simplified.rData"))


for (i in names(imputed_measures %>% dplyr::select(contains("_measures")))) {
  imputed_measures[[i]] <- factor(imputed_measures[[i]]) 
}

dummies <- model.matrix(~ . + 0, data = imputed_measures %>% 
                          dplyr::select(contains("_measures"))) %>% 
  as_tibble()


for (i in 1:ncol(dummies)) {
  le <- nchar(names(dummies)[[i]])
  k <- parse_number(names(dummies)[[i]])
  if (k<10) names(dummies)[[i]] <- paste(substr(names(dummies)[[i]], 1, le-1),
                                         substr(names(dummies)[[i]], le, le), sep = "_0")
  if (k>=10) names(dummies)[[i]] <- paste(substr(names(dummies)[[i]], 1, le-2),
                                          substr(names(dummies)[[i]], le-1, le), sep = "_")
}


imputed_measures %<>% dplyr::select(-contains("_measures"))
imputed_measures <- cbind(imputed_measures, dummies)
imputed_measures <- imputed_measures %>% dplyr::select(-ends_with("_01"))

rm(dummies)

table(imputed_measures$Id_BL, useNA = "always")



## (2) Aggregate to Federal States level ----------------------

bev <- read_csv2(file = file.path("Daten", "GENESIS-Online_Bevoelkerung_Kreise_Altersgruppen.csv"))
bev %<>% dplyr::mutate(Id_BL = as.integer(substr(Kreisschlüssel, 1, 2)),
                       ags5 = case_when(substr(Kreisschlüssel,1,2)=="11" ~ "11000",
                                        TRUE ~ Kreisschlüssel)) %>%
  dplyr::group_by(Id_BL, BL = Bundesland, ags5) %>%
  dplyr::summarise(Bev = sum(Bevölkerung)) %>% as_tibble()


imputed_measures <- left_join(imputed_measures, bev, by = c("ags5"))

imputed_measures %<>% dplyr::group_by(Id_BL, date) %>% 
                      dplyr::mutate(ges_Bev = sum(Bev),
                                    bev_weight = Bev / ges_Bev)

summary(imputed_measures$bev_weight)

bl_measures <- imputed_measures %>% ungroup() %>%
  dplyr::select(contains("measures_0"), contains("measures_1"), 
                Id_BL, date, bev_weight) %>% 
  group_by(Id_BL, date) %>% 
  dplyr::summarise(across(.cols = everything(),
                          .fns = ~ weighted.mean(., w = bev_weight, na.rm = TRUE)))

summary(bl_measures$school_measures_02)
summary(bl_measures$nightlife_measures_03)

modeling_bl <- imputed_measures %>% ungroup() %>% group_by(Id_BL, BL, date) %>% 
                dplyr::summarise(Bev = sum(Bev))

modeling_bl <- left_join(modeling_bl, bl_measures, 
                         by = c("Id_BL", "date"))

rm(bl_measures)
rm(imputed_measures)

modeling_bl %<>% dplyr::group_by(Id_BL) %>% 
  dplyr::arrange(Id_BL, date) %>% 
  dplyr::mutate(day_of_year = as.integer(date - min(date)))

summary(modeling_bl$day_of_year)





##  (3) Add Vaccination data ------------

load(file = file.path("Daten", "bl_impf_ag.rData"))

bl_impf_ag %<>% dplyr::mutate(ag = factor(ag, labels = c("0 bis 17 Jahre", 
                                                         "18 bis 59 Jahre", 
                                                         "60 und mehr Jahre")))

bl_impf <- bl_impf_ag %>% group_by(Impfdatum, Impfschutz, Id_BL, BL) %>% 
  dplyr::summarise(Bev = sum(Bev),
                   Anzahl = sum(Anzahl),
                   cum_Anzahl = sum(cum_Anzahl)) %>% 
  dplyr::mutate(Impfquote = cum_Anzahl / Bev *100) %>% as_tibble()


bl_impf %<>% dplyr::mutate(date = Impfdatum) 
bl_impf %<>% dplyr::select(Id_BL, BL, date, everything()) %>% dplyr::filter(Impfschutz<=2)
bl_impfw <- bl_impf %>% pivot_wider(id_cols = c(Id_BL, date, Bev),
                                    names_from = Impfschutz,
                                    values_from = c("Anzahl", "cum_Anzahl", # "Bev", 
                                                    "Impfquote"))


modeling_bl <- left_join(modeling_bl, 
                         bl_impfw %>% dplyr::select(Id_BL, date, check_Bev = Bev,
                                          starts_with("cum_Anzahl"), starts_with("Impfquote_")), 
                         by = c("Id_BL", "date")) %>%
                dplyr::select(Id_BL, BL, date, Bev, check_Bev, everything())


with(modeling_bl %>% dplyr::filter(!is.na(check_Bev)), stopifnot(Bev == check_Bev))
modeling_bl$check_Bev <- NULL

for (i in names(bl_impfw %>% dplyr::select(starts_with("cum_Anzahl"), starts_with("Impfquote_")))) {
  modeling_bl[[i]][is.na(modeling_bl[[i]])] <- 0
}

modeling_bl %<>% dplyr::group_by(Id_BL) %>%
  dplyr::arrange(Id_BL, date) %>%
  dplyr::mutate(per_vacc_1 = -log(1 - Impfquote_1/100)/log(2), 
                per_vacc_2 = -log(1 - Impfquote_2/100)/log(2)) %>% 
  dplyr::select(BL, Id_BL, date, 
                starts_with("cum_Anzahl"),
                starts_with("Impfquote_"), 
                starts_with("per_vacc"),
                everything()) %>%
  as_tibble()


modeling_bl <- add_dummy_seasonality(modeling_bl)



modeling_bl <- modeling_bl %>% dplyr::arrange(Id_BL, date)




## (4) CVD data by onset of disease -----------------

mean_onset_AG <- read_csv2(file.path("Daten", 
                                     "IfSG_COVID-19_Erkrankungsbeginn_Erwartungswert.csv"))

mean_onset_AG %<>%
  dplyr::select(BL = Bundesland, 
                date = Datum,
                AG = Altersgrupppe,
                N_onset = EW_Fallzahl) %>% 
  dplyr::mutate(N_onset = as.numeric(N_onset)) %>% 
  as_tibble()


mean_onset <- mean_onset_AG %>% dplyr::group_by(BL, date) %>% 
  dplyr::summarise(N_onset = sum(N_onset)) %>% as_tibble()

rm(mean_onset_AG)

summary(mean_onset$date)
summary(modeling_bl$date)
16 * length(unique(modeling_bl$date))
16 * length(unique(mean_onset$date))


mean_onset %<>% dplyr::filter(date >= as.Date("2020/03/01", "%Y/%m/%d") &
                                   date <= as.Date("2021/09/15", "%Y/%m/%d"))


mean_onset %<>% group_by(BL) %>% arrange(BL, date) %>% 
  dplyr::mutate(
    N_onset_smooth = slider::slide_dbl(N_onset, mean, .before = 6, .after = 0),
    lag4_N_onset = lag(N_onset_smooth, 4),
    R_ons_all = N_onset_smooth / lag4_N_onset,
    R_ons = case_when((N_onset_smooth<1) 
                      ~ NA_real_,
                      TRUE ~ R_ons_all),
    
  ) %>% ungroup()



modeling_bl <- left_join(modeling_bl, mean_onset, by = c("BL", "date"))
summary(modeling_bl$N_onset)



##  (5) Add Variant data ---------------------

load(file = file.path("Daten", "variants_data.rData"))

modeling_bl <- left_join(modeling_bl, 
                       seq_data_wide %>% 
                         dplyr::select(date = Entnahmedatum, Wildtype = ma_Other, Alpha = ma_Alpha, Delta = ma_Delta), 
                       by = c("date")) %>% as_tibble()



##  (6) Add holiday variables -------------

load(file = file.path("Daten", "holidays.rData"))

holidays$bundesland[holidays$bundesland=="Baden-Wuerttemberg"] <- "Baden-Württemberg"
holidays$bundesland[holidays$bundesland=="Thueringen"] <- "Thüringen"


modeling_bl <- left_join(modeling_bl, 
                         holidays %>% dplyr::select(BL = bundesland, everything()), 
                         by = c("BL", "date"))



modeling_bl %<>% dplyr::select(BL, Id_BL, date, starts_with("N_onset"), 
                               R_ons_all, R_ons, Wildtype, Alpha, Delta, 
                               contains("holiday"), easter_christmas,
                               everything()) %>%
  as_tibble()

save(modeling_bl, file = file.path("Daten", paste0("modeling_bl.rData")))

