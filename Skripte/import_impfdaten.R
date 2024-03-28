library(tidyverse)
library(readr)
library(RODBC)
library(tidyquant)
library(ISOweek)
library(readxl)
library(fs)
library(magrittr)
library(lubridate)

rm(list = ls())

source("Skripte/helper_main.R")




url <- "https://zenodo.org/records/6559081/files/robert-koch-institut/COVID-19-Impfungen_in_Deutschland-2022-05-18.zip"
destination_path <- file.path("Daten", "COVID-19-Impfungen_in_Deutschland-2022-05-18a.zip")
decompressed_path <- file.path("Daten")

if (!file_exists(destination_path)) {
  # Download the file
  options(timeout=200)
  download.file(url, destfile = destination_path)
  
  # decompress the .zip file
  unzip(destination_path, 
        exdir = decompressed_path, 
        files = "robert-koch-institut-COVID-19-Impfungen_in_Deutschland-c2a413d/Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv")
}

lk_impf <- read_csv(file = file.path("Daten", 
                              "robert-koch-institut-COVID-19-Impfungen_in_Deutschland-c2a413d", 
                              "Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv"))

lk_impf %<>% dplyr::select(KrS = LandkreisId_Impfort, everything()) %>% as_tibble()

lk_impf %<>% dplyr::filter(KrS != "u")
lk_impf %<>% dplyr::filter(KrS != "17000")

lk_impf %<>% dplyr::group_by(Altersgruppe, Impfschutz, KrS) %>%
  dplyr::arrange(Altersgruppe, Impfschutz, KrS, Impfdatum) %>%
  dplyr::mutate(Anzahl = case_when(is.na(Anzahl) ~ 0,
                                   !is.na(Anzahl) ~ Anzahl),
                Id_BL = as.integer(substr(KrS, 1, 2)) ) %>%
  dplyr::arrange(Altersgruppe, Impfschutz, KrS, Impfdatum) %>%
  as_tibble()





##  Aggregieren auf Bundesländer -----------------------------


bl_impf <- lk_impf %>% group_by(Impfdatum, Altersgruppe, Impfschutz, Id_BL) %>% 
                       dplyr::summarise(Anzahl = sum(Anzahl, na.rm = T)) %>% as_tibble()

rm(lk_impf)

bevI <- read_csv2(file = file.path("Daten", "GENESIS-Online_Bevoelkerung_Kreise_Altersgruppen.csv"))
table(bevI$Altersgruppe)
bevI %<>% dplyr::mutate(Id_BL = as.integer(substr(Kreisschlüssel, 1, 2))) %>%
          dplyr::group_by(Id_BL, BL = Bundesland, ag = Altersgruppe) %>%
          dplyr::summarise(Bev = sum(Bevölkerung))


table(bl_impf$Altersgruppe)
bl_impf %<>% dplyr::mutate(AG = factor(Altersgruppe, labels = c("05-11", "12-17", "18-59", "60+")))

bl_empty <- crossing(Id_BL=1:16, 
                     AG = unique(bl_impf$AG),
                     Impfdatum = min(bl_impf$Impfdatum):max(bl_impf$Impfdatum),
                     Impfschutz = 1:3) %>% 
             dplyr::mutate(Impfdatum = as_date(Impfdatum, origin = lubridate::origin))

16*5*3*(max(bl_impf$Impfdatum) - min(bl_impf$Impfdatum)+1)


bl_impf <- left_join(bl_empty, bl_impf, by = c("Id_BL", "AG", "Impfdatum", "Impfschutz"))

bl_impf %<>% dplyr::group_by(AG, Impfschutz, Id_BL) %>%
             dplyr::arrange(AG, Impfschutz, Id_BL, Impfdatum) %>%
             dplyr::mutate(Anzahl = case_when(is.na(Anzahl) ~ 0,
                           !is.na(Anzahl) ~ Anzahl),
                           cum_Anzahl = cumsum(Anzahl)) %>%
             dplyr::arrange(AG, Impfschutz, Id_BL, Impfdatum) %>%
             as_tibble()


bl_impf %<>% dplyr::mutate(ag = case_when(
                    (AG %in% c("00-04", "05-11", "12-17")) ~ "00-17",
                    (AG %in% c("18-59")) ~ "18-59",
                    (AG %in% c("60+")) ~ "60+",
                    TRUE ~ as.character(NA)))

with(bl_impf, table(AG, ag, useNA = "always"))

bl_impf_ag <- bl_impf %>% 
  group_by(Id_BL, Impfdatum, ag, Impfschutz) %>% 
  dplyr::summarise(Anzahl = sum(Anzahl, na.rm = T)) %>% 
  as.data.frame()

bl_impf_ag <- left_join(bl_impf_ag, 
                     bevI, 
                     by = c("Id_BL", "ag"))

table(bl_impf_ag$BL, useNA = "always")

bl_impf_ag %<>% dplyr::group_by(BL, ag, Impfschutz) %>%
  dplyr::arrange(BL, ag, Impfschutz, Impfdatum) %>%
  dplyr::mutate(cum_Anzahl = cumsum(Anzahl),
                Impfquote = cum_Anzahl / Bev * 100) %>%
  dplyr::arrange(BL, ag, Impfschutz, Impfdatum) %>%
  as.data.frame()

save(bl_impf_ag, file = file.path("Daten", "bl_impf_ag.rData"))


##  Comparison Federal States
bl_impf_ag %>% dplyr::filter(Impfdatum==max(bl_impf_ag$Impfdatum) & 
                            Impfschutz==2) %>%
  dplyr::group_by(BL, Impfdatum) %>% 
  dplyr::summarise(Bev = sum(Bev, na.rm = T), 
                   cum_Anzahl = sum(cum_Anzahl, na.rm = T),
                   Impfquote = cum_Anzahl / Bev * 100) %>%
  dplyr::arrange(Impfquote) %>% print()
