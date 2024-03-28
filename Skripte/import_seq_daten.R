library(tidyverse)
library(readr)
library(RODBC)
library(tidyquant)
library(ISOweek)
library(readxl)
library(fs)
library(magrittr)
library(slider)
library(R.utils)

rm(list = ls())

source("Skripte/helper_main.R")

url <- "https://zenodo.org/records/10813808/files/SARS-CoV-2-Sequenzdaten_Deutschland.tsv.xz"
destination_path <- file.path("Daten", "sequences.tsv.xz")
decompressed_path <- sub("\\.xz$", "", destination_path)

if (!file_exists(decompressed_path)) {
  if (!file_exists(destination_path)) {
    download.file(url, destfile = destination_path)
  }

  # Decompress the .xz file
  gunzip(destination_path, destname = decompressed_path, remove = FALSE)
}

var_data <- read_tsv("https://zenodo.org/records/10813808/files/SARS-CoV-2-Entwicklungslinien_zu_Varianten.tsv")

var_data %<>% dplyr::group_by(CONTRIBUTING_LINEAGES) %>% dplyr::mutate(n = n()) %>% as_tibble()
with(var_data, table(n, variant_category))
var_data %<>% dplyr::filter(variant_category != "VOI")
var_data$n <- NULL

seq_data <- read_tsv(destination_path)

seq_data %<>% dplyr::select(Entnahmedatum = SEQUENCE.DATE_OF_SAMPLING, 
                            Grund = SEQUENCE.SEQUENCING_REASON,
                            CONTRIBUTING_LINEAGES = PANGOLIN.LINEAGE_LATEST,
                            everything()) %>%
  dplyr::filter(Entnahmedatum <= as.Date("2021/09/15", "%Y/%m/%d"),
                Grund == "N")


seq_data <- left_join(seq_data, var_data, by = c("CONTRIBUTING_LINEAGES"))

seq_data %<>% dplyr::mutate(kw = isoweek(Entnahmedatum),
                            kwj = isoyear(Entnahmedatum),
                            voc = case_when(year(Entnahmedatum) < 2021 ~ "Other",
                                            WHO_LABEL %in% c("Alpha", "Delta") ~ WHO_LABEL,
                                            TRUE ~ "Other")
                            )

seq_data %<>% ungroup() %>% dplyr::group_by(kwj, kw, Entnahmedatum, voc) %>% 
  dplyr::summarise(n_voc = n())
summary(seq_data$n_voc)
seq_data %<>% dplyr::group_by(voc, kwj, kw) %>% 
              dplyr::mutate(voc_kw = sum(n_voc)) %>% as_tibble()


seq_data_wide <- pivot_wider(seq_data %>% dplyr::select(Entnahmedatum, kwj, kw, voc, voc_kw), 
                             names_from = voc, 
                             values_from = voc_kw)

date_empty <- tibble(Entnahmedatum = seq(min(seq_data_wide$Entnahmedatum), max(seq_data_wide$Entnahmedatum), by = 1))
seq_data_wide <- left_join(date_empty,
                           seq_data_wide,
                           by = c("Entnahmedatum"))

seq_data_wide %<>% dplyr::mutate(Other = case_when(is.na(Other) ~ 0,
                                  TRUE ~ Other),
                Alpha = case_when(is.na(Alpha) ~ 0,
                                  TRUE ~ Alpha),
                Delta = case_when(is.na(Delta) ~ 0,
                                  TRUE ~ Delta),
                Gesamt = Other + Alpha + Delta,
                p_Other = Other / Gesamt,
                p_Alpha = Alpha / Gesamt,
                p_Delta = Delta / Gesamt)

seq_data_wide %<>% dplyr::mutate(ma_Other = slider::slide_dbl(p_Other, ~mean(.x, na.rm = TRUE), .before = 6, .after = 0, na.rm = T),
                ma_Alpha = slider::slide_dbl(p_Alpha, ~mean(.x, na.rm = TRUE), .before = 6, .after = 0, na.rm = T),
                ma_Delta = slider::slide_dbl(p_Delta, ~mean(.x, na.rm = TRUE), .before = 6, .after = 0, na.rm = T))

summary(seq_data_wide %>% dplyr::filter(is.na(ma_Other)) %>% dplyr::select(Entnahmedatum))

seq_data_wide %<>% dplyr::mutate(ma_Other = case_when(is.na(ma_Other) ~ 1,
                                                   TRUE ~ ma_Other),
                                 ma_Alpha = case_when(is.na(ma_Alpha) ~ 0,
                                                   TRUE ~ ma_Alpha),
                                 ma_Delta = case_when(is.na(ma_Delta) ~ 0,
                                                   TRUE ~ ma_Delta))
                                 

save(seq_data_wide, file = file.path("Daten", "variants_data.rData"))
