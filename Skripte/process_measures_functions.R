
clean_measures_fine_grained <- function(measures){
  
  measures_transformed <- compute_school_measures_categorical(measures)
  measures_transformed <- compute_retail_measures_categorical(measures_transformed)
  measures_transformed <- compute_private_space_measures_categorical(measures_transformed)
  measures_transformed <- compute_gastronomy_measures_categorical(measures_transformed)
  measures_transformed <- compute_public_event_indoor_measures_categorical(measures_transformed)
  measures_transformed <- compute_nightlife_measures_categorical(measures_transformed)
  measures_transformed <- compute_workplace_measures_categorical(measures_transformed)
  measures_transformed <- compute_culture_education_measures_categorical(measures_transformed)
  measures_transformed <- compute_daycare_measures_categorical(measures_transformed)
  measures_transformed <- compute_primary_school_measures_categorical(measures_transformed)
  measures_transformed <- compute_public_space_measures_categorical(measures_transformed)
  measures_transformed <- compute_public_event_outdoor_measures_categorical(measures_transformed)
  measures_transformed <- compute_travel_domestic_measures_categorical(measures_transformed)
  measures_transformed <- compute_travel_foreign_measures_categorical(measures_transformed)#TODO
  measures_transformed <- compute_mask_measures_categorical(measures_transformed)
  measures_transformed <- compute_services_measures_categorical(measures_transformed)
  measures_transformed <- compute_hospitality_measures_categorical(measures_transformed)
  measures_transformed <- compute_sport_indoor_measures_categorical(measures_transformed)
  measures_transformed <- compute_sport_outdoor_measures_categorical(measures_transformed)
  measures_transformed <- compute_curfew_measures_categorical(measures_transformed)
  measures_transformed <- compute_abstand_measures_categorical(measures_transformed)
  measures_transformed <- compute_test_measures_categorical(measures_transformed) 
  
  return(measures_transformed)
}

acceptable_answers <- c(1,2,3,4,5)


compute_school_measures_categorical <- function(measures_data) {
  #' Compute school severity index on infas data.
  
  measures_data <-  measures_data %>%
    mutate(
      school_measures_categorical = case_when(
        M02a_040 %in% acceptable_answers ~ "10 vollstaendige Schliessung",
        M02a_036 %in% acceptable_answers ~ "09 Selektiv 4 bestimmte Faecher",
        M02a_035 %in% acceptable_answers ~ "08 Selektiv 3 Notbetrieb nach Elterngruppen",
        M02a_034 %in% acceptable_answers ~ "07 Selektiv 2 Jahrgangsstufen",
        M02a_033 %in% acceptable_answers ~ "06 Selektiv 1 Abschlussklassen",
        M02a_032 %in% acceptable_answers ~ "05 Zeitversetzt 2",
        M02a_031 %in% acceptable_answers ~ "04 Zeitversetzt 1",
        M02a_030 %in% acceptable_answers ~ "03 eingeschr. Oeffnung gemaess Hygienekonzept",
        M02a_020 %in% acceptable_answers ~ "02 vollst. Oeffnung gemaess Hygienekonzept",
        M02a_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "11 NA"
      )
    )
  return(measures_data)
}

compute_retail_measures_categorical <- function(measures_data){
  #' Compute retail severity index on infas data.
  
  measures_data <-  measures_data %>%
    mutate(
      retail_measures_categorical = case_when(
        M07_040 %in% acceptable_answers ~ "09 Vollstaendige Schliessung",
        M07_031 %in% acceptable_answers ~ "08 Nur kritische Versorgung",
        M07_032 %in% acceptable_answers ~ "07 Nur kritische Versorgung inkl. Buch- und Blumenhandel",
        M07_036 %in% acceptable_answers ~ "06 Nur bestimme Oeffnungszeiten",
        M07_034 %in% acceptable_answers ~ "05 Max Flaeche 700qm",
        M07_033 %in% acceptable_answers ~ "04 Max Flaeche 800qm",
        M07_030 %in% acceptable_answers ~ "03 Eingeschraenkte Oeffnung gemaess Hygienekonzept (oberkategorie von allen 30ern)",
        M07_020 %in% acceptable_answers ~ "02 Vollstaendige Oeffnung gemaess Hygienekonzept",
        M07_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "10 NA"
      )
    )
  return(measures_data)
}

compute_private_space_measures_categorical <- function(measures_data){
  #' Compute private space index on infas data.
  
  measures_data <-  measures_data %>%
    mutate(
      private_space_measures_categorical = case_when(
        M01a_140 %in% acceptable_answers ~ "10 Haushalt",
        M01a_130 %in% acceptable_answers ~ "09 2 Pers aus 2 Haushalten",
        M01a_150 %in% acceptable_answers ~ "08 2 Haushalte",
        M01a_120 %in% acceptable_answers ~ "07 5 Pers",
        M01a_110 %in% acceptable_answers ~ "06 10 Pers",
        M01a_100 %in% acceptable_answers ~ "05 20 Pers",
        M01a_090 %in% acceptable_answers ~ "04 50 Pers",
        M01a_080 %in% acceptable_answers ~ "03 100 Pers",
        M01a_020 %in% acceptable_answers ~ "02 Empfehlung Vermeidung soz. Kontakte",
        M01a_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "11 NA"
      )
    )
  return(measures_data)
}

compute_gastronomy_measures_categorical <- function(measures_data){
  #' Compute gastronomy severity index on infas data.
  
  measures_data <-  measures_data %>%
    mutate(
      gastronomy_measures_categorical = case_when(
        M08_040 %in% acceptable_answers ~ "07 Vollstaendige Schliessung",
        M08_033 %in% acceptable_answers ~ "06 Nur Abholung",
        M08_032 %in% acceptable_answers ~ "05 Vor-Ort-Verzehr nur im Aussenbereich",
        M08_031 %in% acceptable_answers ~ "04 Nur zu bestimmten Zeiten geoeffnet",
        M08_030 %in% acceptable_answers ~ "03 Eingeschraenkte Oeffnung gemaess Hygienekonzept",
        M08_020 %in% acceptable_answers ~ "02 Vollstaendige Oeffnung gemaess Hygienekonzept",
        M08_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "08 NA"
      )
    )
  return(measures_data)
}

compute_public_event_indoor_measures_categorical <- function(measures_data){
  #' Compute public event severity index on infas data.
  
  measures_data <-  measures_data %>%
    mutate(
      public_event_indoor_measures_categorical = case_when(
        M04_140 %in% acceptable_answers ~ "11 Vollstaendig verboten",
        M04_130 %in% acceptable_answers ~ "10 bis 5",
        M04_120 %in% acceptable_answers ~ "09 bis 10",
        M04_110 %in% acceptable_answers ~ "08 bis 20",
        M04_100 %in% acceptable_answers ~ "07 bis 30",
        M04_090 %in% acceptable_answers ~ "06 bis 50",
        M04_080 %in% acceptable_answers ~ "05 bis 100",
        M04_070 %in% acceptable_answers ~ "04 bis 200",
        M04_060 %in% acceptable_answers ~ "03 bis 500",
        M04_050 %in% acceptable_answers ~ "02 bis 1000",
        M04_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "12 NA"
      )
    )
  return(measures_data)
}

compute_nightlife_measures_categorical <-function(measures_data){
  #' Compute nightlife severity index on infas data.
  
  measures_data <-  measures_data %>%
    mutate(
      nightlife_measures_categorical = case_when(
        M10_040 %in% acceptable_answers ~ "07 Vollst. Schliessung",
        M10_033 %in% acceptable_answers ~ "06 Vollstaendige oder eingeschraenkte Oeffnung gemaess Hygienevorschriften",
        M10_032 %in% acceptable_answers ~ "05 Vollstaendige oder eingeschraenkte Oeffnung gemaess Hygienevorschriften",
        M10_031 %in% acceptable_answers ~ "04 Vollstaendige oder eingeschraenkte Oeffnung gemaess Hygienevorschriften",
        M10_030 %in% acceptable_answers ~ "03 Vollstaendige oder eingeschraenkte Oeffnung gemaess Hygienevorschriften",
        M10_020 %in% acceptable_answers ~ "02 Vollstaendige oder eingeschraenkte Oeffnung gemaess Hygienevorschriften",
        M10_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "08 NA"
      )
    )
  return(measures_data)
}

compute_workplace_measures_categorical <- function(measures_data){
  #' Compute workplace severity index on infas data.

  measures_data <-  measures_data %>%
    mutate(
      workplace_measures_categorical = case_when(
        M17_030 %in% acceptable_answers ~ "03 Einschraenkungsempfehlung, Vorschriften, teilweise Schliessung",
        M17_020 %in% acceptable_answers ~ "02 Einschraenkungsempfehlung, Vorschriften, teilweise Schliessung",
        M17_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "04 NA"
      )
    )
  return(measures_data)
}

compute_culture_education_measures_categorical <-function(measures_data){
  #' Compute culture and education severity index on infas data.
  
  measures_data <-  measures_data %>%
    mutate(
      culture_education_measures_categorical = case_when(
        M06_040 %in% acceptable_answers ~ "08 Voll. Schliessung",
        M06_034 %in% acceptable_answers ~ "07 Eingeschr. Oeffnung gemaess Hygienevorschriften",
        M06_033 %in% acceptable_answers ~ "06 Eingeschr. Oeffnung gemaess Hygienevorschriften",
        M06_032 %in% acceptable_answers ~ "05 Eingeschr. Oeffnung gemaess Hygienevorschriften",
        M06_031 %in% acceptable_answers ~ "04 Eingeschr. Oeffnung gemaess Hygienevorschriften",
        M06_030 %in% acceptable_answers ~ "03 Eingeschr. Oeffnung gemaess Hygienevorschriften",
        M06_020 %in% acceptable_answers ~ "02 Voll. Oeffnung gemaess Hygienevorschriften",
        M06_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "09 NA"
      )
    )
  return(measures_data)
}

compute_daycare_measures_categorical <- function(measures_data){
  #' Compute daycare severity index on infas data.
  
  measures_data <-  measures_data %>%
    mutate(
      daycare_measures_categorical = case_when(
        M03_060 %in% acceptable_answers ~ "05 Notbetreuung (nur systemrelevant)",
        M03_050 %in% acceptable_answers ~ "05 Notbetreuung (nur systemrelevant)",
        M03_040 %in% acceptable_answers ~ "04 erweiterte Notbetreuung",
        M03_030 %in% acceptable_answers ~ "03 Eingeschr. Betrieb",
        M03_020 %in% acceptable_answers ~ "02 Volle Oeffnung gemaess Hygienevorschriften",
        M03_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "06 NA"
      )
    )
  return(measures_data)
}

compute_primary_school_measures_categorical <- function(measures_data){
  #' Compute primary_school severity index on infas data.
  
  measures_data <-  measures_data %>%
    mutate(
      primary_school_measures_categorical = case_when(
        M02b_040 %in% acceptable_answers ~ "10 Volle Schliessung",
        M02b_036 %in% acceptable_answers ~ "09 Eingeschr. Oeffnung gem. Hygienevorschriften",
        M02b_035 %in% acceptable_answers ~ "08 Eingeschr. Oeffnung gem. Hygienevorschriften",
        M02b_034 %in% acceptable_answers ~ "07 Eingeschr. Oeffnung gem. Hygienevorschriften",
        M02b_033 %in% acceptable_answers ~ "06 Eingeschr. Oeffnung gem. Hygienevorschriften",
        M02b_032 %in% acceptable_answers ~ "05 Eingeschr. Oeffnung gem. Hygienevorschriften",
        M02b_031 %in% acceptable_answers ~ "04 Eingeschr. Oeffnung gem. Hygienevorschriften",
        M02b_030 %in% acceptable_answers ~ "03 Eingeschr. Oeffnung gem. Hygienevorschriften",
        M02b_020 %in% acceptable_answers ~ "02 Volle Oeffnung gem. Hygienevorschriften",
        M02b_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "11 NA"
      )
    )
  return(measures_data)
}

compute_public_space_measures_categorical <- function(measures_data){
  #' Compute public_space severity index on infas data.
  
  measures_data <-  measures_data %>%
    mutate(
      public_space_measures_categorical = case_when(
        M01b_150 %in% acceptable_answers ~ "10 1-2 Haushalte o. aehnlich",
        M01b_140 %in% acceptable_answers ~ "09 1-2 Haushalte o. aehnlich",
        M01b_130 %in% acceptable_answers ~ "08 1-2 Haushalte o. aehnlich",
        M01b_120 %in% acceptable_answers ~ "07 5-10 Personen",
        M01b_110 %in% acceptable_answers ~ "06 5-10 Personen",
        M01b_100 %in% acceptable_answers ~ "05 20-100 Personen",
        M01b_090 %in% acceptable_answers ~ "04 20-100 Personen",
        M01b_080 %in% acceptable_answers ~ "03 20-100 Personen",
        M01b_020 %in% acceptable_answers ~ "02 Empf. Vermeid. soz. Kontakte",
        M01b_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "11 NA"
      )
    )
  return(measures_data)
}


compute_public_event_outdoor_measures_categorical <-function(measures_data){
  #' Compute public event outdoor severity index on infas data.

  measures_data <-  measures_data %>%
    mutate(
      public_event_outdoor_measures_categorical = case_when(
        M05_160 %in% acceptable_answers ~ "13 Vollstaendig verboten",
        M05_150 %in% acceptable_answers ~ "12 bis 10",
        M05_140 %in% acceptable_answers ~ "11 bis 20",
        M05_130 %in% acceptable_answers ~ "10 bis 50",
        M05_120 %in% acceptable_answers ~ "09 bis 100",
        M05_110 %in% acceptable_answers ~ "08 bis 200",
        M05_100 %in% acceptable_answers ~ "07 bis 300",
        M05_090 %in% acceptable_answers ~ "06 bis 400",
        M05_080 %in% acceptable_answers ~ "05 bis 500",
        M05_070 %in% acceptable_answers ~ "04 bis 700",
        M05_060 %in% acceptable_answers ~ "03 bis 1000",
        M05_050 %in% acceptable_answers ~ "02 bis 5000",
        M05_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "14 NA"
      )
    )
  return(measures_data)
}

compute_travel_domestic_measures_categorical <- function(measures_data){
  #' Compute travel_domestic severity index on infas data.
  
  measures_data <-  measures_data %>%
    mutate(
      travel_domestic_measures_categorical = case_when(
        M14_030 %in% acceptable_answers ~ "03 Einreiseverbot in andere B.laender",
        M14_020 %in% acceptable_answers ~ "02 Vorraussetzung, z.B. Radius",
        M14_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "04 NA"
      )
    )
  return(measures_data)
}

compute_travel_foreign_measures_categorical <- function(measures_data){
  #' Compute travel_foreign severity index on infas data.
  
  measures_data <-  measures_data %>%
    mutate(
      travel_foreign_measures_categorical = case_when(
        M15_030 %in% acceptable_answers ~ "03 nur bestimmte Länder",
        M15_020 %in% acceptable_answers ~ "02 nur EU",
        M15_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "04 NA"
      )
    )
  return(measures_data)
}

compute_mask_measures_categorical <- function(measures_data){
  #' Compute mask severity index on infas data.
  
  measures_data <-  measures_data %>%
    mutate(
      mask_measures_categorical = case_when(
        M16_100 %in% acceptable_answers ~ "08 Maskenpflicht strafbewehrt",
        M16_090 %in% acceptable_answers ~ "07 Maskenpflicht Grundschulen",
        M16_080 %in% acceptable_answers ~ "07 Maskenpflicht Grundschulen",
        M16_070 %in% acceptable_answers ~ "06 Maskenpflicht weiterf. Schulen",
        M16_060 %in% acceptable_answers ~ "06 Maskenpflicht weiterf. Schulen",
        M16_050 %in% acceptable_answers ~ "05 Maskenpflicht öffentlicher Raum",
        M16_040 %in% acceptable_answers ~ "04 Maskenpflicht Verkaufsstätten",
        M16_030 %in% acceptable_answers ~ "03 Maskenpflicht ÖPNV",
        M16_020 %in% acceptable_answers ~ "02 Empfehlung",
        M16_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "09 NA"
      )
    )
  return(measures_data)
}

compute_services_measures_categorical <- function(measures_data){
  #' Compute services severity index on infas data.
  
  measures_data <-  measures_data %>%
    mutate(
      services_measures_categorical = case_when(
        M09_040 %in% acceptable_answers ~ "07 Hauptsaechliche oder komplette Schliessung",
        M09_033 %in% acceptable_answers ~ "06 Hauptsaechliche oder komplette Schliessung",
        M09_032 %in% acceptable_answers ~ "05 Hauptsaechliche oder komplette Schliessung",
        M09_031 %in% acceptable_answers ~ "04 eingeschr. Oeffnung",
        M09_030 %in% acceptable_answers ~ "03 eingeschr. Oeffnung",
        M09_020 %in% acceptable_answers ~ "02 Vollst. Oeffnung gm. Hygienevorschrift",
        M09_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "08 NA"
      )
    )
  return(measures_data)
}

compute_hospitality_measures_categorical <- function(measures_data){
  #' Compute hospitality severity index on infas data.

  measures_data <-  measures_data %>%
    mutate(
      hospitality_measures_categorical = case_when(
        M11_040 %in% acceptable_answers ~ "04 touristisches o. allgemeines Beherbergungsverbot",
        M11_030 %in% acceptable_answers ~ "03 touristisches o. allgemeines Beherbergungsverbot",
        M11_020 %in% acceptable_answers ~ "02 unter Auflagen",
        M11_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "05 NA"
      )
    )
  return(measures_data)
}

compute_sport_indoor_measures_categorical <- function(measures_data){
  #' Compute sport indoor severity index on infas data.
  
  measures_data <-  measures_data %>%
    mutate(
      sport_indoor_measures_categorical = case_when(
        M12_070 %in% acceptable_answers ~ "05 Verbot Sportbetrieb",
        M12_040 %in% acceptable_answers ~ "04 begr. Pers.anzahl, kontaktfrei",
        M12_030 %in% acceptable_answers ~ "03 begr. Pers.anzahl",
        M12_020 %in% acceptable_answers ~ "02 begr. Pers.anzahl",
        M12_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "06 NA"
      )
    )
  return(measures_data)
}

compute_sport_outdoor_measures_categorical <- function(measures_data){
  #' Compute sport outdoor severity index on infas data.
  
  measures_data <-  measures_data %>%
    mutate(
      sport_outdoor_measures_categorical = case_when(
        M13_070 %in% acceptable_answers ~ "05 nur Individualsport",
        M13_040 %in% acceptable_answers ~ "04 nur Individualsport",
        M13_030 %in% acceptable_answers ~ "03 begr. Pers.anzahl, kontaktfrei",
        M13_020 %in% acceptable_answers ~ "02 begr. Pers.anzahl",
        M13_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "06 NA"
      )
    )
  return(measures_data)
}

compute_curfew_measures_categorical <- function(measures_data){
  #' Compute curfew severity index on infas data.
  
  measures_data <-  measures_data %>%
    mutate(
      curfew_measures_categorical = case_when(
        M18_040 %in% acceptable_answers ~ "04 teilweise o. vollst. Ausgangsbeschraenkung",
        M18_030 %in% acceptable_answers ~ "03 teilweise o. vollst. Ausgangsbeschraenkung",
        M18_020 %in% acceptable_answers ~ "02 Empfehlung",
        M18_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "05 NA"
      )
    )
  return(measures_data)
}

compute_abstand_measures_categorical <- function(measures_data){
  #' Compute Abstand severity index on infas data.
  
  measures_data <-  measures_data %>%
    mutate(
      abstand_measures_categorical = case_when(
        M20_020 %in% acceptable_answers ~ "02 Abstandsregelung",
        M20_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "03 NA"
      )
    )
  return(measures_data)
}

compute_test_measures_categorical <- function(measures_data){
  #' Compute test severity index on infas data.
  
  measures_data <-  measures_data %>%
    mutate(
      test_measures_categorical = case_when(
        M21_080 %in% acceptable_answers ~ "08 bei Einreise/Schulen",
        M21_070 %in% acceptable_answers ~ "07 bei Einreise/Schulen",
        M21_060 %in% acceptable_answers ~ "06 bei Einreise/Schulen",
        M21_050 %in% acceptable_answers ~ "05 bei Einreise/Schulen",
        M21_040 %in% acceptable_answers ~ "04 für Veranstaltungen",
        M21_030 %in% acceptable_answers ~ "03 für Veranstaltungen",
        M21_020 %in% acceptable_answers ~ "02 bei Symptomen",
        M21_010 %in% acceptable_answers ~ "01 Keine Beschraenkung",
        TRUE ~ "09 NA"
      )
    )
  return(measures_data)
}


impute_NAs <- function(modeling_data){
  # Impute "01 Keine Beschraenkung" on 2020-03-01, if missing.
  # Then forward fill all missing values, respecting county and time-order
  
  variables_with_lag <- modeling_data %>%
    dplyr::select(contains("measures_categorical")) %>%
    names()
  
  # replace placeholder NAs with real NAs
  modeling_data <-
    modeling_data %>%
    mutate(
      across(
        .cols = {{variables_with_lag}},
        .fns = ~if_else(.x %in% c("03 NA", "04 NA", "05 NA", "06 NA", "07 NA", "08 NA", "09 NA", "10 NA", "11 NA", "12 NA", "13 NA", "14 NA"), NA_character_, .x)
      )
    )
  
  # Impute "01 Keine Beschraenkung" on 2020-03-01 
  modeling_data <-
    modeling_data %>%
    mutate(
      across(
        .cols = {{variables_with_lag}},
        .fns = ~if_else((is.na(.x)) & (date == lubridate::ymd("2020-03-01")), "01 Keine Beschraenkung", .x)
      )
    )
  
  # Forward fill missing values
  modeling_data <-
    modeling_data %>%
    group_by(ags5) %>%
    tidyr::fill({{variables_with_lag}}, .direction = "downup") %>%
    ungroup()
  
  # Basic check for mistakes
  modeling_data %>%
    group_by(ags5) %>%
    summarise(
      across(
        .cols = {{variables_with_lag}},
        .fns = ~sum(is.na(.x)))) %>%
    rowwise(ags5) %>%
    summarise(sum = sum(c_across({{variables_with_lag}}))) %>%
    filter(sum != 0) %>%
    pull(ags5) -> landkreise_with_missings
  
  if(length(landkreise_with_missings) > 0){
    stop(paste0("Imputation with procedure 'downup' failed. The following counties contained no data for some measures: ", paste(landkreise_with_missings, collapse = ", ")))
  }
  
  return(modeling_data)
}