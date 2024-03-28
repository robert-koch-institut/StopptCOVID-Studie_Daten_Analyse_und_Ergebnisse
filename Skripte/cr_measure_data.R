library(tidyverse)
library(ggplot2)
library(magrittr)
library(lubridate)

rm(list = ls())

source("Skripte/helper_main.R")


###   (1) Data Preparation -------------

load(file = file.path("Daten", "npi_measures.rData"))

table(imputed_measures[["nightlife_measures_categorical"]])

names(imputed_measures) %<>%
  str_replace_all("_categorical", "")

for (v in names(imputed_measures %>% dplyr::select(ends_with("measures")))) {
  imputed_measures %<>% dplyr::mutate(!!sym(v) := as.numeric(factor(!!sym(v))))
}

print(prop.table(table(imputed_measures[["nightlife_measures"]]))*100, digits = 2)


save(imputed_measures, file = file.path("Daten", "measures_districts.rData"))


simplify <- function(data = imputed_measures, measure = "curfew", place = 3) {
  change <- paste0(measure, "_measures")
  # print(table(data[[change]]))
  data %<>% dplyr::mutate(
    !!sym(change) := case_when(!!sym(change) > place ~ !!sym(change) - 1,
                               TRUE ~ !!sym(change)) )
  # print(table(data[[change]]))
  return(data)                                   
}

imputed_measures %<>% dplyr::mutate(
  school_measures = case_when(
    school_measures == 8 ~ 9,
    school_measures == 9 ~ 8,
    TRUE ~ school_measures),
  primary_school_measures = case_when(
    primary_school_measures == 8 ~ 9,
    primary_school_measures == 9 ~ 8,
    TRUE ~ primary_school_measures))

imputed_measures %<>% dplyr::mutate(
  public_event_outdoor2_measures = public_event_outdoor_measures,
  public_event_indoor2_measures = public_event_indoor_measures
)



imputed_measures <- simplify(measure = "private_space", place = 9)
imputed_measures <- simplify(measure = "private_space", place = 8)
imputed_measures <- simplify(measure = "private_space", place = 6)
imputed_measures <- simplify(measure = "private_space", place = 4)
imputed_measures <- simplify(measure = "private_space", place = 3)
imputed_measures <- simplify(measure = "public_space", place = 9)
imputed_measures <- simplify(measure = "public_space", place = 8)
imputed_measures <- simplify(measure = "public_space", place = 6)
imputed_measures <- simplify(measure = "public_space", place = 4)
imputed_measures <- simplify(measure = "public_space", place = 3)
imputed_measures <- simplify(measure = "public_event_indoor", place = 9)
imputed_measures <- simplify(measure = "public_event_indoor", place = 8)
imputed_measures <- simplify(measure = "public_event_indoor", place = 7)
imputed_measures <- simplify(measure = "public_event_indoor", place = 4)
imputed_measures <- simplify(measure = "public_event_indoor", place = 2)
imputed_measures <- simplify(measure = "public_event_outdoor", place = 11)
imputed_measures <- simplify(measure = "public_event_outdoor", place = 10)
imputed_measures <- simplify(measure = "public_event_outdoor", place = 8)
imputed_measures <- simplify(measure = "public_event_outdoor", place = 7)
imputed_measures <- simplify(measure = "public_event_outdoor", place = 6)
imputed_measures <- simplify(measure = "public_event_outdoor", place = 4)
imputed_measures <- simplify(measure = "public_event_outdoor", place = 2)
imputed_measures <- simplify(measure = "public_event_indoor2", place = 9)
imputed_measures <- simplify(measure = "public_event_indoor2", place = 8)
imputed_measures <- simplify(measure = "public_event_indoor2", place = 7)
imputed_measures <- simplify(measure = "public_event_indoor2", place = 6)
imputed_measures <- simplify(measure = "public_event_indoor2", place = 4)
imputed_measures <- simplify(measure = "public_event_indoor2", place = 2)
imputed_measures <- simplify(measure = "public_event_outdoor2", place = 11)
imputed_measures <- simplify(measure = "public_event_outdoor2", place = 10)
imputed_measures <- simplify(measure = "public_event_outdoor2", place = 8)
imputed_measures <- simplify(measure = "public_event_outdoor2", place = 7)
imputed_measures <- simplify(measure = "public_event_outdoor2", place = 6)
imputed_measures <- simplify(measure = "public_event_outdoor2", place = 5)
imputed_measures <- simplify(measure = "public_event_outdoor2", place = 3)
imputed_measures <- simplify(measure = "public_event_outdoor2", place = 2)
imputed_measures <- simplify(measure = "retail", place = 7)
imputed_measures <- simplify(measure = "retail", place = 6)
imputed_measures <- simplify(measure = "retail", place = 4)
imputed_measures <- simplify(measure = "school", place = 9)
imputed_measures <- simplify(measure = "school", place = 7)
imputed_measures <- simplify(measure = "school", place = 6)
imputed_measures <- simplify(measure = "school", place = 4)
imputed_measures <- simplify(measure = "school", place = 3)
imputed_measures <- simplify(measure = "primary_school", place = 9)
imputed_measures <- simplify(measure = "primary_school", place = 7)
imputed_measures <- simplify(measure = "primary_school", place = 6)
imputed_measures <- simplify(measure = "primary_school", place = 4)
imputed_measures <- simplify(measure = "primary_school", place = 3)
imputed_measures <- simplify(measure = "daycare", place = 4)
imputed_measures <- simplify(measure = "culture_education", place = 5)
imputed_measures <- simplify(measure = "culture_education", place = 3)
imputed_measures <- simplify(measure = "services", place = 6)
imputed_measures <- simplify(measure = "services", place = 3)
imputed_measures <- simplify(measure = "nightlife", place = 5)
imputed_measures <- simplify(measure = "nightlife", place = 4)
imputed_measures <- simplify(measure = "nightlife", place = 3)
imputed_measures <- simplify(measure = "nightlife", place = 2)
imputed_measures <- simplify(measure = "hospitality", place = 3)
imputed_measures <- simplify(measure = "sport_indoor", place = 2)
imputed_measures <- simplify(measure = "mask", place = 6)
imputed_measures <- simplify(measure = "mask", place = 3)
imputed_measures <- simplify(measure = "mask", place = 1)
imputed_measures <- simplify(measure = "workplace", place = 2)
imputed_measures <- simplify(measure = "curfew", place = 3)
imputed_measures <- simplify(measure = "curfew", place = 2)
imputed_measures <- simplify(measure = "gastronomy", place = 6)
imputed_measures <- simplify(measure = "gastronomy", place = 3)
imputed_measures <- simplify(measure = "test", place = 6)
imputed_measures <- simplify(measure = "test", place = 5)
imputed_measures <- simplify(measure = "test", place = 2)


imputed_measures %<>% dplyr::mutate(
  public_space_measures = case_when(
    public_space_measures == 7 ~ 8,
    public_space_measures == 8 ~ 7,
    TRUE ~ public_space_measures))
imputed_measures %<>% dplyr::mutate(
  test_measures = case_when(
    test_measures == 5 ~ 3,
    TRUE ~ test_measures
  ))

print(prop.table(table(imputed_measures$public_space_measures))*100, digits = 2)
print(prop.table(table(imputed_measures$test_measures))*100, digits = 3)




##  create combined measures at district level ---------------------

imputed_measures %<>% dplyr::mutate(
  sport_measures = case_when(sport_indoor_measures <= 3 ~ sport_indoor_measures,
                             sport_indoor_measures == 4 & sport_outdoor_measures < 5 ~ 4,
                             sport_indoor_measures == 4 & sport_outdoor_measures == 5 ~ 5),
  schools_measures = case_when(primary_school_measures <= 4 ~ primary_school_measures,
                               primary_school_measures == 5 & school_measures<=4 ~ 5,
                               primary_school_measures == 5 & school_measures==5 ~ 6),
  public_event_measures = public_event_outdoor_measures,
  public_event2_measures = public_event_indoor_measures,
  public_event3_measures = case_when(public_event_indoor2_measures <= 4 ~ public_event_indoor2_measures,
                                     public_event_indoor2_measures == 5 & public_event_outdoor2_measures < 5 ~ 5,
                                     public_event_indoor2_measures == 5 & public_event_outdoor2_measures == 5 ~ 6),
  ceshg_max_measures = (1 + (culture_education_measures == max(culture_education_measures)) +
                          (sport_measures == max(sport_measures)) +
                          (gastronomy_measures == max(gastronomy_measures)) +
                          (hospitality_measures == max(hospitality_measures))),
  ceshg_max2_measures = (1 + (culture_education_measures == max(culture_education_measures)-1) +
                           (sport_measures == max(sport_measures)-1) +
                           (gastronomy_measures == max(gastronomy_measures)-1) +
                           (hospitality_measures == max(hospitality_measures)-1)),
  ceshgrs_max_measures = (1 + (culture_education_measures == max(culture_education_measures)) +
                            (sport_measures == max(sport_measures)) +
                            (gastronomy_measures == max(gastronomy_measures)) +
                            (hospitality_measures == max(hospitality_measures)) +
                            (services_measures == max(services_measures)) +
                            (retail_measures == max(retail_measures))),
  ceshgrs_max2_measures = (1 + (culture_education_measures == max(culture_education_measures)-1) +
                             (sport_measures == max(sport_measures)-1) +
                             (gastronomy_measures == max(gastronomy_measures)-1) +
                             (hospitality_measures == max(hospitality_measures)-1) +
                             (services_measures == max(services_measures)-1) +
                             (retail_measures == max(retail_measures)-1)))



imputed_measures <- simplify(measure = "ceshg_max2", place = 3)
imputed_measures <- simplify(measure = "ceshgrs_max2", place = 4)

save(imputed_measures, 
     file = file.path("Daten", "measures_simplified.rData"))



