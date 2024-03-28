library(dplyr)
library(lubridate)
library(data.table, include.only = "rleid")

source("Skripte/holidays_functions.R")

template <- make_school_holiday_template()
school_holidays <- add_school_holidays(template)
school_holidays_with_after_effects <- add_after_holiday_effect(school_holidays)
school_holidays_with_second_half_effects <- add_second_half_effect(school_holidays_with_after_effects)
holidays <- add_easter_christmas(school_holidays_with_second_half_effects)

save(holidays, file = file.path("Daten", "holidays.rData"))

