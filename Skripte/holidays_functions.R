


make_school_holiday_template <- function(){
  # Make a template with all states and all dates
  # This is the basis on which to join all other holidays
  
  template <-
    expand.grid(
      date = seq(ymd('2020-03-01'), ymd('2021-12-31'), by = '1 day'),
      bundesland = c(
        "Schleswig-Holstein",
        "Hamburg",
        "Niedersachsen"  ,
        "Nordrhein-Westfalen" ,
        "Hessen"             ,
        "Rheinland-Pfalz"       ,
        "Baden-Wuerttemberg"   ,
        "Bayern"              ,
        "Saarland"              ,
        "Berlin"               ,
        "Brandenburg"         ,
        "Mecklenburg-Vorpommern",
        "Sachsen"               ,
        "Sachsen-Anhalt"       ,
        "Thueringen"            ,
        "Bremen"
      ),
      stringsAsFactors = FALSE
    )
  return(template)
}

add_school_holidays <- function(dates_and_federal_states){
  # Add school holidays to a df with dates and federal states
  # School holidays for 2020 and 2021 are available.
  
  dates_and_federal_states_and_holidays <- dates_and_federal_states %>%
    rowwise() %>%
    mutate(school_holiday = if_else(date %within% holiday_list[bundesland][[1]] , 1, 0))
  return(dates_and_federal_states_and_holidays)
}

add_after_holiday_effect <- function(holidays){
  # Add an after effect to school holidays
  
  holidays_with_lag <- 
    holidays %>%
    group_by(bundesland) %>%
    mutate(after_holiday = if_else(school_holiday - lag(school_holiday, n = 1, default = 0) < 0, 1, 0),
           after_holiday = if_else(date == ymd("2020-03-01") & bundesland == "Bayern", 1, after_holiday), # Bavaria has vacations ending on 28 February
           after_holiday = if_else(lag(after_holiday, n=1, default = 0) == 1 |
                                     lag(after_holiday, n=2, default = 0) == 1|
                                     lag(after_holiday, n=3, default = 0) == 1|
                                     lag(after_holiday, n=4, default = 0) == 1, 1, after_holiday),
           after_holiday = if_else(date == ymd("2020-03-05") & bundesland == "Bayern", 0, after_holiday)) %>%# cut last day from Bavaria
    ungroup()
  return(holidays_with_lag)
}

add_second_half_effect <- function(school_holidays){
  # Add a variable that specifically marks the second half of school holidays
  # Ignores school holidays shorter than 12 days
  
  school_holidays_with_second_half_effects <- 
    school_holidays %>%
    mutate(run_length = sequence_along_runs(school_holiday),
           run_id = data.table::rleid(school_holiday)
    ) %>%
    group_by(run_id) %>%
    mutate(school_holiday_second_half = if_else(school_holiday==1 & max(run_length)>=12 & run_length>0.5*max(run_length), 1, 0)) %>%
    ungroup() %>%
    dplyr::select(-c(run_length, run_id))
  return(school_holidays_with_second_half_effects)
}

sequence_along_runs <- function(vec){
  # Compute the length of a run, i.e. the number of consecutive days until change
  
  runs <- c()
  run_length_encoding <- rle(vec)
  for (length in run_length_encoding[["lengths"]]) {
    runs <- c(runs,seq_len(length))
  }
  return(runs)
}

add_easter_christmas <- function(modeling_data){
  # Mark the period around Christmas and Easter, 
  # where people are less likely to visit a doctor.
  
  modeling_data_holidays  <- 
    modeling_data %>%
    mutate(
      easter_christmas = case_when(
        date %within% interval(ymd("2020-04-10"), ymd("2020-04-13")) ~ 1,
        date %within% interval(ymd("2020-12-24"), ymd("2021-01-01")) ~ 1,
        date %within% interval(ymd("2021-04-02"), ymd("2021-04-05")) ~ 1,
        date %within% interval(ymd("2021-12-24"), ymd("2022-01-01")) ~ 1,
        TRUE ~ 0
      )
    )
  return(modeling_data_holidays)
}

holiday_list <- list(
  "Baden-Wuerttemberg" = list(
    interval(lubridate::dmy("06.04.2020"), lubridate::dmy("18.04.2020")),
    interval(lubridate::dmy("02.06.2020"), lubridate::dmy("13.06.2020")),
    interval(lubridate::dmy("30.07.2020"), lubridate::dmy("12.09.2020")),
    interval(lubridate::dmy("26.10.2020"), lubridate::dmy("30.10.2020")),
    interval(lubridate::dmy("23.12.2020"), lubridate::dmy("09.01.2021")),
    interval(lubridate::dmy("06.04.2021"), lubridate::dmy("10.04.2021")),
    interval(lubridate::dmy("25.05.2021"), lubridate::dmy("05.06.2021")),
    interval(lubridate::dmy("29.07.2021"), lubridate::dmy("11.09.2021")),
    interval(lubridate::dmy("02.11.2021"), lubridate::dmy("06.11.2021")),
    interval(lubridate::dmy("23.12.2021"), lubridate::dmy("08.01.2022"))
  ),
  
  "Bayern" = list(
    interval(lubridate::dmy("24.02.2020"), lubridate::dmy("28.02.2020")),
    interval(lubridate::dmy("06.04.2020"), lubridate::dmy("18.04.2020")),
    interval(lubridate::dmy("02.06.2020"), lubridate::dmy("13.06.2020")),
    interval(lubridate::dmy("27.07.2020"), lubridate::dmy("07.09.2020")),
    interval(lubridate::dmy("31.10.2020"), lubridate::dmy("06.11.2020")),
    interval(lubridate::dmy("18.11.2020"), lubridate::dmy("18.11.2020")),
    interval(lubridate::dmy("23.12.2020"), lubridate::dmy("09.01.2021")),
    interval(lubridate::dmy("29.03.2021"), lubridate::dmy("10.04.2021")),
    interval(lubridate::dmy("25.05.2021"), lubridate::dmy("04.06.2021")),
    interval(lubridate::dmy("30.07.2021"), lubridate::dmy("13.09.2021")),
    interval(lubridate::dmy("02.11.2021"), lubridate::dmy("05.11.2021")),
    interval(lubridate::dmy("17.11.2021"), lubridate::dmy("17.11.2021")),
    interval(lubridate::dmy("24.12.2021"), lubridate::dmy("08.01.2022"))
  ),
  
  "Berlin" = list(
    interval(lubridate::dmy("03.02.2020"), lubridate::dmy("08.02.2020")),
    interval(lubridate::dmy("06.04.2020"), lubridate::dmy("17.04.2020")),
    interval(lubridate::dmy("22.05.2020"), lubridate::dmy("22.05.2020")),
    interval(lubridate::dmy("25.06.2020"), lubridate::dmy("07.08.2020")),
    interval(lubridate::dmy("12.10.2020"), lubridate::dmy("24.10.2020")),
    interval(lubridate::dmy("21.12.2020"), lubridate::dmy("02.01.2021")),
    
    interval(lubridate::dmy("01.02.2021"), lubridate::dmy("06.02.2021")),
    interval(lubridate::dmy("29.03.2021"), lubridate::dmy("10.04.2021")),
    interval(lubridate::dmy("14.05.2021"), lubridate::dmy("14.05.2021")),
    interval(lubridate::dmy("24.06.2021"), lubridate::dmy("06.08.2021")),
    interval(lubridate::dmy("11.10.2021"), lubridate::dmy("23.10.2021")),
    interval(lubridate::dmy("23.12.2021"), lubridate::dmy("31.12.2021"))
  ),
  
  "Brandenburg" = list(
    interval(lubridate::dmy("03.02.2020"), lubridate::dmy("08.02.2020")),
    interval(lubridate::dmy("06.04.2020"), lubridate::dmy("17.04.2020")),
    interval(lubridate::dmy("25.06.2020"), lubridate::dmy("08.08.2020")),
    interval(lubridate::dmy("12.10.2020"), lubridate::dmy("24.10.2020")),
    interval(lubridate::dmy("21.12.2020"), lubridate::dmy("02.01.2021")),
    
    interval(lubridate::dmy("01.02.2021"), lubridate::dmy("06.02.2021")),
    interval(lubridate::dmy("29.03.2021"), lubridate::dmy("09.04.2021")),
    interval(lubridate::dmy("24.06.2021"), lubridate::dmy("07.08.2021")),
    interval(lubridate::dmy("11.10.2021"), lubridate::dmy("23.10.2021")),
    interval(lubridate::dmy("23.12.2021"), lubridate::dmy("31.12.2021"))
  ),
  
  "Bremen" = list(
    interval(lubridate::dmy("03.02.2020"), lubridate::dmy("04.02.2020")),
    interval(lubridate::dmy("28.03.2020"), lubridate::dmy("14.04.2020")),
    interval(lubridate::dmy("22.05.2020"), lubridate::dmy("22.05.2020")),
    interval(lubridate::dmy("02.06.2020"), lubridate::dmy("02.06.2020")),
    interval(lubridate::dmy("16.07.2020"), lubridate::dmy("26.08.2020")),
    interval(lubridate::dmy("12.10.2020"), lubridate::dmy("24.10.2020")),
    interval(lubridate::dmy("23.12.2020"), lubridate::dmy("08.01.2021")),
    
    interval(lubridate::dmy("01.02.2021"), lubridate::dmy("02.02.2021")),
    interval(lubridate::dmy("27.03.2021"), lubridate::dmy("10.04.2021")),
    interval(lubridate::dmy("14.05.2021"), lubridate::dmy("14.05.2021")),
    interval(lubridate::dmy("25.05.2021"), lubridate::dmy("25.05.2021")),
    interval(lubridate::dmy("22.07.2021"), lubridate::dmy("01.09.2021")),
    interval(lubridate::dmy("18.10.2021"), lubridate::dmy("30.10.2021")),
    interval(lubridate::dmy("23.12.2021"), lubridate::dmy("08.01.2022"))
  ),
  
  "Hamburg" = list(
    interval(lubridate::dmy("31.01.2020"), lubridate::dmy("31.01.2020")),
    interval(lubridate::dmy("02.03.2020"), lubridate::dmy("13.03.2020")),
    interval(lubridate::dmy("18.05.2020"), lubridate::dmy("22.05.2020")),
    interval(lubridate::dmy("25.06.2020"), lubridate::dmy("05.08.2020")),
    interval(lubridate::dmy("05.10.2020"), lubridate::dmy("16.10.2020")),
    interval(lubridate::dmy("21.12.2020"), lubridate::dmy("04.01.2021")),
    
    interval(lubridate::dmy("29.01.2021"), lubridate::dmy("29.01.2021")),
    interval(lubridate::dmy("01.03.2021"), lubridate::dmy("12.03.2021")),
    interval(lubridate::dmy("10.05.2021"), lubridate::dmy("14.05.2021")),
    interval(lubridate::dmy("24.06.2021"), lubridate::dmy("04.08.2021")),
    interval(lubridate::dmy("04.10.2021"), lubridate::dmy("15.10.2021")),
    interval(lubridate::dmy("23.12.2021"), lubridate::dmy("04.01.2022"))
  ),
  
  "Hessen" = list(
    interval(lubridate::dmy("06.04.2020"), lubridate::dmy("18.04.2020")),
    interval(lubridate::dmy("06.07.2020"), lubridate::dmy("14.08.2020")),
    interval(lubridate::dmy("05.10.2020"), lubridate::dmy("17.10.2020")),
    interval(lubridate::dmy("21.12.2020"), lubridate::dmy("09.01.2021")),
    
    interval(lubridate::dmy("06.04.2021"), lubridate::dmy("16.04.2021")),
    interval(lubridate::dmy("19.07.2021"), lubridate::dmy("27.08.2021")),
    interval(lubridate::dmy("11.10.2021"), lubridate::dmy("23.10.2021")),
    interval(lubridate::dmy("23.12.2021"), lubridate::dmy("08.01.2022"))
  ),
  
  "Mecklenburg-Vorpommern" = list(
    interval(lubridate::dmy("10.02.2020"), lubridate::dmy("21.02.2020")),
    interval(lubridate::dmy("06.04.2020"), lubridate::dmy("15.04.2020")),
    interval(lubridate::dmy("22.05.2020"), lubridate::dmy("22.05.2020")),
    interval(lubridate::dmy("29.05.2020"), lubridate::dmy("02.06.2020")),
    interval(lubridate::dmy("22.06.2020"), lubridate::dmy("01.08.2020")),
    interval(lubridate::dmy("05.10.2020"), lubridate::dmy("10.10.2020")),
    interval(lubridate::dmy("02.11.2020"), lubridate::dmy("03.11.2020")),
    interval(lubridate::dmy("21.12.2020"), lubridate::dmy("02.01.2021")),
    
    interval(lubridate::dmy("06.02.2021"), lubridate::dmy("19.02.2021")),
    interval(lubridate::dmy("29.03.2021"), lubridate::dmy("07.04.2021")),
    interval(lubridate::dmy("14.05.2021"), lubridate::dmy("14.05.2021")),
    interval(lubridate::dmy("21.05.2021"), lubridate::dmy("25.05.2021")),
    interval(lubridate::dmy("21.06.2021"), lubridate::dmy("31.07.2021")),
    interval(lubridate::dmy("02.10.2021"), lubridate::dmy("09.10.2021")),
    interval(lubridate::dmy("01.11.2021"), lubridate::dmy("02.11.2021")),
    interval(lubridate::dmy("22.12.2021"), lubridate::dmy("31.12.2021"))
  ),
  
  "Niedersachsen" = list(
    interval(lubridate::dmy("03.02.2020"), lubridate::dmy("04.02.2020")),
    interval(lubridate::dmy("30.03.2020"), lubridate::dmy("14.04.2020")),
    interval(lubridate::dmy("22.05.2020"), lubridate::dmy("22.05.2020")),
    interval(lubridate::dmy("02.06.2020"), lubridate::dmy("02.06.2020")),
    interval(lubridate::dmy("16.07.2020"), lubridate::dmy("26.08.2020")),
    interval(lubridate::dmy("12.10.2020"), lubridate::dmy("23.10.2020")),
    interval(lubridate::dmy("21.12.2020"), lubridate::dmy("08.01.2021")),
    
    interval(lubridate::dmy("01.02.2021"), lubridate::dmy("02.02.2021")),
    interval(lubridate::dmy("29.03.2021"), lubridate::dmy("09.04.2021")),
    interval(lubridate::dmy("14.05.2021"), lubridate::dmy("14.05.2021")),
    interval(lubridate::dmy("25.05.2021"), lubridate::dmy("25.05.2021")),
    interval(lubridate::dmy("22.07.2021"), lubridate::dmy("01.09.2021")),
    interval(lubridate::dmy("18.10.2021"), lubridate::dmy("29.10.2021")),
    interval(lubridate::dmy("23.12.2021"), lubridate::dmy("07.01.2022"))
  ),
  
  "Nordrhein-Westfalen" = list(
    interval(lubridate::dmy("06.04.2020"), lubridate::dmy("18.04.2020")),
    interval(lubridate::dmy("02.06.2020"), lubridate::dmy("02.06.2020")),
    interval(lubridate::dmy("29.06.2020"), lubridate::dmy("11.08.2020")),
    interval(lubridate::dmy("12.10.2020"), lubridate::dmy("24.10.2020")),
    interval(lubridate::dmy("21.12.2020"), lubridate::dmy("06.01.2021")),
    
    interval(lubridate::dmy("29.03.2021"), lubridate::dmy("10.04.2021")),
    interval(lubridate::dmy("25.05.2021"), lubridate::dmy("25.05.2021")),
    interval(lubridate::dmy("05.07.2021"), lubridate::dmy("17.08.2021")),
    interval(lubridate::dmy("11.10.2021"), lubridate::dmy("23.10.2021")),
    interval(lubridate::dmy("24.12.2021"), lubridate::dmy("08.01.2022"))
  ),
  "Rheinland-Pfalz" = list(
    interval(lubridate::dmy("17.02.2020"), lubridate::dmy("21.02.2020")),
    interval(lubridate::dmy("09.04.2020"), lubridate::dmy("17.04.2020")),
    interval(lubridate::dmy("06.07.2020"), lubridate::dmy("14.08.2020")),
    interval(lubridate::dmy("12.10.2020"), lubridate::dmy("23.10.2020")),
    interval(lubridate::dmy("21.12.2020"), lubridate::dmy("31.12.2020")),
    
    interval(lubridate::dmy("29.03.2021"), lubridate::dmy("06.04.2021")),
    interval(lubridate::dmy("25.05.2021"), lubridate::dmy("02.06.2021")),
    interval(lubridate::dmy("19.07.2021"), lubridate::dmy("27.08.2021")),
    interval(lubridate::dmy("11.10.2021"), lubridate::dmy("22.10.2021")),
    interval(lubridate::dmy("23.12.2021"), lubridate::dmy("31.12.2021"))
  ),
  "Saarland" = list(
    interval(lubridate::dmy("17.02.2020"), lubridate::dmy("25.02.2020")),
    interval(lubridate::dmy("14.04.2020"), lubridate::dmy("24.04.2020")),
    interval(lubridate::dmy("06.07.2020"), lubridate::dmy("14.08.2020")),
    interval(lubridate::dmy("12.10.2020"), lubridate::dmy("23.10.2020")),
    interval(lubridate::dmy("21.12.2020"), lubridate::dmy("31.12.2020")),
    
    interval(lubridate::dmy("15.02.2021"), lubridate::dmy("19.02.2021")),
    interval(lubridate::dmy("29.03.2021"), lubridate::dmy("07.04.2021")),
    interval(lubridate::dmy("25.05.2021"), lubridate::dmy("28.05.2021")),
    interval(lubridate::dmy("19.07.2021"), lubridate::dmy("27.08.2021")),
    interval(lubridate::dmy("18.10.2021"), lubridate::dmy("29.10.2021")),
    interval(lubridate::dmy("23.12.2021"), lubridate::dmy("03.01.2022"))
  ),
  "Sachsen" = list(
    interval(lubridate::dmy("10.02.2020"), lubridate::dmy("22.02.2020")),
    interval(lubridate::dmy("10.04.2020"), lubridate::dmy("18.04.2020")),
    interval(lubridate::dmy("22.05.2020"), lubridate::dmy("22.05.2020")),
    interval(lubridate::dmy("20.07.2020"), lubridate::dmy("28.08.2020")),
    interval(lubridate::dmy("19.10.2020"), lubridate::dmy("31.10.2020")),
    interval(lubridate::dmy("19.12.2020"), lubridate::dmy("02.01.2021")),
    
    interval(lubridate::dmy("31.01.2021"), lubridate::dmy("06.02.2021")),
    interval(lubridate::dmy("27.03.2021"), lubridate::dmy("10.04.2021")),
    interval(lubridate::dmy("14.05.2021"), lubridate::dmy("14.05.2021")),
    interval(lubridate::dmy("26.07.2021"), lubridate::dmy("03.09.2021")),
    interval(lubridate::dmy("18.10.2021"), lubridate::dmy("30.10.2021")),
    interval(lubridate::dmy("23.12.2021"), lubridate::dmy("01.01.2022"))
  ),
  "Sachsen-Anhalt" = list(
    interval(lubridate::dmy("10.02.2020"), lubridate::dmy("14.02.2020")),
    interval(lubridate::dmy("06.04.2020"), lubridate::dmy("11.04.2020")),
    interval(lubridate::dmy("18.05.2020"), lubridate::dmy("30.05.2020")),
    interval(lubridate::dmy("16.07.2020"), lubridate::dmy("26.08.2020")),
    interval(lubridate::dmy("19.10.2020"), lubridate::dmy("24.10.2020")),
    interval(lubridate::dmy("21.12.2020"), lubridate::dmy("10.01.2021")),
    
    interval(lubridate::dmy("08.02.2021"), lubridate::dmy("13.02.2021")),
    interval(lubridate::dmy("29.03.2021"), lubridate::dmy("03.04.2021")),
    interval(lubridate::dmy("10.05.2021"), lubridate::dmy("22.05.2021")),
    interval(lubridate::dmy("22.07.2021"), lubridate::dmy("01.09.2021")),
    interval(lubridate::dmy("25.10.2021"), lubridate::dmy("30.10.2021")),
    interval(lubridate::dmy("22.12.2021"), lubridate::dmy("08.01.2022"))
  ),
  "Schleswig-Holstein" = list(
    interval(lubridate::dmy("30.03.2020"), lubridate::dmy("17.04.2020")),
    interval(lubridate::dmy("22.05.2020"), lubridate::dmy("22.05.2020")),
    interval(lubridate::dmy("29.06.2020"), lubridate::dmy("08.08.2020")),
    interval(lubridate::dmy("05.10.2020"), lubridate::dmy("17.10.2020")),
    interval(lubridate::dmy("19.12.2020"), lubridate::dmy("06.01.2021")),
    
    interval(lubridate::dmy("01.04.2021"), lubridate::dmy("16.04.2021")),
    interval(lubridate::dmy("14.05.2021"), lubridate::dmy("15.05.2021")),
    interval(lubridate::dmy("21.06.2021"), lubridate::dmy("31.07.2021")),
    interval(lubridate::dmy("04.10.2021"), lubridate::dmy("16.10.2021")),
    interval(lubridate::dmy("23.12.2021"), lubridate::dmy("08.01.2022"))
  ),
  "Thueringen" = list(
    interval(lubridate::dmy("10.02.2020"), lubridate::dmy("14.02.2020")),
    interval(lubridate::dmy("06.04.2020"), lubridate::dmy("18.04.2020")),
    interval(lubridate::dmy("22.05.2020"), lubridate::dmy("22.05.2020")),
    interval(lubridate::dmy("20.07.2020"), lubridate::dmy("29.08.2020")),
    interval(lubridate::dmy("17.10.2020"), lubridate::dmy("30.10.2020")),
    interval(lubridate::dmy("23.12.2020"), lubridate::dmy("02.01.2021")),
    
    interval(lubridate::dmy("25.01.2021"), lubridate::dmy("30.01.2021")),
    interval(lubridate::dmy("29.03.2021"), lubridate::dmy("10.04.2021")),
    interval(lubridate::dmy("14.05.2021"), lubridate::dmy("14.05.2021")),
    interval(lubridate::dmy("26.07.2021"), lubridate::dmy("04.09.2021")),
    interval(lubridate::dmy("25.10.2021"), lubridate::dmy("06.11.2021")),
    interval(lubridate::dmy("23.12.2021"), lubridate::dmy("31.12.2021"))
  ),
  "Deutschland" = list()
)


