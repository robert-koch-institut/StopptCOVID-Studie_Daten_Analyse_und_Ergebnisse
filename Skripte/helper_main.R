library(tidyverse)
library(ggplot2)
library(dotwhisker)
library(wesanderson)
library(ggthemes)
library(MASS)



##  lag for impact of vaccination taken from optimizing AIC ------ 
vlag = 5L

##  lag for impact of NPI taken from optimizing AIC ------ 
chosen_lag = -1L

##  setting model offset to "AD"
off = "AD"



themeMod <- function(bs = 11) {
  theme_bw(base_size = bs, base_line_size = 1/2, base_rect_size = 1/2)  +
    theme(plot.title = element_text(lineheight=2.0, face="bold"),
          axis.title.x = element_text(vjust=-.5),
          axis.text.x  = element_text(angle=0, vjust=1),
          axis.title.y = element_text(vjust=1.5),
          axis.text.y  = element_text(angle=0, vjust=0.5),
          strip.text.x = element_text(angle=0),
          strip.background = element_blank(), 
          axis.line = element_line(),
          legend.key = element_rect(color = NA),
          legend.position = "right", 
          legend.justification = "right",
          #        legend.box = "horizontal",
          legend.key.size = unit(bs/4, "line"),
          legend.key.height = unit(bs/4, "line"),
          #        legend.key.width = unit(5,"cm")
          panel.border = element_blank(), 
          panel.grid.minor = element_blank(),
          # panel.grid.major.x = element_blank(),
          panel.background = element_blank()
    )
}



# season until March 20
add_dummy_seasonality <- function(modeling_data){
  modeling_data_with_dummy_season <-
    modeling_data %>%
    mutate(winter = if_else(
      date %within% interval(lubridate::ymd("2020-01-01"), lubridate::ymd("2020-03-20"))|
        date %within% interval(lubridate::ymd("2020-09-22"), lubridate::ymd("2021-03-20"))|
        date %within% interval(lubridate::ymd("2021-09-22"), lubridate::ymd("2022-03-20")),
      1, 0)
    )
  return(modeling_data_with_dummy_season)
}


dates <- rev(c(as.Date("2021/09/01", "%Y/%m/%d"), 
               as.Date("2021/07/01", "%Y/%m/%d"), 
               as.Date("2021/02/15", "%Y/%m/%d"),
               as.Date("2020/10/01", "%Y/%m/%d"),
               as.Date("2020/05/01", "%Y/%m/%d")))


orig_areas <- list(
  "schools",
  "school",
  "primary_school",
  "daycare",
  "workplace",
  "private_space",
  "curfew",
  "public_space",
  "public_event",
  "public_event_outdoor",
  "public_event_indoor",
  "sport",
  "sport_outdoor",
  "sport_indoor",
  "culture_education",
  "services",
  "hospitality",
  "gastronomy",
  "retail",
  "nightlife",
  "test",
  "abstand",
  "mask"
) %>% unlist()


params <- c(
  "schools_measures",
  "private_space_measures",
  "workplace_measures",
  "daycare_measures",
  "public_space_measures",
  "public_event_measures",
  "curfew_measures",
  "retail_measures",
  "nightlife_measures",
  "services_measures",
  "ceshg_max_measures",
  "ceshg_max2_measures",
  "test_measures",
  "abstand_measures",
  "mask_measures"
) 
length(params)

main_measures <- c(
  "schools",
  "private_space",
  "workplace",
  "daycare",
  "public_space",
  "pEvent",
  "curfew",
  "retail",
  "nightlife",
  "services",
  "ceshg_max",
  "ceshg_max2",
  "test",
  "abstand",
  "mask")
length(main_measures)


label_measures_orig <- c("Teststrategie", 
                    "Abstandsregeln", 
                    "Regeln zu Masken", 
                    "Arbeitsplätze", 
                    "Nachtleben", 
                    "Groß- und Einzelhandel", 
                    "Gastronomie",
                    "Beherbergung", 
                    "Kultur", 
                    "Sport", 
                    "Dienstleistungen", 
                    "Öffentliche Veranstaltungen", 
                    "Ausgangsbeschränkungen", 
                    "Private Räume", 
                    "Öffentliche Räume", 
                    "Schulen", 
                    "Kindertagesstätten") %>% rev()
length(label_measures_orig)

label_measures <- c("Regeln zu Masken", 
                    "Abstandsregeln", 
                    "Teststrategie", 
                    "KSGB - Anzahl \n zweitstärkste Stufe",
                    "KSGB - Anzahl \n maximale Stufe",
                    "Dienstleistungen", 
                    "Nachtleben", 
                    "Groß- und Einzelhandel", 
                    "Ausgangsbeschränkungen", 
                    "Öffentliche Veranstaltungen", 
                    "Öffentliche Räume", 
                    "Kindertagesstätten",
                    "Arbeitsplätze", 
                    "Private Räume", 
                    "Schulen") %>% rev()
length(label_measures)



label_model <- c("Impf-Index", "Saisonalität Cosinus", "Saisonalität Sinus", 
                     "Ostern, Weihnachten",                
                     "Schulferien, zweite Hälfte", "Nach den Ferien", "Schulferien", 
                     "Masken, Stufe 5",
                     "Masken, Stufe 4", "Masken, Stufe 3", "Masken, Stufe 2",
                     "Abstandsregeln, Stufe 2",
                     "Teststrategie, Stufe 4", "Teststrategie, Stufe 3", "Teststrategie, Stufe 2", 
                     "KSGB max2, Anzahl 2", 
                     "KSGB max2, Anzahl 1", 
                     "KSGB max, Anzahl 4",
                     "KSGB max, Anzahl 3",
                     "KSGB max, Anzahl 2",
                     "KSGB max, Anzahl 1",
                     "Dienstleistungen, Stufe 5",
                     "Dienstleistungen, Stufe 4",
                     "Dienstleistungen, Stufe 3",
                     "Dienstleistungen, Stufe 2",
                     "Nachtleben, Stufe 3",
                     "Nachleben, Stufe 2",
                     "Groß- und Einzelhandel, Stufe 5",
                     "Groß- und Einzelhandel, Stufe 4",
                     "Groß- und Einzelhandel, Stufe 3",
                     "Groß- und Einzelhandel, Stufe 2",
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
length(label_model)


lines_model <- c(0.5, 1.5, 3.5, 7.5, 11.5, 12.5, 15.5, 17.5, 21.5, 25.5, 
                 27.5, 31.5, 32.5, 37.5, 41.5, 44.5, 45.5, 49.5, 54.5) 



lag_data <- function(clag = chosen_lag, varn = names_bl, data = modeling_bl) {
  for (v in varn) {
    data %<>% group_by(BL) %>% arrange(BL, date) %>% 
      dplyr::mutate(!!sym(v) := case_when(
        (date >= min(data$date) + clag) & (clag >= 0) ~ dplyr::lag(get(v), n = abs(clag)),
        (date < min(data$date) + clag) & (clag >= 0) ~ 0,
        (date <= max(data$date) + clag) & (clag < 0) ~ dplyr::lead(get(v), n = abs(clag)),
        (date > max(data$date) + clag) & (clag < 0) ~ get(v)  # [n()]  
      ) ) %>% ungroup()
  }
  return(data)
}


gen_formula_bl_log <- function(names_npi, 
                               cross_vacc = 0,
                               vaccination = TRUE, 
                               seasonality = TRUE, 
                               offset = "AD", 
                               spatial = "BL"){
  
  measures_txt <- paste(names_npi
                        , collapse=" + ")
  formula_txt <- paste0("logR ~ 0 + ", measures_txt,  " + school_holiday + after_holiday + 
                        school_holiday_second_half + easter_christmas")
  
  if(seasonality == TRUE){
    formula_txt <- paste0(formula_txt,
                          "+sin(2*pi*day_of_year/365)+cos(2*pi*day_of_year/365)")
  }
  
  if(vaccination == TRUE | vaccination==1){
    formula_txt <- paste0(formula_txt, "+ per_vacc_1")
  }
  if(vaccination==2){
    formula_txt <- paste0(formula_txt, "+ per_vacc_2")
  }
  
  if(cross_vacc ==T){
    formula_txt <- paste0(formula_txt, " + pv_AG2")
  }
  
  if(offset == "No"){
    formula_txt <- paste0(
      formula_txt,
      "+ Alpha + Delta"
    )
  }
  
  if(offset == "D"){
    formula_txt <- paste0(
      formula_txt,
      "+ Alpha + offset(offlogD)"
    )
  }
  
  if(offset == "AD"){
    formula_txt <- paste0(
      formula_txt,
      "+ offset(offlog)"
    )
  }
  
  formula_txt <- paste0(
    formula_txt,
    "+ ", spatial)
  
  
  formula <- as.formula(formula_txt)
  return(formula)
}


gen_formula_bl <- function(names_npi, 
                                cross_vacc = 0,
                                vaccination = TRUE, 
                                seasonality = TRUE, 
                                offset = "AD", 
                                spatial = "BL"){
  
  measures_txt <- paste(names_npi
                        , collapse=" + ")
  formula_txt <- paste0("R ~ ", measures_txt,  "+ school_holiday + after_holiday + 
                        school_holiday_second_half + easter_christmas")
  
  if(seasonality == TRUE){
    formula_txt <- paste0(formula_txt,
                          "+sin(2*pi*day_of_year/365)+cos(2*pi*day_of_year/365)")
  }
  
  if(vaccination == TRUE | vaccination==1){
    formula_txt <- paste0(formula_txt, "+ per_vacc_1")
  }
  if(vaccination==2){
    formula_txt <- paste0(formula_txt, "+ per_vacc_2")
  }
  
  if(cross_vacc ==T){
    formula_txt <- paste0(formula_txt, " + pv_AG2")
  }
  
  if(offset == "No"){
    formula_txt <- paste0(
      formula_txt,
      "+ Alpha + Delta"
    )
  }
  
  if(offset == "D"){
    formula_txt <- paste0(
      formula_txt,
      "+ Alpha + offset(offlogD)"
    )
  }
  
  if(offset == "AD"){
    formula_txt <- paste0(
      formula_txt,
      "+ offset(offset)"
    )
  }
  
  formula_txt <- paste0(
    formula_txt,
    "+ ", spatial)
  
  
  formula <- as.formula(formula_txt)
  return(formula)
}



finish_prep <- function(data = modeling_bl) {
  pdata <- data %>% dplyr::filter(date <= as.Date("2021/08/31", "%Y/%m/%d")) %>%
    dplyr::mutate(N_smooth = N_onset_smooth,
                  R = R_ons,
                  logR = log(R_ons),
                  offlog = 0.3*Alpha + 0.6*Delta,
                  offlogD = 0.6*Delta,
                  offset = log(1 + 0.3*Alpha + 0.6*Delta),
                  offsetD = log(1 + 0.6*Delta) )
  pdata %<>% dplyr::filter(!is.na(logR) & logR<Inf)
  pdata %<>% dplyr::mutate(N_onset_norm = 
                           N_onset_smooth/mean(pdata$N_onset_smooth))
  return(pdata)
}

model_fit_ag <- function(data_bl = modeling_bl, params = params, 
                         vlag = vlag,
                         vacc = T, cross_vacc = 0,
                         spatial = BL,
                         offset = "AD") {
  
  
  data_bl <- finish_prep(data = data_bl)
  
  names_npi <- names(data_bl %>% dplyr::select(
    starts_with(params %>% unlist() )))
  
  formula_bl <-
    gen_formula_bl_log(names_npi = names_npi,
                       cross_vacc = cross_vacc,
                       vaccination = vacc,
                       seasonality = TRUE,
                       offset = offset)
  
  formula_bl
  
  model_bl1 <-
    lm(formula = formula_bl,
       # family = gaussian(link = "identity"),
       data = data_bl, # %>% dplyr::select(-c(date, Meldefaelle, R_all, lag4_mfs, winter)),
       weights = N_onset_norm
    )
  
  summary(model_bl1)
  str(model_bl1$coefficients)
  nrow(as.array(model_bl1$coefficients))
  model_bl1$aic
  model_bl1$deviance
  tm_bl <- tidy_model(model_bl1)
  
  return(tm_bl)
}

tidy_model <- function(model = model) {
  
  tm <- model %>%
    broom::tidy() %>%
    filter(!(grepl("ags5", term))) %>%
    filter(!(grepl("BL", term))) %>%
    filter(!(grepl("kreis", term))) %>%
    filter(!(grepl("(Intercept)", term))) %>%
    mutate(model = "my_name")
  
  tm %<>%
    mutate(log_est = estimate,
           log_est.low = estimate - 1.96*std.error,
           log_est.high = estimate + 1.96*std.error,
           conf.low = exp(estimate - 1.96*std.error),
           conf.high = exp(estimate + 1.96*std.error),
           estimate = exp(estimate),
           variable = gsub("_measures_lag10", "", term),
           variable = gsub(" Oeffnung gemaess", "", variable),
           variable = gsub(" Oeffnung oder vollstaendige", "", variable),
           variable = gsub("Vollstaendige oder ", "", variable),
           variable = gsub(", teilweise Schliessung", "", variable),
           variable = gsub("oder komplette ", "", variable),
           variable = gsub("gem. Hygienevorschriften", "", variable)
    )
  
  tm %<>%
    mutate(var_f = factor(variable, levels = rev(unique(tm$variable))),
           protect = (estimate < 1)
    )
  
  
  tm %<>% dplyr::mutate(variable = sub(" .*", "", variable))
  tm %<>% dplyr::mutate(variable = sub("0", "_0", variable))
  tm %<>% dplyr::mutate(variable = sub("__", "_", variable))
  
  return(tm)
}


model_fit_lag_tidy <- function(vlag = vlag,
                               chosen_lag = chosen_lag, 
                               vacc = T,
                               data_bl = modeling_bl, 
                               other = other, op = op, 
                               olag = olag,
                               offset = "AD") {
  
  # data_bl = modeling_bl
  # chosen_lag = 1L
  # olag = 0L
  
  names_npi <- names(data_bl %>% dplyr::select(
    starts_with(other %>% unlist() )))
  
  data_bl <- lag_data(clag = olag, varn = names_npi, data = data_bl)
  
  if (length(op) >0) {
    names_op <- names(data_bl %>% dplyr::select(
      starts_with(op %>% unlist() )))

    data_bl <- lag_data(clag = chosen_lag, varn = names_op, data = data_bl)
  }
  if (length(op) == 0) {
    names_op = ""
  }
  
  data_bl <- finish_prep(data = data_bl)
  

  length(names_npi)
  names_npi <- c(names_npi, names_op)
  
  
  formula_bl <-
    gen_formula_bl_log(names_npi = names_npi,
                            vaccination = vacc,
                            seasonality = TRUE,
                            offset = offset)
  
  formula_bl
  
  model_bl1 <-
    lm(formula = formula_bl,
       data = data_bl,
       weights = N_onset_norm
    )
  
  tm_bl <- tidy_model(model_bl1)
  
  return(list(tidy = tm_bl, model = model_bl1))
}


model_fit_vacc <- function(vlag = vlag, 
                           data_bl = modeling_bl, 
                           params = params, number = 1, 
                           offset = "AD") {
  
  
  per = paste0("per_vacc_", number)
  data_bl <- lag_data(clag = vlag, varn = per, data = data_bl)
  data_bl <- finish_prep(data = data_bl)

  names_npi <- names(data_bl %>% dplyr::select(c(starts_with(params) )))
  
  
  formula_bl <-
    gen_formula_bl_log(names_npi = names_npi,
                            vaccination = number,
                            seasonality = TRUE,
                            offset = offset)
  
  model_bl1 <-
    lm(formula = formula_bl,
       # family = gaussian(link = "identity"),
       data = data_bl,
       weights = N_onset_norm
    )
  
  return(AIC(model_bl1))
}



model_fit_lag <- function(vlag = vlag, 
                          chosen_lag = chosen_lag, 
                          vacc = T,
                          data_bl = modeling_bl, 
                          other = other, op = op, 
                          olag = olag,
                          offset = "AD") {
  

  names_npi <- names(data_bl %>% dplyr::select(
    starts_with(other %>% unlist() )))

  data_bl <- lag_data(clag = olag, varn = names_npi, data = data_bl)


  if (length(op) >0) {
    names_op <- names(data_bl %>% dplyr::select(
      starts_with(op %>% unlist() )))
    
    data_bl <- lag_data(clag = chosen_lag, varn = names_op, data = data_bl)

  }
  if (length(op) == 0) {
    names_op = ""
  }

  data_bl <- finish_prep(data = data_bl)  

  length(names_npi)
  names_npi <- c(names_npi, names_op)
  
  
  formula_bl <-
    gen_formula_bl_log(names_npi = names_npi,
                            vaccination = vacc,
                            seasonality = TRUE,
                            offset = offset)
  
  formula_bl
  
  model_bl1 <-
    lm(formula = formula_bl,
       data = data_bl,
       weights = N_onset_norm
    )
  
  return(AIC(model_bl1))
}



ci_pi_lm <- function(model = model_main, data = data_main, 
                     formula = formula_main,
                     n_real = 200, 
                     id = c("BL", "date"), 
                     sum = c("BL", "date"),
                     outcome = "logR", offset = "AD") {
  
  # model <- model_main
  # data <- modeling_bl_
  # formula <- formula_main
  # n_real <- 200
  # id <- c("BL", "date")
  # sum <- c("BL", "date")
  # outcome <- "logR"
  # offset <- "D"
  
  if(offset == "No"){
    data %<>% dplyr::mutate(os = log(1))
  }
  
  if(offset == "D"){
    data %<>% dplyr::mutate(os = log(1 + 0.6)*Delta )
  }
  
  if(offset == "AD"){
    
    data %<>% dplyr::mutate(os = log(1 + 0.3)*Alpha + log(1 +0.6)*Delta )
  }
  
  
  # summary(model)
  beta <- coef(model) # coefficients of smooth functions
  vcov <- vcov(model) # * disp #, freq=TRUE) # covariance matrix
  
  # theta <- model$theta
  sigma <- summary(model)$sigma
  
  set.seed(3456)
  
  # n_real <- 200
  
  mvn <- mvrnorm(n_real, beta, vcov) ## simulate n coefficent vectors 
  
  form <- paste(" ", gsub("\n", "", formula[3]),sep='~')
  # form <- gsub("offset\\(offlog) \\+", "", form)
  form
  data_m <- with(data, model.matrix(as.formula(form)))
  
  # colnames(mvn)
  # colnames(data_m)
  dim(mvn)
  dim(data_m)
  dim(data)
  
  set.seed(4567)
  res <- data %>% dplyr::select(all_of(c(id, outcome, "Bev"))) %>% arrange(across(all_of(id)))
  
  for (i in 1:n_real) {
    mean =  paste0("mean_outcome_", i)
    res[[mean]] = (data$os + data_m %*% mvn[i,])[,1]
    neu =  paste0("outcome_", i) 
    res[[neu]] = rnorm(n = nrow(res), 
                       mean = (data$os + data_m %*% mvn[i,])[,1], 
                       sd = sigma)
  }
  
  result <- res %>% as.data.frame() %>% dplyr::select(-starts_with("mean_")) %>%
    pivot_longer(cols = starts_with("outcome_"),
                 names_to = "real",
                 names_prefix = "outcome_",
                 values_to = "outcome_"
    ) %>% 
    dplyr::mutate(real = as.integer(real)) %>%
    arrange(across(all_of(id)))
  
  resultm <- res %>% as.data.frame() %>% dplyr::select(-starts_with("outcome_")) %>%
    pivot_longer(cols = starts_with("mean_outcome_"),
                 names_to = "real",
                 names_prefix = "mean_outcome_",
                 values_to = "mean_outcome"
    ) %>% 
    dplyr::mutate(real = as.integer(real)) %>%
    arrange(across(all_of(id)))                 
  
  result <- left_join(result, resultm, by = c(id, "real", outcome, "Bev"))                 
  
  result <- result %>% group_by(across(all_of(c("real", sum)))) %>% 
    dplyr::summarise(outcome_ = weighted.mean(outcome_, w = Bev),
                     mean_outcome = weighted.mean(mean_outcome, w = Bev),
                     logR = weighted.mean(logR, w = Bev))
  
  res_l <- result %>% 
    group_by(across(all_of(sum))) %>%
    dplyr::summarise(pred_mean_outcome = mean(mean_outcome),
                     lb_mean_outcome = quantile(mean_outcome, probs= 0.025),
                     ub_mean_outcome = quantile(mean_outcome, probs= 0.975),
                     pred_logR = mean(outcome_),
                     lb_logR = quantile(outcome_, probs= 0.025),
                     ub_logR = quantile(outcome_, probs= 0.975),
                     q1_logR = quantile(outcome_, probs= 0.25),
                     q3_logR = quantile(outcome_, probs= 0.75),
                     logR = mean(logR))
  
  return(res_l)
  
}


