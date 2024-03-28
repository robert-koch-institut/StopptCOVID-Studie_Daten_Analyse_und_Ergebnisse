library(dplyr)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(lubridate)
library(zoo)
library(data.table)

rm(list = ls())

source("Skripte/helper_main.R")


min <- -21L
max <- 21L


num_op <- list()
# num_op <- setdiff(1L:15L, c(1L, 4L, 6L, 7L, 9L, 10L, 11L, 12L))
if (length(num_op) >0) { op = params[num_op] } else op = list()
other = params[!(params %in% op)]

length(op)
length(other)


aic_gesamt <- NULL

for (j in 1:5) { 

aic_ag <- list()
for (a in 1:4) {

  
  vacc <- (a > 1)
  
  if (a==1) {
    ag = "0 bis 17 Jahre"
    l = 5
  }
  if (a==2) {
    ag = "18 bis 59 Jahre"
    l = 5
  }
  if (a==3) {
    ag = "60 und mehr Jahre"
    l = 2
  }
  if (a==4) {
    ag = "alle Altersgruppen"
    l = 5
  }
  
  
  print(ag)  
  
  if (a<=3) {
    load(file = file.path("Daten", paste0("modeling_bl_ag.rData")))  
    modeling_bl <- modeling_bl_ag %<>% dplyr::filter(AG == ag, date < dates[[j]])
  }
  
  if (a==4) {
    load(file = file.path("Daten", paste0("modeling_bl.rData")))  
    modeling_bl %<>% dplyr::filter(date < dates[[j]])
  }
  
  modeling_bl <- lag_data(clag = vlag, varn = c("per_vacc_1"), data = modeling_bl)


  aic <- list()
  for (olag in min:max) {
    
    cat( olag )

    ind <- olag + 1L -min
    aic[[ind]] <- model_fit_lag(vlag = vlag, 
                                chosen_lag = chosen_lag, 
                                vacc = vacc,
                                data_bl = modeling_bl, 
                                other = other, 
                                op = op, olag = olag,
                                offset = off)
  }
  aic_ag[[a]] <- aic %>% unlist()
  
}
aic_df <- bind_cols(AG1 = aic_ag[[1]], AG2 = aic_ag[[2]], AG3 = aic_ag[[3]], AG4 = aic_ag[[4]])
aic_df %<>% dplyr::mutate(lag = row_number()-1+min)

aic_comp <- pivot_longer(aic_df, cols = starts_with("AG"), names_to = "AG", names_prefix = "AG", values_to = "aic")
aic_comp %<>% dplyr::group_by(AG) %>% dplyr::mutate(min_aic = min(aic),
                                                    AG = as.numeric(AG)) %>% data.frame()
aic_comp %<>% dplyr::mutate(AG_f = factor(AG, labels = c("0 bis 17 Jahre", "18 bis 59 Jahre", "60 und mehr Jahre", "alle Altersgruppen")))
with(aic_comp, table(AG, AG_f))

aic_gesamt <- bind_rows(aic_gesamt, aic_comp %>% dplyr::mutate(Zeitraum = j))

ggplot(data = aic_comp, 
       aes(x = lag)) +
  geom_line(aes(y = aic, color = AG_f)) +
  geom_point(aes(y = min_aic, color = AG_f), data = aic_comp %>% dplyr::filter(aic==min_aic)) +
  themeMod(12) +
  labs(y = "AIC", x = "Verzug in Tagen zwischen Verordnung und Wirkung auf den R-Wert") +
  theme(
    plot.title = element_text(face = "bold"),
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(color = NA),
    axis.text.x  = element_text(angle=90, vjust=.3)
  ) +
  scale_color_manual(
    values = rev(wes_palette("Zissou1", 
                             length(unique(aic_comp$AG)), 
                             type = "continuous")),
    breaks = c("0 bis 17 Jahre", "18 bis 59 Jahre", "60 und mehr Jahre", "alle Altersgruppen"),
    labels = c("0 to 17", "18 to 59", "60+", "all"),
    name = "Age group") +
  scale_x_continuous(breaks = min:max)
ggsave(file.path("output", paste0("aic_lag_npi_", length(op), "_", max, 
                                          "_vlag_", vlag, "_Zeitraum_", j, ".pdf")), 
       width = 10, height = 5, dpi = 1200)
ggsave(file.path("output", paste0("aic_lag_npi_", length(op), "_", max, 
                                          "_vlag_", vlag, "_Zeitraum_", j, ".png")), 
       width = 10, height = 5, dpi = 1200)

}

