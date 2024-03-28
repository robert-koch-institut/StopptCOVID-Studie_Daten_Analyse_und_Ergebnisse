library(dplyr)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(lubridate)
library(zoo)
library(data.table)
library(multcomp)
library(cowplot)


rm(list = ls())

source("Skripte/helper_main.R")


for (number in 1:2) {


if (number == 1) {
  min = -100L
  vlags = 100L
}

if (number == 2) {
  min = -140L
  vlags = 40L
}




aic_ag <- list()
for (a in 1:4) {

  if (a==1) {
    ag = "0 bis 17 Jahre" 
  }
  if (a==2) {
    ag = "18 bis 59 Jahre"
  }
  if (a==3) {
    ag = "60 und mehr Jahre"
  }

  if (a<=3) {
    print(ag)  
    load(file = file.path("Daten", paste0("modeling_bl_ag.rData")))  
    modeling_bl <- modeling_bl_ag %>% dplyr::filter(AG == ag)
  }
  
  if (a==4) {
    ag = "all"
    print(ag)  
    load(file = file.path("Daten", paste0("modeling_bl.rData")))
  }

  names_var <- names(modeling_bl %>% dplyr::select(starts_with(params)))
  
  if (a==3) {
    
    chosen_lag = 1
    modeling_bl <- lag_data(clag = chosen_lag, varn = names_var, data = modeling_bl)
  }
  
  if (a %in% c(1, 4)) {
    
    chosen_lag = -1L
    modeling_bl <- lag_data(clag = chosen_lag, varn = names_var, data = modeling_bl)
  }
 
  if (a == 2) {
    
    chosen_lag = -2L
    modeling_bl <- lag_data(clag = chosen_lag, varn = names_var, data = modeling_bl)
  }
  

  aic <- list()
  for (vlag in min:vlags) {
    
    cat( vlag )
    ind <- vlag + 1L - min
    aic[[ind]] <- model_fit_vacc(vlag = vlag, data_bl = modeling_bl, 
                                 params = params, number = number, offset = off)
  }
  aic_ag[[a]] <- aic %>% unlist()
  
}
aic_df <- bind_cols(AG1 = aic_ag[[1]], AG2 = aic_ag[[2]], AG3 = aic_ag[[3]], AG4 = aic_ag[[4]])
aic_df %<>% dplyr::mutate(lag = row_number()-1+min)

aic_comp <- pivot_longer(aic_df, cols = starts_with("AG"), names_to = "AG", names_prefix = "AG", values_to = "aic")
aic_comp %<>% dplyr::group_by(AG) %>% dplyr::mutate(min_aic = min(aic),
                                                    AG = as.numeric(AG)) %>% data.frame()
aic_comp %<>% dplyr::mutate(AG_f = factor(AG, labels = c("0 bis 17 Jahre", "18 bis 59 Jahre", "60 und mehr Jahre", "all")))

save(aic_comp, file = file.path("Daten", 
                                paste0("aic_vacc", number, "_optim.rData")))

}





load(file = file.path("Daten", 
                      paste0("aic_vacc", 1, "_optim.rData")))
aic_comp_vacc1 <- aic_comp
summary(aic_comp_vacc1$lag)
rm(aic_comp)

load(file = file.path("Daten", 
                      paste0("aic_vacc", 2, "_optim.rData")))
aic_comp_vacc2 <- aic_comp
rm(aic_comp)


p1 <- ggplot(data = aic_comp_vacc1 %>% dplyr::filter(AG == 3), 
             aes(x = lag)) +
  geom_line(aes(y = aic, color = AG_f)) +
  geom_point(aes(y = min_aic, color = AG_f), data = aic_comp_vacc1 %>% 
               dplyr::filter(aic==min_aic & AG == 3)) +
  labs(y = "AIC", x = "Verzug zwischen Impfdatum und Wirkung auf den R-Wert in Tagen",
       subtitle = "Modell mit Impfquote der 1. Impfung") +
  themeMod(11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x  = element_text(angle=0, vjust=.5)
  ) +
  scale_color_manual(
    values = rev(wes_palette("Zissou1", 
                             1, 
                             type = "continuous")),
    breaks = c("60 und mehr Jahre"),
    labels = c("60+"),
    name = "Age group",
    guide="none") +
  scale_x_continuous(breaks = seq(-100, 100, by = 10))

p1

p2 <- ggplot(data = aic_comp_vacc2 %>% dplyr::filter(AG == 3), 
             aes(x = lag)) +
  geom_line(aes(y = aic, color = AG_f)) +
  geom_point(aes(y = min_aic, color = AG_f), data = aic_comp_vacc2 %>% 
               dplyr::filter(aic==min_aic & AG == 3)) +
  labs(y = "AIC", x = "Verzug zwischen Impfdatum und Wirkung auf den R-Wert in Tagen",
       subtitle = "Modell mit Impfquote der 2. Impfung") +
  themeMod(11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x  = element_text(angle=0, vjust=.5)
  ) +
  scale_color_manual(
    values = rev(wes_palette("Zissou1", 
                             1, 
                             type = "continuous")),
    breaks = c("60 und mehr Jahre"),
    labels = c("60+"),
    name = "Age group",
    guide="none") +
  scale_x_continuous(breaks = seq(-140, 40, by = 10))


plot_grid(p1, p2, labels = "", nrow = 2)
ggsave(file.path("output", paste0("aic_vacc_optim_AG60plus.pdf")), 
       width = 10, height = 5, dpi = 1200)
ggsave(file.path("output", paste0("aic_vacc_optim_AG60plus.png")), 
       width = 10, height = 5, dpi = 1200)
ggsave(file.path("output", paste0("aic_vacc_optim_AG60plus.emf")), 
       width = 10, height = 5, dpi = 1200)


sink(file.path("output", "comp_vacc_AG60plus_optim.txt"), split = T)

with(aic_comp_vacc1 %>% dplyr::filter(aic==min_aic), table(AG, lag)) %>% print()
with(aic_comp_vacc2 %>% dplyr::filter(aic==min_aic), table(AG, lag)) %>% print()

sink()

