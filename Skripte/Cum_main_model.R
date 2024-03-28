library(dplyr)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(lubridate)
library(ggtext)
library(multcomp)
library(RColorBrewer)
library(scales)

source("Skripte/helper_main.R")


load(file = file.path("Daten", "main_model.rData"))


### Cumulative effects in main model -----------------

levels <- c(6, 5, 2, 4, 5, 6, 2, 5, 3, 5, 5, 3, 4, 2, 5)
sum(levels) - length(levels)
length(levels)
sum(levels)

main_model$coefficients %>% attributes()
length(main_model$coefficients)

level_max <- list()

level_max[[1]] <- c(rep(0, times = levels[[1]]-2), 1,
                    rep(0, times = levels[[2]]-2), 1,
                    1,
                    rep(0, times = levels[[4]]-2), 1,
                    rep(0, times = levels[[5]]-2), 1,
                    rep(0, times = levels[[6]]-2), 1,
                    1,
                    rep(0, times = levels[[8]]-2), 1,
                    rep(0, times = levels[[9]]-2), 1,
                    rep(0, times = levels[[10]]-2), 1,
                    rep(0, times = levels[[11]]-2), 1,
                    rep(0, times = levels[[12]]-2), 1,
                    rep(0, times = levels[[13]]-2), 0,  # Test-Maßnahmen
                    1,
                    rep(0, times = levels[[15]]-2), 1,
                    rep(0, times = 6), 0,
                    rep(0, times = 16)
)

level_max[[2]] <- c(rep(0, times = levels[[1]]-3), 1, 0,
                    rep(0, times = levels[[2]]-3), 1, 0,
                    1,
                    rep(0, times = levels[[4]]-3), 1, 0,
                    rep(0, times = levels[[5]]-3), 1, 0,
                    rep(0, times = levels[[6]]-3), 1, 0,
                    1,
                    rep(0, times = levels[[8]]-3), 1, 0,
                    1, 0,
                    rep(0, times = levels[[10]]-3), 1, 0,
                    rep(0, times = levels[[11]]-3), 1, 0,
                    1, 0,
                    rep(0, times = levels[[13]]-3), 0, 0,
                    1,
                    rep(0, times = levels[[15]]-3), 1, 0,
                    rep(0, times = 6), 0,
                    rep(0, times = 16)
)

level_max[[3]] <- c(rep(0, times = levels[[1]]-4), 1, 0, 0,
                    rep(0, times = levels[[2]]-4), 1, 0, 0,
                    1,
                    1, 0, 0,
                    rep(0, times = levels[[5]]-4), 1, 0, 0,
                    rep(0, times = levels[[6]]-4), 1, 0, 0,
                    1,
                    rep(0, times = levels[[8]]-4), 1, 0, 0,
                    1, 0,
                    rep(0, times = levels[[10]]-4), 1, 0, 0,
                    rep(0, times = levels[[11]]-4), 1, 0, 0,
                    1, 0,
                    0, 0, 0,
                    1,
                    rep(0, times = levels[[15]]-4), 1, 0, 0,
                    rep(0, times = 6), 0,
                    rep(0, times = 16)
)

level_max[[4]] <- c(rep(0, times = levels[[1]]-5), 1, 0, 0, 0,
                    1, 0, 0, 0,
                    1,
                    1, 0, 0,
                    1, 0, 0, 0,
                    rep(0, times = levels[[6]]-5), 1, 0, 0, 0,
                    1,
                    1, 0, 0, 0,
                    1, 0,
                    1, 0, 0, 0,
                    1, 0, 0, 0,
                    1, 0,
                    0, 0, 0,
                    1,
                    1, 0, 0, 0,
                    rep(0, times = 6), 0,
                    rep(0, times = 16)
)

level_cont <- list()

level_cont[[1]] <- c(rep(0, times = levels[[1]]-2), 1,
                     rep(0, times = levels[[2]]-2), 1,
                     1,
                     rep(0, times = levels[[4]]-2), 1,
                     rep(0, times = levels[[5]]-2), 1,
                     rep(0, times = levels[[6]]-2), 1,
                     1,
                     rep(0, times = levels[[8]]-2), 1,
                     rep(0, times = levels[[9]]-2), 1,
                     rep(0, times = levels[[10]]-2), 1,
                     rep(0, times = levels[[11]]-2), 1,
                     rep(0, times = levels[[12]]-2), 1,
                     rep(0, times = levels[[13]]-2), 0,  # Test-Maßnahmen
                     1,
                     rep(0, times = levels[[15]]-2), 1,
                     rep(0, times = 6), 0,
                     rep(0, times = 16)
)

level_cont[[2]] <- c(rep(0, times = levels[[1]]-3), 1, 0,
                     rep(0, times = levels[[2]]-3), 1, 0,
                     0,
                     rep(0, times = levels[[4]]-3), 1, 0,
                     rep(0, times = levels[[5]]-3), 1, 0,
                     rep(0, times = levels[[6]]-3), 1, 0,
                     0,
                     rep(0, times = levels[[8]]-3), 1, 0,
                     1, 0,
                     rep(0, times = levels[[10]]-3), 1, 0,
                     rep(0, times = levels[[11]]-3), 1, 0,
                     1, 0,
                     rep(0, times = levels[[13]]-3), 0, 0,
                     0,
                     rep(0, times = levels[[15]]-3), 1, 0,
                     rep(0, times = 6), 0,
                     rep(0, times = 16)
)

level_cont[[3]] <- c(rep(0, times = levels[[1]]-4), 1, 0, 0,
                     rep(0, times = levels[[2]]-4), 1, 0, 0,
                     0,
                     1, 0, 0,
                     rep(0, times = levels[[5]]-4), 1, 0, 0,
                     rep(0, times = levels[[6]]-4), 1, 0, 0,
                     0,
                     rep(0, times = levels[[8]]-4), 1, 0, 0,
                     1, 0,
                     rep(0, times = levels[[10]]-4), 1, 0, 0,
                     rep(0, times = levels[[11]]-4), 1, 0, 0,
                     1, 0,
                     0, 0, 0,
                     0,
                     rep(0, times = levels[[15]]-4), 1, 0, 0,
                     rep(0, times = 6), 0,
                     rep(0, times = 16)
)

level_cont[[4]] <- c(rep(0, times = levels[[1]]-5), 1, 0, 0, 0,
                     1, 0, 0, 0,
                     0,
                     0, 0, 0,
                     1, 0, 0, 0,
                     rep(0, times = levels[[6]]-5), 1, 0, 0, 0,
                     0,
                     1, 0, 0, 0,
                     0, 0,
                     1, 0, 0, 0,
                     1, 0, 0, 0,
                     0, 0,
                     0, 0, 0,
                     0,
                     1, 0, 0, 0,
                     rep(0, times = 6), 0,
                     rep(0, times = 16)
)

cum_eff <- data.frame(Estimate = double(),
                      lwr = double(),
                      upr = double())

eff_meas_cont <- list()
eff_meas_max <- list()



for (i in seq(4,1, by=-1)) {
  length(level_cont[[i]])
  eff_meas_cont[[i]] <- glht(main_model, linfct = t(level_cont[[i]]))
  confint(eff_meas_cont[[i]])$confint %>% exp() %>% print()
  cum_eff[5-i,] <- confint(eff_meas_cont[[i]])$confint %>% exp() %>% data.frame()
}

for (i in seq(4,1, by=-1)) {
  
  length(level_max[[i]])
  eff_meas_max[[i]] <- glht(main_model, linfct = t(level_max[[i]]))
  confint(eff_meas_max[[i]])$confint %>% exp() %>% print()
  cum_eff[9-i,] <- confint(eff_meas_max[[i]])$confint %>% exp() %>% data.frame()
}

vacc <- list()
eff_vacc <- list()

length(main_model$coefficients)
length(coef(main_model))

for (i in 1:3) {
  vacc[[i]] <- c(rep(0, times = 70-17), i, 
                 rep(0, times = 16))
  length(vacc[[i]])
  eff_vacc[[i]] <- glht(main_model, linfct = t(vacc[[i]]))
  cum_eff[8+i,] <- confint(eff_vacc[[i]])$confint %>% exp()
}


cum_eff$var <- c("StufeC_2", "StufeC_3", "StufeC_4", "StufeC_5", 
                 "Stufe_2", "Stufe_3", "Stufe_4", "Stufe_5", 
                 "vacc_50%", "vacc_75%", "vacc_87,5%")
cum_eff %<>% rowwise %>% dplyr::mutate(type = strsplit(var, "\\_", perl=T)[[1]][[1]],
                                       strength = gsub("vacc_", "", var))

cum_eff %<>% dplyr::mutate(strength_e = factor(strength, 
                                               levels = unique(cum_eff$strength)))
cum_eff %<>% dplyr::mutate(strength_f = gsub("Stufe_", "Stufe ", 
                                                gsub("StufeC_", "Stufe ", 
                                                         strength)),
                           strength_f_engl = gsub("Stufe ", "Strength ", strength_f)) %>% data.frame()

with(cum_eff, table(type))



sink(file = file.path("output", "npi_vaccine_effect.txt"), split = T)

cum_eff %>% dplyr::select(type, strength_e, Estimate, lwr, upr)

main_model$coefficients[70-16+1] %>% attributes()

cum_eff %>% 
  dplyr::filter(type=="StufeC") %>% dplyr::summarise(level = unique(strength_e),
                                                     PE = round(Estimate*100, digits = 1),
                                                     lb = round(lwr*100, digits = 1),
                                                     ub = round(upr*100, digits = 1))

round(exp(main_model$coefficients[[70-16+1]]*1) *100, digits = 1)
round(exp(main_model$coefficients[[70-16+1]]*2) *100, digits = 1)
round(exp(main_model$coefficients[[70-16+1]]*3) *100, digits = 1)


cat("\n Reduktion \n")
cum_eff %>% 
  dplyr::filter(type=="StufeC") %>% dplyr::summarise(level = unique(strength_e),
                                                     PE = 100 - round(Estimate*100, digits = 1),
                                                     lb = 100 - round(lwr*100, digits = 1),
                                                     ub = 100 - round(upr*100, digits = 1))

100 - round(exp(main_model$coefficients[[70-16+1]]*1) *100, digits = 1)
100 - round(exp(main_model$coefficients[[70-16+1]]*2) *100, digits = 1)
100 - round(exp(main_model$coefficients[[70-16+1]]*3) *100, digits = 1)


sink()



ggplot(data = cum_eff, aes(x = strength_e)) +
  geom_bar(aes(y = Estimate, fill = type), stat = "identity") +
  geom_errorbar(aes(ymin = lwr, ymax = upr)) +
  labs(y = "Wirkung auf den R-Wert", x ="Stärke der Intervention") +
  themeMod(11) +
  theme(panel.grid.major.x = element_blank(),
        plot.title.position = "plot",
        axis.text.x = element_text(angle=45, vjust = 0.6)) +
  scale_fill_discrete(
    breaks = c("StufeC", "Stufe", "vacc"),
    labels = c("Strategie 1", "Strategie 2", "Impfung"),
    name = "Intervention") +
  scale_y_continuous(breaks = seq(0, .7, by = .1)) +
  scale_x_discrete(labels = cum_eff$strength_f)

ggsave(file.path("output", paste0("cum_main_model.pdf")), 
       width = 7, height = 4, dpi = 1200)
ggsave(file.path("output", paste0("cum_main_model.png")), 
       width = 7, height = 4, dpi = 1200)


ggplot(data = cum_eff, aes(x = strength_e)) +
  geom_bar(aes(y = Estimate, fill = type), stat = "identity") +
  geom_errorbar(aes(ymin = lwr, ymax = upr)) +
  labs(y = "Effect on R-value", x ="Strength of intervention") +
  themeMod(11) +
  theme(panel.grid.major.x = element_blank(),
        plot.title.position = "plot",
        axis.text.x = element_text(angle=45, vjust = 0.6)) +
  scale_fill_manual(
    values = brewer.pal(4, "Set1")[c(4, 2, 3)],
    breaks = c("StufeC", "Stufe", "vacc"),
    labels = c("Strategy 1", "Strategy 2", "Vaccination"),
    name = "Intervention") +
  scale_y_continuous(breaks = seq(0, .7, by = .1)) +
  scale_x_discrete(labels = cum_eff$strength_f_engl)

ggsave(file.path("output", paste0("cum_main_model_engl.pdf")), 
       width = 7, height = 4, dpi = 1200)
ggsave(file.path("output", paste0("cum_main_model_engl.png")), 
       width = 7, height = 4, dpi = 1200)



eff_short <- cum_eff %>% dplyr::filter(type != "Stufe")

ggplot(data = eff_short, aes(x = strength_e)) +
  geom_bar(aes(y = 1-Estimate, fill = type), stat = "identity") +
  geom_errorbar(aes(ymin = 1-upr, ymax = 1-lwr)) +
  labs(y = "Reduction of R-value", x ="Strength of intervention") +
  themeMod(11) +
  theme(panel.grid.major.x = element_blank(),
        plot.title.position = "plot",
        axis.text.x = element_text(angle=45, vjust = 0.6)) +
  scale_fill_manual(
    values = brewer.pal(4, "Set1")[c(4, 3)],
    breaks = c("StufeC", "vacc"),
    labels = c("NPI measures", "Vaccination"),
    name = "Intervention") +
  scale_y_continuous(breaks = seq(0, .7, by = .1), labels=percent) +
  scale_x_discrete(labels = eff_short$strength_f_engl)

ggsave(file.path("output", paste0("cum_main_model_engl_short.pdf")), 
       width = 7, height = 4, dpi = 1200)
ggsave(file.path("output", paste0("cum_main_model_engl_short.png")), 
       width = 7, height = 4, dpi = 1200)
