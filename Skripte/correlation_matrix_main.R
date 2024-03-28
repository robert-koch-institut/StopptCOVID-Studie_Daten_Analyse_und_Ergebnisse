library(tidyverse)
library(magrittr)
library(lubridate)
library(ggplot2)

rm(list = ls())

source("Skripte/helper_main.R")

## Compute correlation matrix ----------

load(file = file.path("Daten", "measures_simplified.rData"))

names_meas <- names(imputed_measures %>% dplyr::select(contains("measures")))
names_meas

length(names_meas)

imputed_measures_shorter_names <- 
  imputed_measures %>%
  transmute(
    daycare = daycare_measures,
    schools = schools_measures,
    space_public = public_space_measures,
    space_private = private_space_measures,
    stay_at_home = curfew_measures,
    public_event = public_event_measures,
    services = services_measures,
    sport = sport_measures,
    culture = culture_education_measures,
    hospitality = hospitality_measures,
    gastronomy = gastronomy_measures,
    retail = retail_measures,
    nightlife = nightlife_measures,
    workplace = workplace_measures,
    masks = mask_measures,
    abstand = abstand_measures,
    test = test_measures
  )

length(imputed_measures_shorter_names)


cor <- imputed_measures_shorter_names %>% cor() 

colnames(cor) <- label_measures_orig
rownames(cor) <- label_measures_orig

cor03 <- ifelse(cor>.3,cor, 0) 
cor04 <- ifelse(cor>.4,cor, 0) 
cor05 <- ifelse(cor>.5,cor, 0) 
cor055 <- ifelse(cor>.55,cor, 0) 
cor06 <- ifelse(cor>.6,cor, 0) 
cor065 <- ifelse(cor>.65,cor, 0) 
cor07 <- ifelse(cor>.7,cor, 0) 
cor075 <- ifelse(cor>.75,cor, 0) 
cor08 <- ifelse(cor>.8,cor, 0) 

# exit

path = file.path("output", "Correlation_matrix.pdf")
pdf(file = path)

cor %>%
  corrplot::corrplot()
title(main= expression("Alle Korrelationen"), adj = 0.01)

cor03 %>%
  corrplot::corrplot()
title(main= expression("Korrelation ">=" 30%"), adj = 0.01)
  
cor04 %>%
  corrplot::corrplot()
title(main= expression("Korrelation ">=" 40%"), adj = 0.01)

cor05 %>%
  corrplot::corrplot()
title(main= expression("Korrelation ">=" 50%"), adj = 0.01)

cor055 %>%
  corrplot::corrplot()
title(main= expression("Korrelation ">=" 55%"), adj = 0.01)

cor06 %>%
  corrplot::corrplot()
title(main= expression("Korrelation ">=" 60%"), adj = 0.01)

cor065 %>%
  corrplot::corrplot()
title(main= expression("Korrelation ">=" 65%"), adj = 0.01)

cor07 %>%
  corrplot::corrplot()
title(main= expression("Korrelation ">=" 70%"), adj = 0.01)

cor075 %>%
  corrplot::corrplot()
title(main= expression("Korrelation ">=" 75%"), adj = 0.01)

cor08 %>%
  corrplot::corrplot()
title(main= expression("Korrelation ">=" 80%"), adj = 0.01)

dev.off()


path = file.path("output", "Correlation_matrix_.png")
png(file = path, width = 600, height = 600, units = "px", pointsize = 12)

cor %>% corrplot::corrplot()
title(main= expression("Alle Korrelationen"), adj = 0.01)

dev.off()

path = file.path("output", "Correlation_matrix_55.png")
png(file = path, width = 600, height = 600, units = "px", pointsize = 12)

cor055 %>%
  corrplot::corrplot()
title(main= expression("Korrelation ">=" 55%"), adj = 0.01)
dev.off()


path = file.path("output", "Correlation_matrix_60.png")
png(file = path, width = 600, height = 600, units = "px", pointsize = 12)

cor06 %>%
  corrplot::corrplot()
title(main= expression("Korrelation ">=" 60%"), adj = 0.01)
dev.off()

path = file.path("output", "Correlation_matrix_65.png")
png(file = path, width = 600, height = 600, units = "px", pointsize = 12)

cor065 %>%
  corrplot::corrplot()
title(main= expression("Korrelation ">=" 65%"), adj = 0.01)
dev.off()


path = file.path("output", "Correlation_matrix_70.png")
png(file = path, width = 600, height = 600, units = "px", pointsize = 12)

cor07 %>%
  corrplot::corrplot()
title(main= expression("Korrelation ">=" 70%"), adj = 0.01)

dev.off()

