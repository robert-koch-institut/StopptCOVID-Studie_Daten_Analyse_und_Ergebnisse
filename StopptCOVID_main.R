
# Main File StopptCOVID-Study ----
# written by Andreas Hicketier and Matthias an der Heiden


library(tidyverse)
library(readr)
library(RODBC)
library(tidyquant)
library(ISOweek)
library(readxl)
library(fs)
library(magrittr)
library(ggplot2)
library(lubridate)
library(zoo)
library(slider)
library(R.utils)
options(encoding = "UTF-8")


##  (1a) Import vaccination data from Zenodo --------
source("Skripte/import_impfdaten.R")
##  (1b) Import sequence data from Zenodo --------
source("Skripte/import_seq_daten.R")

##  (2a) Process data from infas --------
# You need to download the data manually from https://www.healthcare-datenplattform.de/dataset/massnahmen_unterkategorien_kreise
# and put all .csv files in data/infas/
source("Skripte/process_measures.R")
##  (2b) Prepare measure data for analysis ----------
source("Skripte/cr_measure_data.R")


##  (3) Compute the correlation matrix of the NPI -----------
source("Skripte/correlation_matrix_main.R")


##  (4a) Create holiday variables ----------------
source("Skripte/holidays.R")
##  (4b) Aggregate measure data on Federal States level and combine with other data -----------
source("Skripte/cr_modeling_data.R")
  ##  (4c) Prepare data for analysis by age group ---------------
source("Skripte/cr_modeling_ag_data.R")

##  (5) Describe the data ----------
source("Skripte/describe_data.R")


##  (6a) Finding the optimal delay for the effect of the vaccination -----
source("Skripte/optim_lag_vacc.R")
##  (6b) Finding the optimal delay for the effect of the NPI -----
source("Skripte/optim_lag_npi.R")

##  (7a) Main model ---------
source("Skripte/Main_model.R")
##  (7b) Main model with varying lag ---------
source("Skripte/Models_rangeLag.R")
##  (7c) Cumulative effects -----------
source("Skripte/Cum_main_model.R")

## (8)Sensitivity analysis ----------
source("Skripte/Sens_Main_model.R")

