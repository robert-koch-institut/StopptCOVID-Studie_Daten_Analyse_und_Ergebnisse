library(readr)
library(dplyr)
library(magrittr)

source("Skripte/process_measures_functions.R")

# We use the NPI data collected by Infas 360 GmbH
# You need to download the data manually from https://www.healthcare-datenplattform.de/dataset/massnahmen_unterkategorien_kreise
# and put all .csv files in Daten/infas/

variables <- c("01a", "01b", "02a", "02b", paste0("0",3:9), as.character(10:21))

for (var in variables) {
  new_measure <-
    read_csv(file.path("Daten", "infas", paste0("kr_massn_unterkat_m",var,".csv")),
             col_types = cols(datum = col_date(format = "%Y-%m-%d")))
  if(var=="01a"){
    joined_measures <- new_measure
  } else {
    joined_measures <- full_join(joined_measures, new_measure, by = c("ags5", "kreis", "datum"))  
  }
  print(paste0("var: ", var, " unique_ags5: ", length(unique(new_measure$ags5)), " dates: ",length(unique(new_measure$datum)), " observations: ", nrow(new_measure)))
  
}


renamed_measures <- rename_with(joined_measures, ~ gsub("code_m", "M", .x, fixed = TRUE))
cleaned_measures <- clean_measures_fine_grained(renamed_measures)
only_measures <- dplyr::select(cleaned_measures, ags5, kreis, datum, contains("categorical")) 
only_measures <- rename(only_measures, date = datum)
imputed_measures <- impute_NAs(only_measures)

imputed_measures %<>% dplyr::filter(date <= as.Date("2021/09/15", "%Y/%m/%d") & 
                                      ags5 != "16056")


save(imputed_measures, file = file.path("Daten", "npi_measures.rData"))

