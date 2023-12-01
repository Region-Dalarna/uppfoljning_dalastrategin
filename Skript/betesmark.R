
hamta_data_betesmark = function(region = "0020",
                                alla_regioner = FALSE,
                                ta_med_riket = FALSE,
                                cont_code = c("N00750"),
                                outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                filnamn = "betesmark.csv",
                                senaste_ar = FALSE, # om man enbart vill ha senaste år
                                returnera_data = FALSE,
                                spara_data = TRUE,
                                tid = 2011:2100 ){# Om flera år, välj ett sent startår så tas sista år med automatiskt.

  # ===========================================================================================================
  # 
  # Skript som hämtar data för betesmark
  # Parametrar som skickas med (= variabler i Kolada-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - ta_med_riket: TRUE om man vill ta med riket också
  # - kpi:  
  # - N00750: Total_betesmark
  # - filnamn : Vad skall filen heta
  # - senaste_ar: Sätts till TRUE om man bara vill ha data för senaste år
  # - tid: Vilka år vill man ha? Välj ett högt senaste år om man vill ha alla
  # - returnera_data: TRUE om data skall returneras som en df
  # - spara_data: TRUE om data skall sparas till CSV
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  if(alla_regioner == TRUE){
    region = hamtaAllaLan(tamedriket = FALSE) 
    region = paste0("00",region)
  }
  
  if(ta_med_riket == TRUE){
    region = c("0000",region)
  }  
  
  if(senaste_ar == TRUE) tid <- max(unique(hamta_kolada_giltiga_ar("N00750",vald_region = region)))
  
  #### Dra hem variablerna från Kolada
  betesmark <- get_values(
    kpi = cont_code,
    municipality = region,
    period = tid)
  
  # Byter namn på variabel och tar bort de vi inte behöver
  betesmark <- betesmark %>% 
    select(-c(gender,count,municipality_type)) %>%
      mutate(kpi = case_when(
        kpi == "N00750" ~ "Total_betesmark"))
  
  # Sparar till Excel om användaren vill det
  if (spara_data == TRUE) write.csv(betesmark, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(betesmark)
}
