
hamta_data_valdeltagande = function(region = c("0020"),
                                    alla_regioner = FALSE,
                                    ta_med_riket = FALSE,
                                    outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                    filnamn = "valdeltagande.csv",
                                    kpi = c("N05401", "N05402", "N05403"),
                                    tid = seq(1998,2022,by=4), # Extremt märklig data. Välj år när man vet att det är val
                                    returnera_data = FALSE,
                                    spara_till_excel = TRUE){ # Välj ett högt värde som sista värde om alla år skall vara med.
  
  
  # ===========================================================================================================
  # Hämtar data för information om valdeltagande
  # Parametrar som skickas med (= variabler i Kolada-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - ta_med_riket: TRUE om man vill ta med riket också
  # - kpi: N05401 - Valdeltagande i senaste kommunalvalet, andel (%),
  # -      N05402 - Valdeltagande i senaste regionvalet, andel (%)
  # -      N05403 - Valdeltagande i senaste riksdagsvalet, andel (%)
  # - filnamn : Vad skall filen heta
  # - senaste_ar: Sätts till TRUE om man bara vill ha data för senaste år
  # - tid: Vilka år vill man ha? Speciallösning i detta fall eftersom det även finns data för år när det inte är val (?)
  # - returnera_data: True om data skall returneras som en df
  # - spara_till_excel: True om data skall sparas till Excel  
  
  # ===========================================================================================================

  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)
  
  if(alla_regioner == TRUE){
    region = hamtaAllaLan(tamedriket = FALSE) 
    region = paste0("00",region)
  }
  
  if(ta_med_riket == TRUE){
    region = c("0000",region)
  }  

  #### Dra hem variablerna från Kolada
  valdeltagande <- get_values(
    kpi = kpi,
    municipality = region,
    period = tid
  )
  
  valdeltagande <- valdeltagande %>% 
    select(-c(count,municipality_type,municipality_id)) %>% 
      mutate(kpi = case_when(
        kpi == "N05401" ~ "Valdeltagande_kommun",
        kpi == "N05402" ~ "Valdeltagande_region",
        kpi == "N05403" ~ "Valdeltagande_riksdag"))
  
  # Sparar till Excel om användaren vill det
  if (spara_till_excel == TRUE) write.csv(valdeltagande, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(valdeltagande)
}