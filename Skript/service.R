
hamta_data_service = function(region = c("0020"),
                              alla_regioner = FALSE,
                              ta_med_riket = FALSE,
                              outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                              filnamn = "service.csv",
                              kpi = c("N07530", "N07531"),
                              senaste_ar = FALSE,
                              tid = 2000:2100,
                              returnera_data = FALSE,
                              spara_till_excel = TRUE){ # Välj ett högt värde som sista värde om alla år skall vara med.
  
  
  # ===========================================================================================================
  # Hämtar data för information om närheten till service
  # Parametrar som skickas med (= variabler i Kolada-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - ta_med_riket: TRUE om man vill ta med riket också
  # - kpi: N07530 - Invånare med tillgång till dagligvarubutik inom 2 km, andel (%),
  #   N07531 - Invånare 0-16 år med tillgång till grundskola inom 2 km, andel (%)
  # - filnamn : Vad skall filen heta
  # - senaste_ar: Sätts till TRUE om man bara vill ha data för senaste år
  # - tid: Vilka år vill man ha? Normalt c("*"), men går även att sätta ett intervall.Om man vill ta med sista år sätts det väldigt högt
  # - returnera_data: True om data skall returneras som en df
  # - spara_till_excel: True om data skall sparas till Excel  
  
  # ===========================================================================================================

  # Hämta data för data kopplat till forskning
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
  
  if(senaste_ar == TRUE) tid <- max(unique(hamta_kolada_giltiga_ar("N07531",vald_region = region)))

#### Dra hem variablerna från Kolada
service <- get_values(
  kpi = kpi,
  municipality = region,
  period = tid
)

# Väljer bort variabler och ger mer rimliga namn.
service <- service %>% 
  select(-c(count,municipality_type,municipality_id,gender)) %>% 
    mutate(kpi = case_when(
      kpi == "N07530" ~ "Invånare med tillgång till dagligvarubutik inom 2 km, andel (%)",
      kpi == "N07531" ~ "Invånare 0-16 år med tillgång till grundskola inom 2 km, andel (%)"))

# Sparar till Excel om användaren vill det
if (spara_till_excel == TRUE) write.csv(service, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)

# Data returneras som en DF om användaren vill det
if(returnera_data == TRUE) return(service)

}
