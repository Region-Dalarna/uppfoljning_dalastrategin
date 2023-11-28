hamta_data_utsatta = function(region = c("0020"),
                              alla_regioner = TRUE,
                              ta_med_riket = FALSE,
                              outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                              filnamn = "utsatthet_ny.csv",
                              kpi = "N66077",
                              tid = 2007:2100, 
                              senaste_ar = FALSE,
                              returnera_data = FALSE,
                              spara_till_excel = TRUE){ # Välj ett högt värde som sista värde om alla år skall vara med.
  
  
  # ===========================================================================================================
  # 
  # Hämtar data för information om unga i ekonomiskt utsatta hushåll. Finns inte på Kolada längre utan har ersatts
  # med invånare med låg ekonomisk standard
  # Parametrar som skickas med (= variabler i Kolada-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - ta_med_riket: TRUE om man vill ta med riket också
  # - kpi:  
  # - N66080 - Invånare med låg ekonomisk standard.
  # - N66077 - Invånare, 0-19 år, med låg ekonomisk standard, andel (%)
  # - N66078 - Invånare, 20-64 år, med låg ekonomisk standard, andel (%)
  # - N66079 - Invånare, 65+ år, med låg ekonomisk standard, andel (%)
  # - filnamn : Vad skall filen heta
  # - senaste_ar: Sätts till TRUE om man bara vill ha data för senaste år
  # - tid: Vilka år vill man ha? Välj ett högt senaste år om man vill ha alla
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
  
  if(senaste_ar == TRUE) tid <- max(unique(hamta_kolada_giltiga_ar("N66077",vald_region = region)))
  
  #### Dra hem variablerna från Kolada
  utsatta <- get_values(
    kpi = kpi,
    municipality = region,
    period = tid
  )
  
  # Väljer bort variabler och ger mer rimliga namn.
  utsatta <- utsatta %>% 
    select(-c(gender,count,municipality_type)) %>% 
    mutate(kpi = case_when(
      kpi == "N66080" ~ "Låg_ek_standard_totalt",
      kpi == "N66077" ~ "Låg_ek_standard_0_19",
      kpi == "N66078" ~ "Låg_ek_standard_20_64",
      kpi == "N66079" ~ "Låg_ek_standard_65_"))

  # Sparar till Excel om användaren vill det
  if (spara_till_excel == TRUE) write.csv(utsatta, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(utsatta)

}