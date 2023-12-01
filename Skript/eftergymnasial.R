hamta_data_eftergymnasial = function(region = c("0020"),
                                     alla_regioner = FALSE,
                                     alla_kommuner = FALSE, 
                                     ta_med_riket = FALSE,
                                     returnera_data = FALSE,
                                     spara_data = TRUE,
                                     outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                     filnamn = "eftergym.csv", 
                                     senaste_ar = FALSE, 
                                     tid = 2013:2100){ 
  
  # ===========================================================================================================
  # 
  # Skript som hämtar data för långtidsarbetslöshet
  # Parametrar som skickas med (= variabler i Kolada-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till TRUE så skriver den över region ovan.
  # - alla_kommuner: Välj om man vill ha alla kommuner. 
  # - ta_med_riket: TRUE om man vill ta med riket också
  # - kpi:  
  # - N01982 - Invånare med eftergymnasial utbildning 25-64 år, andel (%). Eftergymnasial avser: eftergymnasial utbildning kortare än 3 år, längre än 3 år samt forskarutbildning
  # - senaste_ar: Sätts till TRUE om man bara vill ha data för senaste år
  # - tid: Vilka år vill man ha? Välj ett högt senaste år om man vill ha alla
  # - returnera_data: TRUE om data skall returneras som en df
  # - spara_data: TRUE om data skall sparas till Excel  
  
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
  
  if(alla_kommuner == TRUE){
    region_kommun = hamtakommuner(substr(region,3,4),tamedlan = FALSE,tamedriket = FALSE)
    region = c(region,region_kommun)
  }
  
  if(ta_med_riket == TRUE){
    region = c("0000",region)
  }
  
  if(senaste_ar == TRUE) tid <- max(unique(hamta_kolada_giltiga_ar("N01982",vald_region = region)))
  
  #### Dra hem variablerna från Kolada
  eftergymnasial <- get_values(
    kpi = c("N01982"),
    municipality = region,
    period = tid
  )
  
  eftergymnasial <- eftergymnasial %>% 
    select(-c("count","municipality_type","municipality_id")) %>% 
      mutate(kpi = ifelse(kpi == "N01982","andel_eftergym",kpi))
  
  # Sparar till CSV om användaren vill det
  if (spara_data == TRUE) write.csv(eftergymnasial, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(eftergymnasial)
}