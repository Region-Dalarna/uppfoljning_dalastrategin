hamta_data_gini = function(region = c("0020", "0000"),
                           alla_regioner = TRUE,
                           ta_med_riket = FALSE,
                           outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                           filnamn = "gini.csv", 
                           senaste_ar = FALSE, # om man enbart vill ha senaste år
                           tid = 2011:2100,
                           returnera_data = FALSE,
                           spara_data = TRUE){ # Välj ett högt värde som sista värde om alla år skall vara med. "senaste år" ger senaste år

  # ===========================================================================================================
  # 
  # Hämtar data för Gini-koefficienten
  # Parametrar som skickas med (= variabler i Kolada-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - ta_med_riket: TRUE om man vill ta med riket också
  # - kpi:  
  # - N00997 - Ginikoefficient
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
  
  if(senaste_ar == TRUE) tid <- max(unique(hamta_kolada_giltiga_ar("N00997",vald_region = region)))
  
  #### Dra hem variablerna från Kolada
  gini <- get_values(
    kpi = c("N00997"),
    municipality = region,
    period = tid
  )
  
  # Tar bort kolumner som inte är relevanta och byter till ett bättre namn
  gini <- gini %>% 
    select(-c("gender","count","municipality_type","municipality_id")) %>% 
      mutate(kpi = ifelse(kpi == "N00997","Ginikoefficient",kpi))

  # Sparar till Excel om användaren vill det
  if (spara_data == TRUE) write.csv(gini, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(gini)
}