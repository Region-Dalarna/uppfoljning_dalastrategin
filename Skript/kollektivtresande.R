hamta_data_kollektivtresande = function(region = c("0020"),
                                        alla_regioner = FALSE,
                                        ta_med_riket = FALSE,
                                        outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                        filnamn = "resande.csv", 
                                        kpi = c("U85427", "N60404"),
                                        senaste_ar = FALSE,
                                        tid = 2010:2100,
                                        returnera_data = FALSE,
                                        spara_till_excel = TRUE){ # Välj ett högt värde som sista värde om alla år skall vara med.


  # ===========================================================================================================
  #
  # Skript för att hämta data från Kolada kopplat till kollektiv resande.
  #
  # Parametrar som skickas med (= variabler i SCB-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - ta_med_riket: TRUE om man vill ta med riket också
  # - kpi: U85427 - Marknadsandel för kollektivtrafik, andel (%),N60404 - Resor med kollektivtrafik, resor/inv
  # - filnamn : Vad skall filen heta
  # - senaste_ar: Sätts till TRUE om man bara vill ha data för senaste år
  # - tid: Vilka år vill man ha? Normalt c("*"), men går även att sätta ett intervall.Om man vill ta med sista år sätts det väldigt högt
  # - returnera_data: True om data skall returneras som en df
  # - spara_till_excel: True om data skall sparas till Excel  
  
  # ===========================================================================================================

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
  
  if(senaste_ar == TRUE) tid <- max(unique(hamta_kolada_giltiga_ar("U85427",vald_region = region)))
  
  #### Dra hem variablerna från Kolada
  resande <- get_values(
    kpi = kpi,
    municipality = region,
    period = tid
  )
  
  # Väljer bort variabler och ger mer rimliga namn.
  resande <- resande %>% 
    filter(gender == "T") %>% 
      select(-c(gender,count,municipality_type,municipality_id)) %>% 
        mutate(kpi = case_when(
          kpi == "U85427" ~ "Marknadsandel_procent",
          kpi == "N60404" ~ "Resor_per_invanare"))

  # Sparar till Excel om användaren vill det
  if (spara_till_excel == TRUE) write.csv(resande, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(resande)

}