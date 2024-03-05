hamta_data_sysselsatta_ny = function(region = c("0020"),
                                  alla_regioner = FALSE,
                                  alla_kommuner = FALSE, 
                                  ta_med_riket = TRUE,
                                  outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                  spara_data = TRUE,
                                  returnera_data = FALSE,
                                  filnamn = "sysselsatt.csv", 
                                  senaste_ar = FALSE, # Om man enbart vill ha senaste år
                                  tid = 2010:2100){ # Välj ett högt värde som sista värde om alla år skall vara med.
  
  # ===========================================================================================================
  #  
  # Parametrar som skickas med (= variabler i Kolada-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - ta_med_riket: TRUE om man vill ta med riket också
  # - kpi:  
  # - N00914 - Antal förvärvsarbetande i åldern 20-64 år dividerat med antal invånare i åldern 20-64 år den 31/12.
  # - filnamn : Vad skall filen heta
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
  
  if(senaste_ar == TRUE) tid <- max(unique(hamta_kolada_giltiga_ar("N00914",vald_region = region)))
  
  #### Dra hem variablerna från Kolada
  sysselsatta <- get_values(
    kpi = c("N00914"),
    municipality = region,
    period = tid
  )
  
  sysselsatta <- sysselsatta %>%
      select(-c("kpi","count","municipality_type","municipality_id")) %>% 
        rename(region = municipality,
               sysselsättningsgrad = value,
               år = year,
               kön = gender) %>% 
                 mutate(år = as.character(år),
                        kön = ifelse(kön == "M", "män", 
                                     ifelse(kön == "K","kvinnor","totalt")),
                        region = ifelse(region == "Region Dalarna","Dalarnas län",region))
  
  region_scb <- substr(region,3,4)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_data_arbetsmarknadsstatus_bas_ar_prel.R")
  sysselsatta_bas = hamta_arbetsmarknadsstatus_bas_ar_prel(region = substr(region,3,4),
                                                           kon_klartext = c("*"),
                                                           fodelseregion_klartext = c("totalt"),
                                                           cont_klartext = "sysselsättningsgrad",
                                                           alder_klartext = "20-64 år") %>% 
    select(-c("regionkoder","ålder","födelseregion"))
  
  
  
  sysselsatta_ut = rbind(sysselsatta,sysselsatta_bas %>% filter(!(år%in%unique(sysselsatta$år)))) %>% 
    filter(region != "Riket")

  # Sparar till CSV om användaren vill det
  if (spara_data == TRUE) write.csv(sysselsatta_ut, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(sysselsatta)
}
