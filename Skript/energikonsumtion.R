hamta_data_energikonsumtion = function(region = c("0020"),
                                       alla_regioner = FALSE,
                                       ta_med_riket = FALSE,
                                       outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                       filnamn = "el.csv",
                                       kpi = c("N45906", "N01951"),
                                       returnera_data = FALSE,
                                       spara_data = TRUE,
                                       senaste_ar = FALSE,
                                       tid = 2016:2100){ # Välj ett högt värde som sista värde om alla år skall vara med. 

  # ===========================================================================================================
  # 
  # Skript som relaterar energikonsumtion till energiproduktion.
  # Parametrar som skickas med (= variabler i Kolada-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - ta_med_riket: TRUE om man vill ta med riket också
  # - kpi:  
  # - N45906 - Slutanvändning av el inom det geografiska området, MWh/inv
  # - N01951 - Invånare totalt, antal
  # - Båda behövs för att beräkna konsumtion per person
  # - filnamn : Vad skall filen heta
  # - senaste_ar: Sätts till TRUE om man bara vill ha data för senaste år
  # - tid: Vilka år vill man ha? Välj ett högt senaste år om man vill ha alla
  # - returnera_data: TRUE om data skall returneras som en df
  # - spara_data: TRUE om data skall sparas till Excel  
  
  # ===========================================================================================================

  #### Lista på variabler och koder 
  #### N45926 - Slutanvändning av el inom det geografiska området, MWh/inv
  
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
  
  if(senaste_ar == TRUE) tid <- max(unique(hamta_kolada_giltiga_ar("N45926",vald_region = region)))
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  #### Dra hem variablerna från Kolada
  elkonsumtion <- get_values(
    kpi = kpi,
    municipality = region,
    period = tid
  )

  # Väljer bort variabler och ger mer rimliga namn.
  elkonsumtion <- elkonsumtion %>% 
    filter(gender == "T") %>% 
      select(-c(gender,count,municipality_type,municipality_id)) %>% 
        mutate(kpi = case_when(
          kpi == "N45906" ~ "Slutanvändning av el inom det geografiska området, MWh/inv",
          kpi == "N01951" ~ "Invånare totalt, antal"))
  
  # Beräknar total konsumtion av el. Detta är enklare om data först görs om till wide
  elkonsumtion <- pivot_wider(elkonsumtion, names_from=kpi, values_from=value) %>% 
    mutate(konsumtion = `Invånare totalt, antal`*`Slutanvändning av el inom det geografiska området, MWh/inv`) %>%
      select(-c(`Invånare totalt, antal`,`Slutanvändning av el inom det geografiska området, MWh/inv`)) 
  
  # Läser in data för produktionen av el i Dalarna och relaterar denna till total produktion. Detta ger en självförsörjningsgrad för el 
  source("Skript/energiproduktion.R", encoding="UTF-8")
  el <- hamta_data_energiproduktion(region = region,
                                    ta_med_riket = FALSE,
                                    returnera_data = TRUE, 
                                    spara_data = FALSE,
                                    tid = tid) %>% 
                                      rename(produktion = value) %>% 
                                        filter(kpi == "Totalt")  %>% 
                                          right_join(elkonsumtion,c("year","municipality")) %>% 
                                            mutate(elgrad = (produktion/konsumtion)*100) %>% 
                                              filter(!(is.na(elgrad))) %>% 
                                                select(year,municipality,elgrad)
  
  # Sparar till CSV om användaren vill det
  if (spara_data == TRUE) write.csv(el, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(el)

}
