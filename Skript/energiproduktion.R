hamta_data_energiproduktion = function(region = c("0020"),
                                       alla_regioner = FALSE,
                                       ta_med_riket = TRUE,
                                       outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                       filnamn = "elproduktion.csv", 
                                       kpi = c("N45926","N45904", "N45927"),
                                       returnera_data = FALSE,
                                       spara_data = TRUE,
                                       senaste_ar = FALSE, # Om man enbart vill ha senaste år
                                       tid = 2012:2100){ # Välj ett högt värde som sista värde om alla år skall vara med.

  # ===========================================================================================================
  # 
  # Skript som hämtar data för energiproduktion från Kolada
  # Parametrar som skickas med (= variabler i Kolada-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - ta_med_riket: TRUE om man vill ta med riket också
  # - kpi:  
  # - N45926 - Elproduktion totalt inom det geografiska området, MWh
  # - N45904 - Elproduktion av vindkraft inom det geografiska området, MWh
  # - N45927 - Elproduktion av vattenkraft inom det geografiska området, MWh
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
  
  if(ta_med_riket == TRUE){
    region = c("0000",region)
  }  

  if(senaste_ar == TRUE) tid <- max(unique(hamta_kolada_giltiga_ar("N45926",vald_region = region)))
  
  #### Dra hem variablerna från Kolada
  elproduktion <- get_values(
    kpi = kpi,
    municipality = region,
    period = tid
  )
  
  # Väljer bort variabler och ger mer rimliga namn.
  elproduktion <- elproduktion %>% 
    select(-c(gender,count,municipality_type,municipality_id)) %>% 
      mutate(kpi = case_when(
        kpi == "N45926" ~ "Totalt",
        kpi == "N45904" ~ "Vindkraft",
        kpi == "N45927" ~ "Vattenkraft"))
  
  ### Beräknar övrig produktion. Detta är enklare om data först görs om till wide
  # elproduktion <- pivot_wider(elproduktion, names_from=kpi, values_from=value) %>%
  #   mutate(Övrigt = Totalt - Vindkraft - Vattenkraft,
  #          `Vattenkraft och övrigt` = Totalt - Vindkraft) %>%
  #     pivot_longer(cols=3:7,names_to = "kpi",values_to = "value")
  
  # elproduktion <- pivot_wider(elproduktion, names_from=kpi, values_from=value) %>% 
  #   mutate(Övrigt = Totalt - Vindkraft - Vattenkraft,
  #          `Vattenkraft och övrigt` = Totalt - Vindkraft) %>% 
  #   pivot_longer(cols=3:7,names_to = "kpi",values_to = "value")
  
  # Sparar till CSV om användaren vill det
  if (spara_data == TRUE) write.csv(elproduktion, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(elproduktion)
}
