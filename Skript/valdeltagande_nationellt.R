
berakna_valdeltagande_SCB = function(region = "20", # Funkar enbart på länsnivå
                                     outputmapp = NA,
                                     filnamn = "valdeltagande.csv",
                                     returnera_data = FALSE){ 
  
  
  # ===========================================================================================================
  # Skript som beräknar valdeltagande på länsnivå för samtliga val (finns sedan 1973). Inte könsuppdelat
  # Vill man istället ha på kommun eller riksnivå, använd:
  # https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_valdeltagande_region_tid_scb.R
  # Skapat av Jon Frank, 2024-03-06
  # ===========================================================================================================
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 readxl)
  
  # Sourcar in funktioner som hämtar data från SCB
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_valdeltagande_region_tid_scb.R") 
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_rostberattigade_kommunfullmaktige_region_medborgarskap_tid_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_rostberattigade_regionfullmaktige_region_medborgarskap_tid_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_rostberattigade_riksdagsval_region_tid_scb.R")
  
  test = hamta_valdeltagande_region_tid_scb(hamtakommuner("20",tamedlan = FALSE,tamedriket = FALSE),long_format = FALSE)
  
  # Hämtar data
  valdeltagande =  hamta_valdeltagande_region_tid_scb(hamtakommuner("20",tamedlan = FALSE,tamedriket = FALSE),long_format = FALSE)
  
  rostberattigade_EU = hamta_rostberattigade_EU_region_medborgarskap_tid_scb(region_vekt = hamtakommuner(region,tamedriket = FALSE,tamedlan = FALSE)) %>%  
    group_by(regionkoder,region,valår) %>% 
    summarize(rostberattigade = sum(`Europaparlamentsval - röstberättigade`, na.rm = TRUE)) %>% 
    ungroup()
  
  valdeltagande_lan = left_join(valdeltagande_EU,rostberattigade_EU, by = c("regionkoder","region","valår")) %>% 
    mutate(antal_rostande = round(rostberattigade * valdeltagande/100,0)) %>% 
    group_by(valår) %>% 
    summarize(rostberattigade = sum(rostberattigade, na.rm = TRUE),
              antal_rostande = sum(antal_rostande, na.rm = TRUE)) %>% 
    mutate(valdeltagande = round(antal_rostande/rostberattigade*100,1)) %>% 
    ungroup()
  
  # Sparar till Excel om användaren vill det
  if (!is.na(outputmapp) & !is.na(filnamn)) write.csv(valdeltagande_lan, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(valdeltagande_lan)
}
