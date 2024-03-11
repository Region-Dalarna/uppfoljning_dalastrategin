
berakna_valdeltagande_nationellt = function(region = "20", # Funkar enbart på länsnivå
                                            outputmapp = NA,
                                            filnamn = "valdeltagande_nationellt.csv",
                                            returnera_data = FALSE){ 
  
  
  # ===========================================================================================================
  # Skript som beräknar valdeltagande på länsnivå för samtliga val (finns sedan 1973). Inte könsuppdelat
  # Vill man istället ha på kommun eller riksnivå, använd:
  # https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_valdeltagande_region_tid_scb.R
  # Skapat av Jon Frank, 2024-03-07
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
  
  #test = hamta_valdeltagande_region_tid_scb(hamtakommuner("20",tamedlan = FALSE,tamedriket = FALSE),long_format = FALSE)
  
  # Hämtar data
  valdeltagande =  hamta_valdeltagande_region_tid_scb(hamtakommuner("20",tamedlan = FALSE,tamedriket = FALSE),long_format = FALSE) %>% 
    rename(valdeltagande_riksdag = `Valdeltagande i riksdagsval, procent`,
           valdeltagande_region = `Valdeltagande i regionfullmäktigval, procent`,
           valdeltagande_kommun = `Valdeltagande i kommunfullmäktigval, procent`)
  
  rostberattigade_riksdag = hamta_rostberattigade_riksdagsval_region_tid_scb(region_vekt = hamtakommuner(region,tamedriket = FALSE,tamedlan = FALSE)) %>% 
    rename(rostberattigade_riksdag = `Riksdagsval - röstberättigade`)

  rostberattigade_region = hamta_rostberattigade_regionfullmaktige_region_medborgarskap_tid_scb(region_vekt = hamtakommuner(region,tamedriket = FALSE,tamedlan = FALSE)) %>%  
    group_by(regionkoder,region,valår) %>%
      summarize(rostberattigade_region = sum(`Regionfullmäktigval - röstberättigade`, na.rm = TRUE)) %>%
        ungroup()
  
  rostberattigade_kommun = hamta_rostberattigade_kommunfullmaktige_region_medborgarskap_tid_scb(region_vekt = hamtakommuner(region,tamedriket = FALSE,tamedlan = FALSE)) %>%  
    group_by(regionkoder,region,valår) %>%
      summarize(rostberattigade_kommun = sum(`Kommunfullmäktigval - röstberättigade`, na.rm = TRUE)) %>%
        ungroup()
  
  valdeltagande_lan = left_join(valdeltagande,rostberattigade_riksdag, by = c("regionkoder","region","valår")) %>% 
    left_join(rostberattigade_region, by = c("regionkoder","region","valår")) %>% 
      left_join(rostberattigade_kommun, by = c("regionkoder","region","valår")) %>%
        mutate(antal_rostande_riksdag = round(rostberattigade_riksdag*valdeltagande_riksdag/100,0),
               antal_rostande_region = round(rostberattigade_region*valdeltagande_region/100,0),
               antal_rostande_kommun = round(rostberattigade_kommun*valdeltagande_kommun/100,0)) %>%
          group_by(valår) %>% 
            summarize(rostberattigade_riksdag = sum(rostberattigade_riksdag, na.rm = TRUE),
                      rostberattigade_region = sum(rostberattigade_region, na.rm = TRUE),
                      rostberattigade_kommun = sum(rostberattigade_kommun, na.rm = TRUE),
                      antal_rostande_riksdag = sum(antal_rostande_riksdag, na.rm = TRUE),
                      antal_rostande_region = sum(antal_rostande_region, na.rm = TRUE),
                      antal_rostande_kommun = sum(antal_rostande_kommun,na.rm = TRUE)) %>% 
              mutate(valdeltagande_riksdag = round(antal_rostande_riksdag/rostberattigade_riksdag*100,1),
                       valdeltagande_region = round(antal_rostande_region/rostberattigade_region*100,1),
                       valdeltagande_kommun = round(antal_rostande_kommun/rostberattigade_kommun*100,1)) %>%
                select(valår, valdeltagande_riksdag, valdeltagande_region, valdeltagande_kommun) %>% 
                  ungroup()
  
  # Pivotera data till long-format
  valdeltagande_lan = valdeltagande_lan %>% 
    pivot_longer(cols = -valår, names_to = "val", values_to = "valdeltagande") %>% 
      mutate(val = case_when(val == "valdeltagande_riksdag" ~ "Riksdagsval",
                             val == "valdeltagande_region" ~ "Val till regionfullmäktige",
                             val == "valdeltagande_kommun" ~ "Val till kommunfullmäktige")) %>% 
        select(valår, val, valdeltagande)

  # Sparar till Excel om användaren vill det
  if (!is.na(outputmapp) & !is.na(filnamn)) write.csv(valdeltagande_lan, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(valdeltagande_lan)
}


