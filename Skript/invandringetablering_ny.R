hamta_data_invandringsetablering = function(region = "20", 
                                            spara_data = TRUE,
                                            returnera_data = FALSE,
                                            outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                            filnamn = "etablering_ny.csv"){ 
  
  
  # ===========================================================================================================
  #
  # Skript för att hämta data från SCB för etableringstid för nyanlända. 
  # 
  # För att få en djupare förklaring av vad som de olika kategorierna under varje variabel betyder, använd: 
  # pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1LanKonUtb", "ContentsCode"), 
  # där man byter till den variabel man är intresserad av
  
  # ===========================================================================================================
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_etableringstid_mm_region_kon_utbniv_bakgrvar_tid_IntGr1LanKonUtb_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_etableringstid_mm_region_kon_utbniv_bakgrvar_tid_IntGr1KomKonUtb_ny_BAS_scb.R")
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb,
                 readxl) 
  
  # Före 2022
  etablering_2021 <- hamta_etableringstid_mm_region_kon_utbniv_bakgrvar_tid_scb(region_vekt = region,
                                                                            kon_klartext = "*",
                                                                            utbniv_klartext = "samtliga utbildningsnivåer",
                                                                            bakgrvar_klartext = c("vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år"),
                                                                            cont_klartext = "Andel förvärvsarbetande (ny definition från och med 2019)",
                                                                            tid_koder = "*") %>% 
    rename(Andel_forvarvsarbetande = `Andel förvärvsarbetande (ny definition från och med 2019)`)
  
  # 2022 - 2023
  etablering_2022_ <- hamta_etableringstid_mm_region_kon_utbniv_bakgrvar_tid_scb_ny(region_vekt = region,
                                                                         kon_klartext = "*",
                                                                         utbniv_klartext = "samtliga utbildningsnivåer",
                                                                         bakgrvar_klartext = c("vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år"),
                                                                         cont_klartext = "Andel sysselsatta",
                                                                         tid_koder = "*") %>% 
    rename(Andel_forvarvsarbetande = `Andel sysselsatta`)
  
  etablering <- rbind(etablering_2021,etablering_2022_)

  
  # Sparar till Excel om användaren vill det
  if (spara_data == TRUE) write.csv(etablering, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(etablering)
}