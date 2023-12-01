hamta_data_invandringsetablering = function(region = "20", 
                                            alla_regioner = TRUE, # True om man vill ha alla regioner
                                            ta_med_riket = FALSE, 
                                            kon = c("1+2", "1","2"),
                                            utb_niva = c("000"), #Enbart en i taget för tillfället
                                            bakgrund = c("INT010","INT020","INT030","INT040"),
                                            spara_data = TRUE,
                                            returnera_data = FALSE,
                                            outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                            filnamn = "etablering.csv", 
                                            senaste_ar = FALSE, # True om man enbart vill ha senaste år
                                            start_ar = NA, # Om man vill ha från ett specifikt startår till senaste år
                                            tid = c("*")){ 


  # ===========================================================================================================
  #
  # Skript för att hämta data från SCB för etableringstid för nyanlända. 
  # 
  # För att få en djupare förklaring av vad som de olika kategorierna under varje variabel betyder, använd: 
  # pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1LanKonUtb", "ContentsCode"), 
  # där man byter till den variabel man är intresserad av
  # 
  # Generellt gäller c("*) om man vill ha alla variabler
  # Parametrar som skickas med (= variabler i SCB-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - ContentsCode" 0000001X - Andel förvärvsarbetande (ny definition från och med 2019)
  # - utb_niva: Se ovan (pxvardelist)
  # - bakgrund: ""
  # - kon: ""
  # - outputmapp: Vart skall data sparas
  # - filnamn : Vad skall filen heta
  # - start_ar: Om man vill ha från ett specifikt startår till senaste år
  # - senaste_ar: Sätts till TRUE om man bara vill ha data för senaste år
  # - tid: Vilka år vill man ha? Normalt c("*"), men går även att sätta ett intervall.
  # - returnera_data: True om data skall returneras som en df
  # - spara_data: True om data skall sparas till Excel  
  # ===========================================================================================================
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb,
                 readxl) 
  
  if(alla_regioner == TRUE){
    region = hamtaAllaLan(tamedriket = FALSE)
  }
  
  if(ta_med_riket == TRUE){
    region = c("00",region)
  }
  
  if(!(is.na(start_ar))){
    tid = as.character(start_ar:max(hamta_giltiga_varden_fran_tabell("https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1LanKonUtb", "tid")))
  }
  
  if (senaste_ar == TRUE){
    tid = max(hamta_giltiga_varden_fran_tabell("https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1LanKonUtb", "tid"))
  }

  # PXWEB query 
  pxweb_query_list <- 
    list("Region"= region,
         "Kon"= kon,
         "UtbNiv"= utb_niva,
         "BakgrVar" = bakgrund,
         "ContentsCode" = c("0000001X"),
         "Tid"= tid)
  
  # Download data 
  etablering <- 
    pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1LanKonUtb",
              query = pxweb_query_list)
  
  # Convert to data.frame 
  etablering <- as.data.frame(etablering, column.name.type = "text", variable.value.type = "text") %>% 
    rename(Andel_forvarvsarbetande = `Andel förvärvsarbetande (ny definition från och med 2019)`)
  
  # Sparar till Excel om användaren vill det
  if (spara_data == TRUE) write.csv(etablering, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(etablering)
}