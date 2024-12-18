hamta_data_socialtdeltagande = function(region = "20", 
                                       alla_regioner = FALSE, 
                                       ta_med_riket = FALSE, 
                                       Soc_relationer = c("35","40"),
                                       Andel_conf = c("01"),
                                       Kon = c("00","01","02"),
                                       outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                       filnamn = "deltagande_ny.csv", 
                                       senaste_ar = FALSE, 
                                       tid = c("*"), # OBS! Tidsintervall (3-år)
                                       returnera_data = FALSE,
                                       spara_till_excel = TRUE){ 
  
  # ===========================================================================================================
  #
  # Data från Folkhälsomyndigheten för lågt socialt deltagande:
  # Länk: http://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/pxweb/sv/A_Folkhalsodata/A_Folkhalsodata__B_HLV__eSocialarel__aSocialarel/hlv1socxreg.px/
  # 
  # För att få en djupare förklaring av vad som de olika kategorierna under varje variabel betyder, använd: 
  # pxvardelist("http://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/api/v1/sv/A_Folkhalsodata/B_HLV/eSocialarel/aSocialarel/hlv1socxreg.px", "Sociala relationer"), 
  # där man byter ContentsCode mot den variabel man är intresserad av.
  # 
  # Generellt gäller c("*) om man vill ha alla variabler
  # Parametrar som skickas med (= variabler i SCB-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - Soc_relationer: Typ av social relation
  # - Andel_conf: Baseras på en enkät, så andelar och konfidensitervall
  # - outputmapp: Vart skall data sparas
  # - filnamn : Vad skall filen heta
  # - senaste_ar: Sätts till TRUE om man bara vill ha data för senaste år
  # - Vilka år vill man ha? Normalt c("*"), men går även att sätta ett intervall.
  # - returnera_data: True om data skall returneras som en df
  # - spara_till_excel: True om data skall sparas till Excel  
  
  # ===========================================================================================================
  
  # Data som sourcas från Region Dalarna
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Läser in paket
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb,
                 readxl)   
  
  # Diverse olika if-satser som ser till att användarens val genomförs
  if(alla_regioner == TRUE){
    region = hamtaAllaLan(tamedriket = FALSE)
  }
  
  if(ta_med_riket == TRUE){
    region = c("00",region)
  }
  
  # Senaste år
  if (senaste_ar == TRUE){
    tid = max(hamta_giltiga_varden_fran_tabell("https://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/api/v1/sv/A_Folkhalsodata/B_HLV/eSocialarel/aSocialarel/hlv1socxreg.px", "År"))
  }

  pxweb_query_list <- 
    list("Region" = "20",
         "Sociala relationer"= Soc_relationer,
         "Andel och konfidensintervall"= Andel_conf,
         "Kön"= Kon,
         "År" = tid
         )
  
  # Download data 
  px_data <- 
    pxweb_get(url = "https://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/api/v1/sv/A_Folkhalsodata/B_HLV/eSocialarel/aSocialarel/hlv1socxreg.px",
              query = pxweb_query_list)
  
  # Convert to data.frame 
  deltagande <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text") %>%
    rename("Sociala_relationer" = `Sociala relationer`,
           "Andel" = `Sociala relationer efter region, kön och år`,
           "Andel_konfidens" = `Andel och konfidensintervall`) 
    
  
  # Sparar till Excel om användaren vill det
  if (spara_till_excel == TRUE) write.csv(deltagande, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(deltagande)
  
}
