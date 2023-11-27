hamta_data_trangboddhet = function(region = "20", 
                                   alla_regioner = FALSE, 
                                   ta_med_riket = FALSE, 
                                   trang = c("TRÅNGB N2"), 
                                   fodelseregion = c("*"), 
                                   Alder = c("*"),
                                   Kon = c("*"), 
                                   cont_code = c("000004PS"),
                                   outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                   filnamn = "trang.csv", 
                                   senaste_ar = FALSE, 
                                   tid = c("*"),
                                   returnera_data = FALSE,
                                   spara_till_excel = TRUE){ 
  
  # ===========================================================================================================
  #
  # Skript för att hämta data från SCB koplpat trångboddhet. 
  # 
  # För att få en djupare förklaring av vad som de olika kategorierna under varje variabel betyder, använd: 
  # pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/LE/LE0105/LE0105B/LE0105Boende02", "ContentsCode"), där man byter Fodelseregion mot vald variabel
  # 
  # Generellt gäller c("*) om man vill ha alla variabler
  # Parametrar som skickas med (= variabler i SCB-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - trang: Olika former av trångboddhet: c("TOT"),c("ej trångbodda enligt norm 2"),c("US"),
  # - Födelseregion: "200","020","030","040","050","010","100" 
  # - Alder: En mängd kategorier. Använd pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/LE/LE0105/LE0105B/LE0105Boende02", "Alder")
  # - Kon: "TOT2" - totalt,"100" - kvinnor ,"200" - män
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
    tid = max(hamta_giltiga_varden_fran_tabell("https://api.scb.se/OV0104/v1/doris/sv/ssd/LE/LE0105/LE0105B/LE0105Boende02", "tid"))
  }
  
  # Själva hämtningen av data
  pxweb_query_list <- 
  list("Region" = region,
       "Trangboddhet" = trang,
       "Fodelseregion"= fodelseregion,
       "Alder" = Alder,
       "Kon" = Kon,
       "ContentsCode" = cont_code,
       "Tid" = tid)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/LE/LE0105/LE0105B/LE0105Boende02",
            query = pxweb_query_list)


trångdboddhet <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Sparar till Excel om användaren vill det
if (spara_till_excel == TRUE) write.csv(trångdboddhet, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)

# Data returneras som en DF om användaren vill det
if(returnera_data == TRUE) return(trångdboddhet)

}