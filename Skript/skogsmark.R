
hamta_data_skogsmark <- function(region = "20",
                                 alla_regioner = FALSE, 
                                 ta_med_riket = FALSE, 
                                 cont_code = c("0000021D", "0000024O"),
                                 overlapp = c("UÖA"),
                                 former = c("FSS"),
                                 typ_skogsmark = c("PRS"),
                                 tid = c("*"),
                                 senaste_ar = FALSE, 
                                 outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                 filnamn = "skogsmark.csv",
                                 returnera_data = FALSE,
                                 spara_data = TRUE){
  
  # ===========================================================================================================
  #
  # Skript för att hämta data från SCB för andel hektar formellt skyddad produktiv skogsmark
  # För att få en djupare förklaring av vad som de olika kategorierna under varje variabel betyder, använd:
  # pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/MI/MI0605/SkyddSkogFrivillig", "ContentsCode"),  
  # där man byter ContentsCode mot den variabel man är intresserad av.
  # 
  # Generellt gäller c("*) om man vill ha alla variabler
  # Parametrar som skickas med (= variabler i SCB-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - cont_code: 0000021D - Area i hektar, 0000024O - Andel i procent
  # - overlapp: se instruktion ovan (pxvardelis)
  # - typ_skogsmark: ""
  # - former: ""
  # - outputmapp: Vart skall data sparas
  # - filnamn : Vad skall filen heta
  # - senaste_ar: Sätts till TRUE om man bara vill ha data för senaste år
  # - Vilka år vill man ha? Normalt c("*"), men går även att sätta ett intervall.
  # - returnera_data: True om data skall returneras som en df
  # - spara_till_excel: True om data skall sparas till Excel
  # ===========================================================================================================  

  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

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
  
  if (senaste_ar == TRUE){
    tid = max(hamta_giltiga_varden_fran_tabell("https://api.scb.se/OV0104/v1/doris/sv/ssd/MI/MI0605/SkyddSkogFrivillig", "tid"))
  }
  
  pxweb_query_list <- 
    list("Region" = region,
         "Overlapp" = overlapp,
         "TypSkogsmark" = typ_skogsmark,
         "Former" = former,
         "ContentsCode" = cont_code,
         "Tid" = tid)
  
  # Download data 
  px_data <-  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/MI/MI0605/SkyddSkogFrivillig",
              query = pxweb_query_list)
  
  # Convert to data.frame 
  skyddadproduktivskogsmark <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
  
  # Byter namn för att senare kunna slå ihop med andra dataframes samt tar bort onödiga variabler
  skyddadproduktivskogsmark <- skyddadproduktivskogsmark %>% 
    rename(År = år,
           Region = region) %>% 
      select(-c(Former,`Överlapp mellan former`,`Typ av skogsmark`)) %>% 
        rename(area_hektar = `Area i hektar`,
               area_procent = `Andel i procent`)
  
  # Sparar till Excel om användaren vill det
  if (spara_data == TRUE) write.csv(skyddadproduktivskogsmark, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(skyddadproduktivskogsmark)
}
