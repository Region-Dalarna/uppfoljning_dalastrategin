hamta_data_matchning = function(region = "20", # Spelar enbart roll om man vill ha ett några län. Sätt annars alla regioner till TRUE.
                                alla_regioner = FALSE, 
                                ta_med_riket = FALSE,
                                alder_fodelseland = "totalt", 
                                Kon = c("1","2","SAMANST"),
                                spara_data = TRUE,
                                returnera_data = FALSE,
                                outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                filnamn = "matchning_ny.csv", 
                                senaste_ar = FALSE, 
                                tid = c("*")){ 

  # ===========================================================================================================
  #
  # Skript för att hämta data från SCB för chefsrepresentation. 
  # 
  # För att få en djupare förklaring av vad som de olika kategorierna under varje variabel betyder, använd: 
  # pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM9906/AM9906A/RegionInd19M2N", "ContentsCode"), 
  # där man byter ContentsCode mot den variabel man är intresserad av.
  # 
  # Generellt gäller c("*) om man vill ha alla variabler
  # Parametrar som skickas med (= variabler i SCB-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - AlderFodelselandgr: Se ovan (pxvardelist)
  # - Kon: ""
  # - bakgrund: ""
  # - kon: ""
  # - outputmapp: Vart skall data sparas
  # - filnamn : Vad skall filen heta
  # - senaste_ar: Sätts till TRUE om man bara vill ha data för senaste år
  # - tid: Vilka år vill man ha? Normalt c("*"), men går även att sätta ett intervall.
  # - returnera_data: True om data skall returneras som en df
  # - spara_data: True om data skall sparas till Excel  
  # ===========================================================================================================
  
  
  # Funktioner som sourcas från Region Dalarna
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
  
  if (senaste_ar == TRUE){
    tid = max(hamta_giltiga_varden_fran_tabell("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906A/RegionInd19M2N1", "tid"))
  }
  
  # Skapa en lista med information som vi vill ha hem 
  pxweb_query_list <- 
    list("Region" = region,
         "Kon" = Kon,
         "AlderFodelselandgr" = alder_fodelseland,
         #"ContentsCode" = c("000005SF"),
         "ContentsCode" = c("000007I3"),
         "Tid"=tid)
  
  # Tidigare "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM9906/AM9906A/RegionInd19M2N"
  # Enligt SCB sker ett skrifte från 2019 när man byter metod. Jag använder enbart ny data. Tidigare var det från 2015
  
  # Download data 
  px_data <- 
    pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906A/RegionInd19M2N1",
              query = pxweb_query_list)
  
  # Convert to data.frame 
  matchning <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text") %>% 
    rename(alder_grupp = `ålder/födelselandgrupp`,
           matchningsgrad =`Matchningsgrad, procent `)

  # Sparar till Excel om användaren vill det
  if (spara_data == TRUE) write.csv(matchning, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(matchning)
  

}
