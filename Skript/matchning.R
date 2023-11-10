hamta_data_matchning = function(region = "20", # Alla ger alla län, annars väljer man baserat på kod, exempelivs 20 för Dalarna
                                alla_regioner = FALSE, 
                                ta_med_riket = FALSE,
                                alder_fodelseland = "totalt", # Alternativ "20-64","20-39","Sverige","Norden/EU","Afrika","Asien","Övriga_världen","totalt"
                                outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                filnamn = "matchning.csv", 
                                senaste_ar = FALSE, # True om man enbart vill ha senaste år
                                tid = c("*")){ # c("*") ger alla år

  source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")
  
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
    tid = max(hamta_giltiga_varden_fran_tabell("https://api.scb.se/OV0104/v1/doris/sv/ssd/NR/NR0105/NR0105A/NR0105ENS2010T01A", "tid"))
  }
  
  # Skapa en lista med information som vi vill ha hem 
  pxweb_query_list <- 
    list("Region"=region,
         "Kon"=c("1","2","SAMANST"),
         "AlderFodelselandgr"=alder_fodelseland,
         "ContentsCode"=c("000005SF"),
         "Tid"=tid)
  
  # Download data 
  px_data <- 
    pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM9906/AM9906A/RegionInd19M2N",
              query = pxweb_query_list)
  
  # Convert to data.frame 
  matchning <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
  
  #matchning <- pivot_wider(px_data_frame, names_from=`ålder/födelselandgrupp`, values_from=`Matchningsgrad, procent `)
  
  write.csv(matchning,paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)

}