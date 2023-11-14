hamta_data_invandringsetablering = function(region = "20", 
                                            alla_regioner = TRUE, # True om man vill ha alla regioner
                                            ta_med_riket = FALSE, 
                                            kon = c("1+2", "1","2"),
                                            outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                            filnamn = "etablering.csv", 
                                            senaste_ar = FALSE, # True om man enbart vill ha senaste år
                                            start_ar = NA, # Om man vill ha från ett specifikt startår till senaste år
                                            tid = c("*")){ # c("*") ger alla år


  ####################################
  ### Etableringstid för nyanlända ###
  ####################################
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
         "UtbNiv"= c("000"),
         "BakgrVar" = c("INT010","INT020","INT030","INT040"),
         "ContentsCode" = c("0000001X"),
         "Tid"= tid)
  
  # Download data 
  etablering <- 
    pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1LanKonUtb",
              query = pxweb_query_list)
  
  # Convert to data.frame 
  etablering <- as.data.frame(etablering, column.name.type = "text", variable.value.type = "text") %>% 
    rename(Andel_forvarvsarbetande = `Andel förvärvsarbetande (ny definition från och med 2019)`)
  
  write.csv(etablering,paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
}