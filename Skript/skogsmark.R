
hamta_data_skogsmark <- function(region = "20",
                                 senaste_ar = FALSE, # Om man bara vill ha senaste år
                                 tid = c("*"),# c("*") ger alla år
                                 cont_code = c("0000021D", "0000024O"),
                                 outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                 filnamn = "skogsmark.csv"){

#######################################################################
### Indikator 9 - Andel hektar formellt skyddad produktiv skogsmark ###
#######################################################################

############################################
### Formellt skyddad produktiv skogsmark ###
############################################
# Länk SCB
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__MI__MI0605/SkyddSkogFrivillig/
  
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb,
                 readxl)
  
  if (senaste_ar == TRUE){
    tid = max(hamta_giltiga_varden_fran_tabell("https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__MI__MI0605/SkyddSkogFrivillig/", "tid"))
  }
  
  pxweb_query_list <- 
    list("Region"=region,
         "Overlapp"=c("UÖA"),
         "TypSkogsmark"=c("PRS"),
         "Former"=c("FSS"),
         "ContentsCode"=cont_code,
         "Tid"=c("*"))
  
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
  
  write.csv(skyddadproduktivskogsmark,paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
}
