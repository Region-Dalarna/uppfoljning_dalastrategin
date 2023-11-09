
hamta_data_chefrepresentation <- function(region = "20",
                                          kon = c("1","2"),
                                          cont_code = c("0000001Y"),
                                          tid = c("*"), # c("*") ger alla år, för enbart senaste år, skriv "senaste"
                                          outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                          filnamn = "chefsrepresentation_ny.csv"){


#### Skript som hämtar data för chefsrepresentation från statistikdatabasen (SCB).  
  
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               pxweb,
               readxl)    

source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

if (tid == "senaste"){
  tid = max(hamta_giltiga_varden_fran_tabell("https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1LanKonUtb", "tid"))
}


# Hämtar data från PXweb
pxweb_query_list <- 
  list("Region"=region,
       "Kon"=kon,
       "UtbNiv"=c("000"),
       "BakgrVar"=c("SE","INTTOT"),
       "ContentsCode"=cont_code,
       "Tid"=tid)

# Download data 
chefspositioner <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1LanKonUtb",
            query = pxweb_query_list)

# Convert to data.frame 
chefspositioner <- as.data.frame(chefspositioner, column.name.type = "text", variable.value.type = "text")

chefspositioner <- chefspositioner %>% 
  select(-utbildningsnivå) %>% 
    mutate(bakgrundsvariabel = ifelse(bakgrundsvariabel =="födelseregion: Sverige", "Inrikes född","Utrikes född")) %>% 
      filter(!(is.na(`Andel i chefsposition, procent`)))

write.csv(chefspositioner, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
}

