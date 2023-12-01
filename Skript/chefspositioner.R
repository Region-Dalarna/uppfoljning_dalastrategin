
hamta_data_chefrepresentation <- function(region = "20",
                                          alla_regioner = FALSE,
                                          ta_med_riket = FALSE,
                                          kon = c("1","2"),
                                          utb_niva = c("000"), #Enbart en i taget för tillfället
                                          bakgrund = c("SE","INTTOT"),
                                          tid = c("*"), 
                                          senaste_ar = FALSE,
                                          spara_data = TRUE,
                                          returnera_data = FALSE,
                                          outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                          filnamn = "chefsrepresentation_ny.csv"){

  # ===========================================================================================================
  #
  # Skript för att hämta data från SCB för chefsrepresentation. 
  # 
  # För att få en djupare förklaring av vad som de olika kategorierna under varje variabel betyder, använd: 
  # pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1LanKonUtb", "UtbNiv"), 
  # där man byter till den variabel man är intresserad av
  # 
  # Generellt gäller c("*) om man vill ha alla variabler
  # Parametrar som skickas med (= variabler i SCB-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - utb_niva: Se ovan (pxvardelist)
  # - bakgrund: ""
  # - kon: ""
  # - outputmapp: Vart skall data sparas
  # - filnamn : Vad skall filen heta
  # - senaste_ar: Sätts till TRUE om man bara vill ha data för senaste år
  # - tid: Vilka år vill man ha? Normalt c("*"), men går även att sätta ett intervall.
  # - returnera_data: True om data skall returneras som en df
  # - spara_data: True om data skall sparas till Excel  
  # ===========================================================================================================
  
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               pxweb,
               readxl)

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

if (senaste_ar == TRUE){
  tid = max(hamta_giltiga_varden_fran_tabell("https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1LanKonUtb", "tid"))
}

# Hämtar data från PXweb
pxweb_query_list <- 
  list("Region" = region,
       "Kon" = kon,
       "UtbNiv" = utb_niva,
       "BakgrVar" = bakgrund,
       "ContentsCode" = c("0000001Y"),
       "Tid" = tid)

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

# Sparar till Excel om användaren vill det
if (spara_data == TRUE) write.csv(chefspositioner, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)

# Data returneras som en DF om användaren vill det
if(returnera_data == TRUE) return(chefspositioner)
}

