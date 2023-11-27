hamta_data_boende = function(region = "20", 
                             alla_regioner = FALSE, 
                             ta_med_riket = FALSE, 
                             Hushallstyp = c("SAMTLH"),
                             Boendeform = c("SMAG","SMBO","SMHY0","FBBO","FBHY0","SPBO","OB","OVR","TOT"),
                             cont_code = c("HE0111DI"),
                             outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                             filnamn = "boendetyper.csv", 
                             senaste_ar = FALSE, 
                             tid = c("*"),
                             returnera_data = FALSE,
                             spara_till_excel = TRUE){ 
  
  # ===========================================================================================================
  #
  # Skript för att hämta data från SCB kopplat till boende. 
  # 
  # För att få en djupare förklaring av vad som de olika kategorierna under varje variabel betyder, använd: 
  # pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/HE/HE0111/HE0111A/HushallT22", "Boendeform"), 
  # där man byter ContentsCode mot den variabel man är intresserad av.
  # 
  # Generellt gäller c("*) om man vill ha alla variabler
  # Parametrar som skickas med (= variabler i SCB-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - Hushallstyp: Vilken typ av hushall är det
  # - Boendeform: Vilken typ av boende är det
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
    tid = max(hamta_giltiga_varden_fran_tabell("https://api.scb.se/OV0104/v1/doris/sv/ssd/HE/HE0111/HE0111A/HushallT22", "tid"))
  }  
  
# Skapa en lista med information som vi vill ha hem -- skapas lättast via pxweb_interactive()
pxweb_query_list <- 
  list("Region"= region,
       "Hushallstyp"= Hushallstyp,
       "Boendeform" = Boendeform,
       "ContentsCode" = cont_code,
       "Tid" = tid)

# Dra hem datan
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/HE/HE0111/HE0111A/HushallT22",
            query = pxweb_query_list)

# Lägg i en dataframe
boende <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Sparar till Excel om användaren vill det
if (spara_till_excel == TRUE) write.csv(boende, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)

# Data returneras som en DF om användaren vill det
if(returnera_data == TRUE) return(boende)

}