hamta_data_langtidsarb = function(region = c("0020"),
                                  alla_regioner = FALSE,
                                  alla_kommuner = FALSE, 
                                  ta_med_riket = FALSE,
                                  outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                  filnamn = "arbetsloshet.csv", 
                                  cond_code = c("N03926"),
                                  returnera_data = FALSE,
                                  spara_data = TRUE,
                                  senaste_ar = FALSE, # om man enbart vill ha senaste år
                                  tid = 2011:2100){ # Välj ett högt värde som sista värde om alla år skall vara med.

  # ===========================================================================================================
  # 
  # Skript som hämtar data för långtidsarbetslöshet
  # Parametrar som skickas med (= variabler i Kolada-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till TRUE så skriver den över region ovan.
  # - alla_kommuner: Välj om man vill ha alla kommuner. 
  # - ta_med_riket: TRUE om man vill ta med riket också
  # - kpi:  
  # - N03926 - Antal invånare 25-64 år (årsmedelvärde år T) som varit öppet arbetslösa eller i program med aktivitetsstöd i minst sex månader, dividerat med antal invånare 25-64 år den 31/12 år T-1.
  # - N03923 - Antal invånare 15-74 år som varit öppet arbetslösa eller i program med aktivitetsstöd i minst sex månader, dividerat med totalt antal invånare 15-74 år som är öppet arbetslösa eller i program med aktivitetsstöd.
  # - filnamn : Vad skall filen heta
  # - senaste_ar: Sätts till TRUE om man bara vill ha data för senaste år
  # - tid: Vilka år vill man ha? Välj ett högt senaste år om man vill ha alla
  # - returnera_data: TRUE om data skall returneras som en df
  # - spara_data: TRUE om data skall sparas till Excel  
  
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  if(alla_regioner == TRUE){
    region = hamtaAllaLan(tamedriket = FALSE) 
    region = paste0("00",region)
  }
  
  if(alla_kommuner == TRUE){
    region_kommun = hamtakommuner(substr(region,3,4),tamedlan = FALSE,tamedriket = FALSE)
    region = c(region,region_kommun)
  }
  
  if(ta_med_riket == TRUE){
    region = c("0000",region)
  }
  
  if(senaste_ar == TRUE) tid <- max(unique(hamta_kolada_giltiga_ar("N03926",vald_region = region)))

  #### Dra hem variablerna från Kolada
  långtidsarbetslöshet <- get_values(
    kpi = cond_code,
    municipality = region,
    period = tid
  )
  
  långtidsarbetslöshet <- långtidsarbetslöshet %>% 
    select(-c("count","municipality_type","municipality_id")) %>% 
      mutate(kpi = ifelse(kpi == "N03926","Långtidsarbetslöshet_25_64",
                          ifelse(kpi == "N03923","Långtidsarbetslöshet_15_74",kpi)))
  
  # Skall egentligen göras med en case_when, men låter vara tillsvidare då det funkar
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Region Stockholm"] = "Stockholms län"
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Region Uppsala"] = "Uppsala län"             
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Region Sörmland"] = "Södermanlands län"            
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Region Östergötland"] = "Östergötlands län"
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Region Jönköpings län"] = "Jönköpings län"
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Region Kronoberg"] = "Kronobergs län"
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Region Kalmar"] = "Kalmar län"
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Region Gotland"] = "Gotlands län"
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Region Blekinge"] = "Blekinge län"
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Region Skåne"] = "Skåne län"
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Region Halland"] = "Hallands län"
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Västra Götalandsregionen"] = "Västra Götalands län"
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Region Värmland"] = "Värmlands län"
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Region Örebro län"] = "Örebro län"
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Region Västmanland"] = "Västmanlands län"
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Region Dalarna"] = "Dalarnas län"
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Region Gävleborg"] = "Gävleborgs län"
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Region Västernorrland"] = "Västernorrlands län"
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Region Jämtland Härjedalen"] = "Jämtlands län"
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Region Västerbotten"] = "Västerbottens län"
  långtidsarbetslöshet$municipality[långtidsarbetslöshet$municipality=="Region Norrbotten"] = "Norrbottens län"

  # Sparar till CSV om användaren vill det
  if (spara_data == TRUE) write.csv(långtidsarbetslöshet, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(långtidsarbetslöshet)

}