hamta_data_energieffektivitet = function(region = "20", 
                                         alla_regioner = TRUE, # True om man vill ha alla regioner
                                         ta_med_riket = FALSE, 
                                         outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                         filnamn = "effektivitet.csv",
                                         senaste_ar = FALSE, # True om man enbart vill ha senaste år
                                         spara_data = TRUE,
                                         returnera_data = FALSE,
                                         tid = c("*")){ # c("*") ger alla år

  # ===========================================================================================================
  # 
  # Skript som hämtar data och beräknar energieffektivitet (SCB)
  # Energieffektivitet mäts som förädlingsvärde per ton koldioxekvivalenter från ett produktionsperspektiv.
  # Data hämtas därför för båda dessa parametrar från PXweb
  # Parametrar som skickas med är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - ta_med_riket: TRUE om man vill ta med riket också
  # - filnamn : Vad skall filen heta
  # - senaste_ar: Sätts till TRUE om man bara vill ha data för senaste år
  # - tid: Vilka år vill man ha? Välj ett högt senaste år om man vill ha alla
  # - returnera_data: TRUE om data skall returneras som en df
  # - spara_data: TRUE om data skall sparas till Excel  
  # Länk till statistikdatabasen: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__MI__MI1301__MI1301B/UtslappKommun/
  # ===========================================================================================================
  
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
  
  # Två uttag, där utsläpp (tid_1) normalt har mer "lagg". Testar därför om de är samma, annars sätts senaste år till utsläppens år
  if (senaste_ar == TRUE){
    tid = max(hamta_giltiga_varden_fran_tabell("https://api.scb.se/OV0104/v1/doris/sv/ssd/NR/NR0105/NR0105A/NR0105ENS2010T01A", "tid"))
    tid_1 = max(hamta_giltiga_varden_fran_tabell("https://api.scb.se/OV0104/v1/doris/sv/ssd/MI/MI1301/MI1301B/UtslappLan", "tid"))
    
    if(tid != tid_1){ 
      tid = tid_1
      }
  }
  
  pxweb_query_list <-
    list("Region" = region,
         "ContentsCode" = c("NR0105AH"),
         "Tid"=tid)
  
  ### Sen använder vi listan som vi har skapat för att dra ner datan från SCB
  px_data <- 
    pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/NR/NR0105/NR0105A/NR0105ENS2010T01A",
              query = pxweb_query_list)
  
  ### Lägger datan i en dataframe och ger den ett namn
  brp <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
  
  ### Döp om kolumner
  colnames(brp) <- c("Region", "År", "BRP")
  
  ########################
  ### Data för utsläpp ###
  ########################
 
  pxweb_query_list <- 
    list("Region"=region,
         "SNI2007"=c("A01-F43","G45-T98"),
         "AmneMiljo"=c("GHG"),
         "ContentsCode"=c("0000015P"),
         "Tid"=tid)
  
  ### Sen använder vi listan som vi har skapat för att dra ner datan från SCB
  px_data <- 
    pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/MI/MI1301/MI1301B/UtslappLan",
              query = pxweb_query_list)
  
  ### Lägger datan i en dataframe och ger den ett namn
  utslapp <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

  ### Slår ihop utsläppen för varje år och region
  utslapp <-  utslapp %>% 
              group_by(region, år) %>% 
              summarise(Ämne = sum(Ämne))
  
  ### Byter namn på kolumnerna i utsläpp för att matcha kolumnerna i BRP
  colnames(utslapp) <- c("Region", "År", "Utsläpp")
  
  ### Slår ihop utsläpp och brp
  energieffektivitet <- merge(utslapp, brp, by=c("Region", "År"))
  
  ### Räkna ut energieffektiviteten och pivotera data
  energieffektivitet <- energieffektivitet %>% 
    mutate(effektivitet = BRP/Utsläpp) %>% 
      select(1,2,5) %>% 
        pivot_longer(cols=3,names_to = "variabel",values_to = "value")
  
  # Sparar till CSV om användaren vill det
  if (spara_data == TRUE) write.csv(energieffektivitet, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(energieffektivitet)
}
