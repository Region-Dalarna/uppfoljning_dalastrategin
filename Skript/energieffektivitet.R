hamta_data_energieffektivitet = function(region = "Alla", # Alla ger alla län, annars väljer man baserat på kod, exempelivs 20 för Dalarna
                                         ta_med_riket = FALSE, 
                                         outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                         filnamn = "effektivitet.csv", 
                                         tid = c("*")){ # c("*") ger alla år, för enbart senaste år, skriv "senaste")


  #####################################################################
  ################ Indikator 3 - Energieffektivitet ###################
  #####################################################################
  
  ### Energieffektivitet mäts som förädlingsvärde per ton koldioxekvivalenter från ett produktionsperspektiv.
  
  ##################################################################################
  ######### BRP-data från SCB för Dalarna för åren 2012-2019 #######################
  ### Använder PX Web API för att skapa en lista på variablerna vi vill ha #########
  ##################################################################################
  
  source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb,
                 readxl) 
  
  if(region == "Alla"){
    region = hamtaAllaLan(tamedriket=FALSE)
  }
  
  if(ta_med_riket == TRUE){
    region = c("00",region)
  }
  
  if (tid == "senaste"){
    tid = max(hamta_giltiga_varden_fran_tabell("https://api.scb.se/OV0104/v1/doris/sv/ssd/NR/NR0105/NR0105A/NR0105ENS2010T01A", "tid"))
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
  
  ### Region = geografierna som ska ingå
  ### Contentscode = vilken variabel vi vill ha ut
  ### Tid = åren vi vill ha
  
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
  
  ### Tar bort dataframes som vi inte behöver
  # px_data <- NULL
  # pxweb_query_list <- NULL
  
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
  
  write.csv(energieffektivitet,paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
}
