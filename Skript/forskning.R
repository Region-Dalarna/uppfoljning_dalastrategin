
hamta_data_forskning = function(region = "0020", # Används bara om man vill ha en eller några regioner
                                alla_regioner = TRUE, # True om man vill ha alla regioner
                                ta_med_riket = FALSE, 
                                kpi = c("N85085", "N85086", "N85087"),
                                returnera_data = FALSE,
                                spara_data = TRUE,
                                outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                filnamn = "forskning.csv", 
                                senaste_ar = FALSE, # Om man enbart vill ha senaste år
                                tid = 2011:2100){ # Välj ett högt värde som sista värde om alla år skall vara med.


  # ===========================================================================================================
  # 
  # Skript som hämtar data kopplat till forskning
  # Parametrar som skickas med (= variabler i Kolada-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till TRUE så skriver den över region ovan.
  # - alla_kommuner: Välj om man vill ha alla kommuner. 
  # - ta_med_riket: TRUE om man vill ta med riket också
  # - kpi:  
  # - N85085 - Företagens utgifter i kronor för egen Forskning och utveckling dividerat med befolkning (31/12) respektive år
  # - N85086 - Offentliga sektorns utgifter i kronor för Forskning och utveckling dividerat med befolkningen (31/12) respektive år
  # - N85087 - Universitets- och högskolesektorns utgifter i kronor för Forskning och utveckling dividerat med befolkningen (31/12) respektive år.
  # - filnamn : Vad skall filen heta
  # - senaste_ar: Sätts till TRUE om man bara vill ha data för senaste år
  # - tid: Vilka år vill man ha? Välj ett högt senaste år om man vill ha alla
  # - returnera_data: TRUE om data skall returneras som en df
  # - spara_data: TRUE om data skall sparas till Excel  
  
  # ===========================================================================================================
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)
  
  if(alla_regioner == TRUE){
    region = hamtaAllaLan(tamedriket = FALSE) 
    region = paste0("00",region)
  }
  
  if(ta_med_riket == TRUE){
    region = c("0000",region)
  }
  
  if(senaste_ar == TRUE) tid <- max(unique(hamta_kolada_giltiga_ar("N85085",vald_region = region)))
  
  #### Dra hem variablerna från Kolada
  forskning <- get_values(
    kpi = c("N85085", "N85086", "N85087"),
    municipality = region,
    period = tid
  )
  
  # Väljer bort variabler och ger mer rimliga namn.
  forskning <- forskning %>% 
    select(-c(gender,count,municipality_type,municipality_id)) %>% 
      mutate(kpi = case_when(
        kpi == "N85085" ~ "Företagens utgifter för egen FoU",
        kpi == "N85086" ~ "Offentliga sektorns utgifter för FoU",
        kpi == "N85087" ~ "Universitets- och högskolesektorns utgifter för FoU"))

 
  # Skall egentligen göras med en case_when, men låter vara tillsvidare då det funkar
  forskning$municipality[forskning$municipality=="Region Stockholm"] = "Stockholms län"
  forskning$municipality[forskning$municipality=="Region Uppsala"] = "Uppsala län"             
  forskning$municipality[forskning$municipality=="Region Sörmland"] = "Södermanlands län"            
  forskning$municipality[forskning$municipality=="Region Östergötland"] = "Östergötlands län"
  forskning$municipality[forskning$municipality=="Region Jönköpings län"] = "Jönköpings län"
  forskning$municipality[forskning$municipality=="Region Kronoberg"] = "Kronobergs län"
  forskning$municipality[forskning$municipality=="Region Kalmar"] = "Kalmar län"
  forskning$municipality[forskning$municipality=="Region Gotland"] = "Gotlands län"
  forskning$municipality[forskning$municipality=="Region Blekinge"] = "Blekinge län"
  forskning$municipality[forskning$municipality=="Region Skåne"] = "Skåne län"
  forskning$municipality[forskning$municipality=="Region Halland"] = "Hallands län"
  forskning$municipality[forskning$municipality=="Västra Götalandsregionen"] = "Västra Götalands län"
  forskning$municipality[forskning$municipality=="Region Värmland"] = "Värmlands län"
  forskning$municipality[forskning$municipality=="Region Örebro län"] = "Örebro län"
  forskning$municipality[forskning$municipality=="Region Västmanland"] = "Västmanlands län"
  forskning$municipality[forskning$municipality=="Region Dalarna"] = "Dalarnas län"
  forskning$municipality[forskning$municipality=="Region Gävleborg"] = "Gävleborgs län"
  forskning$municipality[forskning$municipality=="Region Västernorrland"] = "Västernorrlands län"
  forskning$municipality[forskning$municipality=="Region Jämtland Härjedalen"] = "Jämtlands län"
  forskning$municipality[forskning$municipality=="Region Västerbotten"] = "Västerbottens län"
  forskning$municipality[forskning$municipality=="Region Norrbotten"] = "Norrbottens län"

  # Sparar till CSV om användaren vill det
  if (spara_data == TRUE) write.csv(forskning, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(forskning)
}