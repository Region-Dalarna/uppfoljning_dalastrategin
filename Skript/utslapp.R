
hamta_data_utslapp <- function(region = c("0020"),
                               alla_regioner = FALSE,
                               ta_med_riket = TRUE,
                               outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                               filnamn = "vaxthusgaser.csv",
                               kpi = c("N85533", "N85534", "N85538", "N85537", "N85536", "N85532", "N85535", "N07702", "N00401"),
                               senaste_ar = FALSE, # om man enbart vill ha senaste år
                               returnera_data = FALSE,
                               spara_data = TRUE,
                               tid = 1900:2100){# Om flera år, välj ett sent startår så tas sista år med automatiskt."

  # ===========================================================================================================
  # 
  # Skript som hämtar data för utsläpp från Kolada
  # Parametrar som skickas med (= variabler i Kolada-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - ta_med_riket: TRUE om man vill ta med riket också
  # - kpi:  
  # - N07702 - Utsläpp till luft av växthusgaser totalt, ton CO2-ekv 
  # - N00401 - Utsläpp till luft av växthusgaser totalt, ton CO2-ekv/inv 
  # - N85535 - Utsläpp till luft av växthusgaser, arbetsmaskiner, ton CO2e 
  # - N85532 - Utsläpp till luft av växthusgaser, egen uppvärmning, ton CO2e 
  # - N85536 - Utsläpp till luft av växthusgaser, el och fjärrvärme, ton CO2e
  # - N85537 - Utsläpp till luft av växthusgaser, industri, ton CO2e
  # - N85538 - Utsläpp till luft av växthusgaser, jordbruk, ton CO2e
  # - N85533 - Utsläpp till luft av växthusgaser, transporter, ton CO2e
  # - N85534 - Utsläpp till luft av växthusgaser, utrikes transporter, ton CO2e 
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
  
  if(ta_med_riket == TRUE){
    region = c("0000",region)
  }  

  if(senaste_ar == TRUE) tid <- max(unique(hamta_kolada_giltiga_ar("N07702",vald_region = region)))
  
  ### Dra hem variablerna från Kolada
  vaxthusgaser <- get_values(
    kpi = c("N85533", "N85534", "N85538", "N85537", "N85536", "N85532", "N85535", "N07702", "N00401"),
    municipality = region,
    period = tid
  )
  
  # Döper om variabler till mer passande namn (enligt mall ovan)
  vaxthusgaser <- vaxthusgaser %>% 
    select(-c(gender,count,municipality_type)) %>% 
      mutate(kpi = case_when(
        kpi == "N07702" ~ "Totalt Utsläpp",
        kpi == "N85535" ~ "Arbetsmaskiner",
        kpi == "N85532" ~ "Egen uppvärmning",
        kpi == "N85536" ~ "El och fjärrvärme",
        kpi == "N85537" ~ "Industri",
        kpi == "N85538" ~ "Jordbruk",
        kpi == "N85533" ~ "Transporter",
        kpi == "N85534" ~ "Utrikes transporter",
        kpi == "N00401" ~ "Utsläpp per invånare",
      ))
  
  # Region Dalarna skrivs Dalarnas län istället
  vaxthusgaser$municipality[vaxthusgaser$municipality=="Region Dalarna"] <- "Dalarnas län"
  
  # Sparar till Excel om användaren vill det
  if (spara_data == TRUE) write.csv(vaxthusgaser, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(vaxthusgaser)
}
