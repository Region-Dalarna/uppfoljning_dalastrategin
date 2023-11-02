

hamta_data_utslapp <- function(region = c("0020", "0000"),
                               outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                               filnamn = "vaxthusgaser.csv"){
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)
  
  #### N07702 - Utsläpp till luft av växthusgaser totalt, ton CO2-ekv 
  #### N00401 - Utsläpp till luft av växthusgaser totalt, ton CO2-ekv/inv 
  #### N85535 - Utsläpp till luft av växthusgaser, arbetsmaskiner, ton CO2e 
  #### N85532 - Utsläpp till luft av växthusgaser, egen uppvärmning, ton CO2e 
  #### N85536 - Utsläpp till luft av växthusgaser, el och fjärrvärme, ton CO2e
  #### N85537 - Utsläpp till luft av växthusgaser, industri, ton CO2e
  #### N85538 - Utsläpp till luft av växthusgaser, jordbruk, ton CO2e
  #### N85533 - Utsläpp till luft av växthusgaser, transporter, ton CO2e
  #### N85534 - Utsläpp till luft av växthusgaser, utrikes transporter, ton CO2e 
  
  ### Dra hem variablerna från Kolada
  vaxthusgaser <- get_values(
    kpi = c("N85533", "N85534", "N85538", "N85537", "N85536", "N85532", "N85535", "N07702", "N00401"),
    municipality = region,
    period = 1900:2100
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
  
  # Sparar data till en csv fil
  write.csv(vaxthusgaser,paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
}
