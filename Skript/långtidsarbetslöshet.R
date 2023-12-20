
hamta_data_langtidsarb = function(region = c("00","20"), # Använd med fördel exempelvis funktionen hamtallakommuner
                                  output_mapp = NA, # Outputmapp för data
                                  filnamn = "arbetsloshet.csv", # Filnamn för data. I detta fall .csv
                                  cond_code = c("N03926"), # Se text nedan för alternativ
                                  returnera_data = FALSE, # Skall data returneras som en df
                                  tid = 2011:2100){ # Välj ett högt värde som sista värde om alla år skall vara med."9999" om man vill ha alla år

  # ===========================================================================================================
  # 
  # Skript som hämtar data för långtidsarbetslöshet
  # Parametrar som skickas med (= variabler i Kolada-tabellen) är:
  # - kpi:  
  # - N03926 - Antal invånare 25-64 år (årsmedelvärde år T) som varit öppet arbetslösa eller i program med aktivitetsstöd i minst sex månader, dividerat med antal invånare 25-64 år den 31/12 år T-1.
  # - N03923 - Antal invånare 15-74 år som varit öppet arbetslösa eller i program med aktivitetsstöd i minst sex månader, dividerat med totalt antal invånare 15-74 år som är öppet arbetslösa eller i program med aktivitetsstöd.

  
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/JonFrank81/funktioner/main/func_API_alternativ.R")
  
  region = ifelse(nchar(region) == 2,paste0("00",region),region)

  if("9999" %in% tid) tid <- max(unique(hamta_kolada_giltiga_ar("N03926",vald_region = region)))

  #### Dra hem variablerna från Kolada
  långtidsarbetslöshet <- get_values(
    kpi = cond_code,
    municipality = region,
    period = tid
  )
  
  långtidsarbetslöshet <- långtidsarbetslöshet %>% 
    select(-c("count","municipality_type","municipality_id")) %>% 
      mutate(kpi = ifelse(kpi == "N03926","Långtidsarbetslöshet_25_64",
                          ifelse(kpi == "N03923","Långtidsarbetslöshet_15_74",kpi)),
             municipality = byt_namn_lan_kolada(municipality))

  # Sparar till CSV om användaren vill det
  if (!is.na(output_mapp) & !is.na(filnamn)) write.csv(långtidsarbetslöshet, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(långtidsarbetslöshet)

}
