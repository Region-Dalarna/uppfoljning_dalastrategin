
hamta_data_betesmark = function(region = "0020",
                                cont_code = c("N00750"),
                                outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                filnamn = "betesmark.csv",
                                tid = 2011:2100 ){# Om flera år, välj ett sent startår så tas sista år med automatiskt. Om man enbart vill ha senaste år, skriv "senaste år"

  #################
  ### Betesmark ###
  #################
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)
  
  source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")
  
  if(tid == "senaste år") tid <- max(unique(hamta_kolada_giltiga_ar("N00750",vald_region = region)))
  
  #### Dra hem variablerna från Kolada
  betesmark <- get_values(
    kpi = cont_code,
    municipality = region,
    period = tid)
  
  # Byter namn på variabel och tar bort de vi inte behöver
  betesmark <- betesmark %>% 
    select(-c(gender,count,municipality_type)) %>%
      mutate(kpi = case_when(
        kpi == "N00750" ~ "Total_betesmark"))
  
  write.csv(betesmark, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
}
