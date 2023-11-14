hamta_data_eftergymnasial = function(region = c("0020"),
                                    outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                    filnamn = "eftergym.csv", 
                                    senaste_ar = FALSE, # Om man enbart vill ha senaste år
                                    tid = 2013:2100){ # Välj ett högt värde som sista värde om alla år skall vara med.

 
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)
  
  source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")
  
  if(senaste_ar == TRUE) tid <- max(unique(hamta_kolada_giltiga_ar("N01982",vald_region = region)))
  
  #### Dra hem variablerna från Kolada
  eftergymnasial <- get_values(
    kpi = c("N01982"),
    municipality = region,
    period = tid
  )
  
  eftergymnasial <- eftergymnasial %>% 
    select(-c("count","municipality_type","municipality_id")) %>% 
      mutate(kpi = ifelse(kpi == "N01982","andel_eftergym",kpi))
  
  write.csv(eftergymnasial,paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
}