hamta_data_sysselsatta = function(region = c("0020"),
                                       outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                       filnamn = "sysselsatt.csv", 
                                       senaste_ar = FALSE, # Om man enbart vill ha senaste år
                                       tid = 2010:2100){ # Välj ett högt värde som sista värde om alla år skall vara med.

  #### Lista på variabler och koder 
  #### N45926 - Elproduktion totalt inom det geografiska området, MWh
  #### N45904 - Elproduktion av vindkraft inom det geografiska området, MWh
  #### N45927 - Elproduktion av vattenkraft inom det geografiska området, MWh
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)
  
  source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")
  
  if(senaste_ar == TRUE) tid <- max(unique(hamta_kolada_giltiga_ar("N00914",vald_region = region)))
  
  #### Dra hem variablerna från Kolada
  sysselsatta <- get_values(
    kpi = c("N00914"),
    municipality = region,
    period = tid
  )
  
  sysselsatta <- sysselsatta %>% 
    select(-c("count","municipality_type","municipality_id")) %>% 
      mutate(kpi = ifelse(kpi == "N00914","sysselsattningsgrad",kpi))
  
  write.csv(sysselsatta,paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
}