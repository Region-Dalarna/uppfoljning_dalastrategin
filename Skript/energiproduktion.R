hamta_data_energiproduktion = function(region = c("0020", "0000"),
                                       outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                       filnamn = "elproduktion.csv", 
                                       tid = 2012:2100){ # Välj ett högt värde som sista värde om alla år skall vara med)

  #######################################
  ### Indikator 2 -  Energiproduktion ###
  #######################################
  
  #### Lista på variabler och koder 
  #### N45926 - Elproduktion totalt inom det geografiska området, MWh
  #### N45904 - Elproduktion av vindkraft inom det geografiska området, MWh
  #### N45927 - Elproduktion av vattenkraft inom det geografiska området, MWh
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)
  
  #### Dra hem variablerna från Kolada
  elproduktion <- get_values(
    kpi = c("N45926","N45904", "N45927"),
    municipality = region,
    period = tid
  )
  
  # Väljer bort variabler och ger mer rimliga namn.
  elproduktion <- elproduktion %>% 
    select(-c(gender,count,municipality_type,municipality_id)) %>% 
      mutate(kpi = case_when(
        kpi == "N45926" ~ "Totalt",
        kpi == "N45904" ~ "Vindkraft",
        kpi == "N45927" ~ "Vattenkraft"))
  
  ### Beräknar övrig produktion. Detta är enklare om data först görs om till wide
  elproduktion <- pivot_wider(elproduktion, names_from=kpi, values_from=value) %>% 
    mutate(Övrigt = Totalt - Vindkraft - Vattenkraft) %>% 
      pivot_longer(cols=3:6,names_to = "kpi",values_to = "value")
  
  write.csv(elproduktion,paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
}
