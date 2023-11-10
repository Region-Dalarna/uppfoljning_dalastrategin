hamta_data_gini = function(region = c("0020", "0000"),
                           outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                           filnamn = "gini.csv", 
                           senaste_ar = FALSE, # om man enbart vill ha senaste år
                           tid = 2011:2100){ # Välj ett högt värde som sista värde om alla år skall vara med. "senaste år" ger senaste år

  ##########################
  ### Gini-koefficienten ###
  ##########################
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)
  
  source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")
  
  if(senaste_ar == TRUE) tid <- max(unique(hamta_kolada_giltiga_ar("N00997",vald_region = region)))
  
  #### Dra hem variablerna från Kolada
  gini <- get_values(
    kpi = c("N00997"),
    municipality = region,
    period = tid
  )
  
  # Tar bort kolumner som inte är relevanta och byter till ett bättre namn
  gini <- gini %>% 
    select(-c("gender","count","municipality_type","municipality_id")) %>% 
      mutate(kpi = ifelse(kpi == "N00997","Ginikoefficient",kpi))

  write.csv(gini, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
}