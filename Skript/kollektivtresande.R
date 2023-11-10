hamta_data_kollektivtresande = function(region = c("0020"),
                                       outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                       filnamn = "resande.csv", 
                                       tid = 2010:2100){ # Välj ett högt värde som sista värde om alla år skall vara med.

  #######################################
  ### Indikator 7 - Kollektiv Resande ###
  #######################################
  
  #### Lista på variabler och koder 
  #### U85427 - Marknadsandel för kollektivtrafik, andel (%)
  #### N60404 - Resor med kollektivtrafik, resor/inv
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)
  
  #### Dra hem variablerna från Kolada
  resande <- get_values(
    kpi = c("U85427", "N60404"),
    municipality = region,
    tid = tid
  )
  
  # Väljer bort variabler och ger mer rimliga namn.
  resande <- resande %>% 
    filter(gender == "T") %>% 
      select(-c(gender,count,municipality_type,municipality_id)) %>% 
        mutate(kpi = case_when(
          kpi == "U85427" ~ "Marknadsandel_procent",
          kpi == "N60404" ~ "Resor_per_invanare"))
  
  ### Skriv ut filen
  write.csv(resande,paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)

}