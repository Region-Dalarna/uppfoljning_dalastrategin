hamta_data_langtidsarb = function(region = c("0020"),
                                 outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                 filnamn = "arbetsloshet.csv", 
                                 cond_code = c("N03926"),
                                 senaste_ar = FALSE, # om man enbart vill ha senaste år
                                 tid = 2011:2100){ # Välj ett högt värde som sista värde om alla år skall vara med.

  ############################
  ### Långtidsarbetslöshet ###
  ############################
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)
  
  source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")
  
  if(senaste_ar == TRUE) tid <- max(unique(hamta_kolada_giltiga_ar("N03926",vald_region = region)))
    
  #### Dra hem variablerna från Kolada
  långtidsarbetslöshet <- get_values(
    kpi = cond_code,
    municipality = region,
    period = tid
  )
  
  långtidsarbetslöshet <- långtidsarbetslöshet %>% 
    select(-c("count","municipality_type","municipality_id")) %>% 
      mutate(kpi = ifelse(kpi == "N03926","Långtidsarbetslöshet_25_64",kpi))
  
  # ## Ta bort kolumner som inte är relevanta
  # långtidsarbetslöshet$count <- NULL
  # långtidsarbetslöshet$municipality_type <- NULL
  # långtidsarbetslöshet$municipality_id <- NULL
  # 
  # ## Döp om variabler till mer begripliga namn
  # långtidsarbetslöshet[långtidsarbetslöshet=="N03926"] <- "Långtidsarbetslöshet 25-64 år, årsmedelvärde, andel (%) av bef."
  # 
  # långtidsarbetslöshet <- pivot_wider(långtidsarbetslöshet, names_from=kpi, values_from=value)
  
  write.csv(långtidsarbetslöshet, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)

}