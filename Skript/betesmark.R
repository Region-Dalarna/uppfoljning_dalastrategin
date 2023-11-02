
hamta_data_betesmark = function(region = "0020",
                                cont_code = c("N00750"),
                                outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                filnamn = "betesmark.csv"){

  #################
  ### Betesmark ###
  #################
  
  #### Dra hem variablerna från Kolada
  betesmark <- get_values(
    kpi = cont_code,
    municipality = region,
    period = 2011:2100
  )
  
  # Byter namn på variabel och tar bort de vi inte behöver
  betesmark <- betesmark %>% 
    select(-c(gender,count,municipality_type)) %>%
      mutate(kpi = case_when(
        kpi == "N00750" ~ "Total_betesmark"))
  
  write.csv(betesmark, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
}
