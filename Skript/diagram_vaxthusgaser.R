diagram_vaxthusgaser <- function(region_vekt = "20",
                                 output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                 filnamn = "utslapp.xlsx",
                                 returnera_data = FALSE,
                                 #spara_data = FALSE,
                                 spara_figur = FALSE){
  
  # ===========================================================================================================

  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)
  
  gg_list <- list()
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  
  vaxthusgaser_df <- hamta_kolada_df(kpi = c("N85533", "N85534", "N85538", "N85537", "N85536", "N85532", "N85535", "N07702", "N00401"),c(region_vekt,"00"),valda_ar = c(1900:2100))
  
  # Döper om variabler till mer passande namn (enligt mall ovan)
  vaxthusgaser_df <- vaxthusgaser_df %>% 
    select(-kon) %>% 
    mutate(variabel_kort = case_when(
      variabelkod == "N07702" ~ "Totalt Utsläpp",
      variabelkod == "N85535" ~ "Arbetsmaskiner",
      variabelkod == "N85532" ~ "Egen uppvärmning",
      variabelkod == "N85536" ~ "El och fjärrvärme",
      variabelkod == "N85537" ~ "Industri",
      variabelkod == "N85538" ~ "Jordbruk",
      variabelkod == "N85533" ~ "Transporter",
      variabelkod == "N85534" ~ "Utrikes transporter",
      variabelkod == "N00401" ~ "Utsläpp per invånare",
    ))
  
  if(returnera_data == TRUE){
    assign("vaxthusgaser_df", vaxthusgaser_df, envir = .GlobalEnv)
  }
  
  
  diagram_titel <- paste0("Klimatpåverkande utsläpp i ",unique(vaxthusgaser_df %>% filter(region != "Riket") %>% .$region)," per källa")
  diagramfilnamn = paste0("utslapp_bransch_",unique(vaxthusgaser_df %>% filter(region != "Riket") %>% .$region),".png")
  diagram_capt = "Källa:  RKA (Kolada) har beräknat utsläpp med data från den Nationella emissionsdatabasen\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Utsläpp av växthusgaser inom det geografiska området i ton CO2-ekvivalenter."

  gg_obj <- SkapaStapelDiagram(skickad_df = vaxthusgaser_df %>%
                                 filter(regionkod == region_vekt,
                                        ar%in%c(2005,2010,2015:last(ar)),
                                        !(variabel_kort %in% c("Utsläpp per invånare","Totalt Utsläpp"))) %>%
                                 mutate(varde=varde/1000),
                               skickad_x_var = "ar",
                               skickad_y_var = "varde",
                               skickad_x_grupp ="variabel_kort",
                               diagram_titel = diagram_titel,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               diagram_capt = diagram_capt,
                               geom_position_stack = TRUE,
                               legend_vand_ordning = TRUE,
                               stodlinjer_avrunda_fem = TRUE,
                               x_axis_lutning = 0,
                               manual_y_axis_title = "Ton CO2 (tusental)",
                               manual_color = diagramfarger("rus_sex"))
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  diagram_titel <- "Utsläpp per invånare"
  diagramfilnamn = paste0("utslapp_per_capita.png")
  diagram_capt = "Källa: RKA (Kolada) har beräknat utsläpp med data från den Nationella emissionsdatabasen\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Utsläpp av växthusgaser inom det geografiska området i ton CO2-ekvivalenter."
  

  gg_obj <- SkapaStapelDiagram(skickad_df = vaxthusgaser_df %>% 
                                   filter(variabel_kort == "Utsläpp per invånare",
                                          ar%in%c(1990,2000,2005,2010,2015:last(ar))) %>% 
                                   mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE)),
                                 skickad_x_var = "ar",
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "region",
                                 diagram_titel = diagram_titel,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 x_axis_lutning = 0,
                                 manual_y_axis_title = "Ton CO2/invånare",
                                 manual_color = diagramfarger("rus_sex"))
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  return(gg_list)
  
  # Sparar till Excel om användaren vill det
  #if (spara_data == TRUE) write.csv(skyddadproduktivskogsmark, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  
}
