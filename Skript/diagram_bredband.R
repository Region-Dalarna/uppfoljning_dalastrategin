diagram_bredband <- function(region_vekt = "20",
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
  
  bredband_df <- hamta_kolada_df(kpi = "N07918",c(region_vekt,00),valda_ar = c(1900:2100))

  if(returnera_data == TRUE){
    assign("bredband_df", bredband_df, envir = .GlobalEnv)
  }
  
  diagram_titel <- paste0("Tillgång till bredband i ",unique(bredband_df %>% filter(region != "Riket") %>% .$region))
  diagramfilnamn = paste0("tillgang_bredband_",unique(bredband_df %>% filter(region != "Riket") %>% .$region),".png")
  diagram_capt = "Källa:  Post- och telestyrelsen, PTS (via Kolada), bearbetning av Samhällsanalys, Region Dalarna\nDiagramförklaring: Hushåll med tillgång till eller möjlighet att ansluta till bredband om minst 1 Gbit/s, andel (%),\nså kallade homes passed"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = bredband_df %>%
                                 filter(regionkod == region_vekt),
                               skickad_x_var = "ar",
                               skickad_y_var = "varde",
                               diagram_titel = diagram_titel,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               diagram_capt = diagram_capt,
                               stodlinjer_avrunda_fem = TRUE,
                               x_axis_lutning = 0,
                               manual_y_axis_title = "procent",
                               manual_color = diagramfarger("rus_sex"),
                               skriv_till_diagramfil = spara_figur)

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")

  return(gg_list)
  
  # Sparar till Excel om användaren vill det
  #if (spara_data == TRUE) write.csv(skyddadproduktivskogsmark, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  
}
