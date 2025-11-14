diagram_vatten <- function(region_vekt = "20",
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
  
  vald_region = skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region)
  
  vatten_df <- hamta_kolada_df(kpi = c("N85052"),hamtaAllaLan(),valda_ar = c(2000:2100)) %>% 
    mutate(region = skapa_kortnamn_lan(region))

  if(returnera_data == TRUE){
    assign("vatten_df", vatten_df, envir = .GlobalEnv)
  }
  
  # Enbart ett län över tid för både elbilar och laddhybrider
  
  diagram_titel <- paste0("Andelen vattendrag med god ekologisk status i",vald_region)
  diagramfilnamn = paste0("vattendrag_",vald_region,".png")
  diagram_capt = "Källa: VISS och Länsstyrelserna (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Klassning görs successivt under en flerårscykel. Nyckeltalet uppdateras eftersom och alla siffror utom det sista i en cykel bör ses som preliminära."
  
  gg_obj <- SkapaStapelDiagram(skickad_df = vatten_df %>% 
                                 filter(region == vald_region),
                               skickad_x_var = "ar",
                               skickad_y_var = "varde",
                               procent_0_100_10intervaller = TRUE,
                               diagram_titel = diagram_titel,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               diagram_capt = diagram_capt,
                               #stodlinjer_avrunda_fem = TRUE,
                               x_axis_lutning = 0,
                               manual_y_axis_title = "procent",
                               manual_color = diagramfarger("rus_sex"),
                               skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  # Jämför län för senaste år
  diagram_titel <- paste0("Andelen vattendrag med god ekologisk status i Sverige")
  diagramfilnamn = paste0("vattendrag_Sverige")
  diagram_capt = "Källa: VISS och Länsstyrelserna (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Klassning görs successivt under en flerårscykel.\nNyckeltalet uppdateras eftersom och alla siffror utom det sista i en cykel bör ses som preliminära."
  
  gg_obj <- SkapaStapelDiagram(skickad_df = vatten_df %>% 
                                 filter(ar == max(ar)),
                               skickad_x_var = "region",
                               skickad_y_var = "varde",
                               procent_0_100_10intervaller = TRUE,
                               diagram_titel = diagram_titel,
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               x_axis_sort_value = TRUE,
                               diagram_capt = diagram_capt,
                               stodlinjer_avrunda_fem = TRUE,
                               x_axis_lutning = 45,
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
