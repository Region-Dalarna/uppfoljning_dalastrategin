diagram_elbilar <- function(region_vekt = "20",
                            output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                            filnamn = "elbilar.xlsx",
                            returnera_data = FALSE,
                            diag_lan = TRUE,
                            diag_kommun = FALSE,
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
  
  if(diag_lan == TRUE){
    
    elbilar_df <- hamta_kolada_df(kpi = c("N07945","N07947"),hamtaAllaLan(),valda_ar = c(2012:2100))
    
    # Väljer bort variabler och ger mer rimliga namn.
    elbilar_df <- elbilar_df %>% 
      mutate(variabel_kort = case_when(
        variabelkod == "N07945" ~ "Elbilar",
        variabelkod == "N07947" ~ "Laddhybrider")) %>% 
      select(ar,region,variabel_kort,varde) %>% 
      mutate(region = skapa_kortnamn_lan(region))
    
    
    if(returnera_data == TRUE){
      assign("elbilar_df", elbilar_df, envir = .GlobalEnv)
    }
    
    # Enbart ett län över tid för både elbilar och laddhybrider
    
    diagram_titel <- paste0("Andelen elbilar och laddhybrider i ",vald_region)
    diagramfilnamn = paste0("elbilar_",vald_region,".png")
    diagram_capt = "Källa: Trafikanalys och SCB (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Andelen personbilar i trafik den 31/12."
    
    gg_obj <- SkapaStapelDiagram(skickad_df = elbilar_df %>% 
                                   filter(region == vald_region),
                                 skickad_x_var = "ar",
                                 skickad_y_var = "varde",
                                 geom_position_stack = TRUE,
                                 skickad_x_grupp = "variabel_kort",
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
    
    # Jämför län för senaste år
    diagram_titel <- paste0("Andelen elbilar och laddhybrider i Sverige år ",max(elbilar_df$ar))
    diagramfilnamn = paste0("elbilar_Sverige.png")
    diagram_capt = "Källa: Trafikanalys och SCB (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Andelen personbilar i trafik den 31/12."
    
    gg_obj <- SkapaStapelDiagram(skickad_df = elbilar_df %>% 
                                   filter(ar == max(ar)),
                                 skickad_x_var = "region",
                                 skickad_y_var = "varde",
                                 geom_position_stack = TRUE,
                                 skickad_x_grupp = "variabel_kort",
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
    
  }
  
  if(diag_kommun == TRUE){
    
    elbilar_kommun_df <- hamta_kolada_df(kpi = c("N07945","N07947"),hamta_kommunkoder(region_vekt)$regionkod,valda_ar = c(2012:2100))
    
    # Väljer bort variabler och ger mer rimliga namn.
    elbilar_kommun_df <- elbilar_kommun_df %>% 
      mutate(variabel_kort = case_when(
        variabelkod == "N07945" ~ "Elbilar",
        variabelkod == "N07947" ~ "Laddhybrider")) %>% 
      select(ar,region,variabel_kort,varde) %>% 
      mutate(region = skapa_kortnamn_lan(region))
    
    
    if(returnera_data == TRUE){
      assign("elbilar_kommun_df", elbilar_kommun_df, envir = .GlobalEnv)
    }
    
    diagram_titel <- paste0("Andelen elbilar och laddhybrider i ",vald_region)
    diagramfilnamn = paste0("elbilar_kommun_",vald_region,".png")
    diagram_capt = "Källa: Trafikanalys och SCB (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Andelen personbilar i trafik den 31/12."
    
    gg_obj <- SkapaStapelDiagram(skickad_df = elbilar_kommun_df %>% 
                                   filter(ar == max(ar)),
                                 skickad_x_var = "region",
                                 skickad_y_var = "varde",
                                 geom_position_stack = TRUE,
                                 skickad_x_grupp = "variabel_kort",
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
    
    
  }
  
  
  return(gg_list)
  
  # Sparar till Excel om användaren vill det
  #if (spara_data == TRUE) write.csv(skyddadproduktivskogsmark, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  
}

