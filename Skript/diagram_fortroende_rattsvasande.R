diagram_fortroende_rattsvasande <- function(region_vekt = "20",
                                            output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                            #filnamn = "utslapp.xlsx",
                                            returnera_data = FALSE,
                                            #spara_data = FALSE,
                                            ggobjektfilnamn_utan_tid = TRUE,
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
  
  fortroende_df <- hamta_kolada_df(kpi = c(" N07600"),hamtaAllaLan(),valda_ar = c(2000:2100)) %>% 
    mutate(region = skapa_kortnamn_lan(region))
  
  if(returnera_data == TRUE){
    assign("fortroende_df", fortroende_df, envir = .GlobalEnv)
  }
  
  # Enbart ett län över tid för både elbilar och laddhybrider
  
  diagram_titel <- paste0("Andel med mycket stort eller ganska stort förtroende för rättsväsandet i ",vald_region)
  diagramfilnamn <- glue("fortroende_rattsvasande_{vald_region}_ar_{min(fortroende_df$ar)}_{max(fortroende_df$ar)}.png")
  diagram_capt = "Källa: BRÅs nationella trygghetsundersökning\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Andel som har svarat Mycket stort eller Ganska stort på frågan\nOm du tänker dig rättsväsendet som en helhet. Hur stort eller litet förtroende har du för rättsväsendet?"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = fortroende_df %>% 
                                 filter(region == vald_region),
                               skickad_x_var = "ar",
                               skickad_y_var = "varde",
                               skickad_x_grupp = "kon",
                               procent_0_100_10intervaller = TRUE,
                               diagram_titel = diagram_titel,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               diagram_capt = diagram_capt,
                               #stodlinjer_avrunda_fem = TRUE,
                               x_axis_lutning = 0,
                               manual_y_axis_title = "procent",
                               manual_color = diagramfarger("kon"),
                               skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
  if (ggobjektfilnamn_utan_tid) {
    names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfilnamn)
  }
  
  
  # Jämför län för senaste år
  diagram_titel <- paste0("Andel med mycket stort eller ganska stort förtroende för rättsväsandet i Sverige år ",max(fortroende_df$ar))
  diagramfilnamn <- glue("fortroende_rattsvasande__sverige_ar_{max(fortroende_df$ar)}.png")
  diagram_capt = "Källa: BRÅs nationella trygghetsundersökning\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Andel som har svarat Mycket stort eller Ganska stort på frågan\nOm du tänker dig rättsväsendet som en helhet. Hur stort eller litet förtroende har du för rättsväsendet?"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = fortroende_df %>% 
                                 filter(ar == max(ar)),
                               skickad_x_var = "region",
                               skickad_y_var = "varde",
                               skickad_x_grupp = "kon",
                               procent_0_100_10intervaller = TRUE,
                               diagram_titel = diagram_titel,
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               x_axis_sort_value = TRUE,
                               x_axis_sort_grp = 1,
                               vand_sortering = TRUE,
                               diagram_capt = diagram_capt,
                               stodlinjer_avrunda_fem = TRUE,
                               x_axis_lutning = 45,
                               manual_y_axis_title = "procent",
                               manual_color = diagramfarger("kon"),
                               skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
  if (ggobjektfilnamn_utan_tid) {
    names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfilnamn)
  }
  
  return(gg_list)
  
  # Sparar till Excel om användaren vill det
  #if (spara_data == TRUE) write.csv(skyddadproduktivskogsmark, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  
}
