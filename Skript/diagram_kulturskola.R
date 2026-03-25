diagram_kulturskola <- function(region_vekt = "20",
                        output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                        returnera_data = FALSE,
                        ggobjektfilnamn_utan_tid = TRUE,
                        #spara_data = FALSE,
                        spara_figur = FALSE){
  
  # ===========================================================================================================
  
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl,
                 glue)
  
  gg_list <- list()
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  
  vald_region = skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region)
  
  
  
  andel_elever_kulturskola_df <- hamta_kolada_df(kpi = c("N09888"),hamtakommuner(tamedlan = FALSE,tamedriket = FALSE),valda_ar = c(2000:2100))
  
  # Väljer bort variabler och ger mer rimliga namn.
  andel_elever_kulturskola_df <- andel_elever_kulturskola_df %>%
    filter(ar == max(ar)) %>% 
    filter(!is.na(varde)) %>% 
    select(ar,region,kon,variabel,varde) %>% 
    mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE),
           kon = case_when(kon == "Män" ~ "Pojkar",
                           kon == "Kvinnor" ~ "Flickor"))
  
  
  if(returnera_data == TRUE){
    assign("andel_elever_kulturskola_df", andel_elever_kulturskola_df, envir = .GlobalEnv)
  }
  

  # Jämför län för senaste år
  diagram_titel <- paste0("Andel elever (6-19 år) i musik eller kulturskola i ",vald_region)
  diagramfilnamn <- glue("andel_kulturskola_Dalarna_ar_{last(andel_elever_kulturskola_df$ar)}.png")
  diagram_capt = "Källa: SCB och Kulturrådet (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Av sekretesskäl har SCB avrundat till närmaste femtal på de könsuppdelade siffrorna\nför antal unika deltagare i kulturskolan, vilket (eventuellt) medför skillnader mellan summeringen\nav könen och de aggregerade siffrorna, som då inte alltid stämmer överens."
  
  gg_obj <- SkapaStapelDiagram(skickad_df = andel_elever_kulturskola_df,
                               skickad_x_var = "region",
                               skickad_y_var = "varde",
                               skickad_x_grupp = "kon",
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
                               y_axis_100proc = TRUE,
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
  
}

