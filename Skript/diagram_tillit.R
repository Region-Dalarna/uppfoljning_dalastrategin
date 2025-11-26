diagram_tillit <- function(region_vekt = "20", # Riks eller region, ej kommun
                           output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                           #filnamn = "utslapp.xlsx",
                           returnera_data = FALSE,
                           ggobjektfilnamn_utan_tid = TRUE,
                           #spara_data = FALSE,
                           spara_figur = FALSE){
  
  # ===========================================================================================================
  # Ett diagram för tillit över tid för vald region. Enbart på riks eller region-nivå
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)
  
  gg_list <- list()
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_sociala_relationer_region_kon_ar_hlv1socxreg_fohm.R")
  
  deltagande <- funktion_upprepa_forsok_om_fel( function() {
    hamta_sociala_relationer_region_kon_ar_fohm(region_vekt = region_vekt,
                                                sociala_relationer_klartext = c("Lågt socialt deltagande","Svårt att lita på andra"),
                                                andel_och_konfidensintervall_klartext = "Andel")
  }, hoppa_over = hoppa_over_felhantering) %>% 
    rename(Andel_konfidens = `Andel och konfidensintervall`,
           Andel = `Sociala relationer efter region, kön och år`,
           Sociala_relationer = `Sociala relationer`,
           Region = `region`) %>% 
    filter(!is.na(Andel)) %>% 
    mutate(Region = skapa_kortnamn_lan(Region))
  
  if(returnera_data == TRUE){
    assign("deltagande_df", deltagande, envir = .GlobalEnv)
  }
  
  diagram_titel <- paste0("Avsaknad av tillit till andra i ",unique(deltagande%>%.$Region))
  diagramfilnamn <- glue("tillit_{unique(deltagande%>% .$Region)}_ar_{substr(min(deltagande$År),1,4)}_{substr(max(deltagande$År),1,4)}.png")
  diagram_capt = " Källa: Nationella folkhälsoenkäten - Hälsa på lika villkor (Folkhälsomyndigheten)\n Bearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Andel (%) invånare 16-84 år med avsaknad av tillit till andra"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = deltagande %>% 
                                 filter(Kön != "Totalt",
                                        Sociala_relationer == "Svårt att lita på andra"),
                               skickad_x_var = "År",
                               skickad_y_var = "Andel",
                               skickad_x_grupp = "Kön",
                               diagram_titel = diagram_titel,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               manual_x_axis_text_hjust = 1,
                               manual_x_axis_text_vjust = 1,
                               diagram_capt = diagram_capt,
                               stodlinjer_avrunda_fem = TRUE,
                               procent_0_100_10intervaller = TRUE,
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
