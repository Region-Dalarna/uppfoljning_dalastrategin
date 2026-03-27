diagram_service_vard <- function(region_vekt = "20",
                                 output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                 returnera_data = FALSE,
                                 diag_service = TRUE, # Skapar ett diagram för valt län över tid samt jämför samtliga län för senaste år
                                 diag_vard = TRUE, # Jämför kommuner i valt län för senaste år
                                 ggobjektfilnamn_utan_tid = TRUE,
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
  
  if(diag_service == TRUE){
    
    service_df <- hamta_kolada_df(kpi = c("N07530","N07531"),region_vekt,valda_ar = c(2012:2100))
    
    service_df <- service_df %>% 
      filter(!(is.na(varde))) %>%
      select(-kon) %>% 
      mutate(variabel = case_when(
        variabel=="Invånare med tillgång till dagligvarubutik inom 2 km, andel (%)" ~ "Dagligvarubutik inom 2km",
        variabel=="Invånare 0-16 år med tillgång till grundskola inom 2 km, andel (%)" ~ "Grundskola inom 2km"))

    if(returnera_data == TRUE){
      assign("service", service_df, envir = .GlobalEnv)
    }
    
    # Enbart ett län över tid för både elbilar och laddhybrider
    diagram_titel <- paste0("Tillgång till service och tjänster i ",vald_region)
    diagramfilnamn <- glue("service_{vald_region}_ar_{first(service_df$ar)}_{last(service_df$ar)}.png")
    diagram_capt = "Källa:  Tillväxtverket/Pipos (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Invånare med tillgång till dagligvarubutik inom 2 km, andel (%).\nInvånare 0-16 år med tillgång till grundskola inom 2 km, andel (%)."    
    
    gg_obj <- SkapaStapelDiagram(skickad_df = service_df %>% 
                                   filter(ar %in% c(min(ar), max(ar))),
                                 skickad_x_var = "ar",
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "variabel",
                                 diagram_titel = diagram_titel,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 procent_0_100_10intervaller = TRUE,
                                 x_axis_lutning = 0,
                                 manual_y_axis_title = "procent",
                                 manual_color = diagramfarger("rus_sex"),
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")


    
    # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
    if (ggobjektfilnamn_utan_tid) {
      names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfilnamn)
    }

  }
  
  if(diag_vard == TRUE){
    
    vard_df <- hamta_kolada_df(kpi = c("U70449"),region_vekt,valda_ar = c(2016:2100))

    
    if(returnera_data == TRUE){
      assign("vard", vard_df, envir = .GlobalEnv)
    }
    
    # Enbart ett län över tid för både elbilar och laddhybrider
    diagram_titel <- paste0("Tillgång till vård i ",vald_region)
    diagramfilnamn <- glue("vard_{vald_region}_ar_{first(vard_df$ar)}_{last(vard_df$ar)}.png")
    diagram_capt = "Källa: Hälso och sjukvårdsbarometern (via Kolada), bearbetning av Samhällsanalys, Region Dalarna\nDiagramförklaring: Andel invånare som har svarat att de instämmer helt eller delvis\ni påståendet om att de har tillgång till den hälso- och sjukvård som de behöver."    
    
    gg_obj <- SkapaStapelDiagram(skickad_df = vard_df,
                                 skickad_x_var = "ar",
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "kon",
                                 diagram_titel = diagram_titel,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 procent_0_100_10intervaller = TRUE,
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
    
  }
  
  return(gg_list)

}

