diagram_valdeltagande <- function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                  returnera_data = FALSE,
                                  diag_nationella = TRUE,
                                  diag_EU = TRUE,
                                  spara_figur = FALSE){
  
  # ===========================================================================================================
  
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 readxl)
  
  gg_list <- list()
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  
  if(diag_nationella == TRUE){
  
    source("Skript/valdeltagande_nationellt.R", encoding="UTF-8")
    valdeltagande <- berakna_valdeltagande_nationellt(returnera_data = TRUE)
  
    if(returnera_data == TRUE){
      assign("valdeltagande", valdeltagande, envir = .GlobalEnv)
    }
    
    # Enbart Dalarna över tid uppdelat på kön
    diagram_titel <- "Valdeltagande i Dalarna"
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
    diagramfilnamn <- "valdeltagande.png"
    
    gg_obj <- SkapaStapelDiagram(skickad_df = valdeltagande %>% 
                                   mutate(valår = valår %>% as.character(),
                                          val = factor(val,levels=c("Riksdagsval","Val till regionfullmäktige","Val till kommunfullmäktige"))),
                                 skickad_x_var = "valår",
                                 skickad_y_var = "valdeltagande",
                                 skickad_x_grupp = "val",
                                 diagram_titel = diagram_titel,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 diagram_capt = diagram_capt,
                                 manual_y_axis_title = "procent",
                                 procent_0_100_10intervaller = TRUE,
                                 x_axis_lutning = 0,
                                 manual_color = diagramfarger("rus_sex"),
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  }
  
  if(diag_EU == TRUE){
    
    source("Skript/valdeltagande_EU.R", encoding="UTF-8")
    
    valdeltagande_EU <- berakna_valdeltagande_EU(returnera_data = TRUE)
    
    if(returnera_data == TRUE){
      assign("valdeltagande_EU", valdeltagande_EU, envir = .GlobalEnv)
    }

    diagram_titel <- "Valdeltagande i EU-val i Dalarna"
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
    diagramfilnamn <- "valdeltagande_EU.png"
    
    gg_obj <- SkapaStapelDiagram(skickad_df = valdeltagande_EU %>% 
                                   mutate(valår = valår %>% as.character()),
                                 skickad_x_var = "valår",
                                 skickad_y_var = "valdeltagande",
                                 diagram_titel = diagram_titel,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 diagram_capt = diagram_capt,
                                 manual_color = diagramfarger("rus_sex")[1],
                                 manual_y_axis_title = "procent",
                                 procent_0_100_10intervaller = TRUE,
                                 x_axis_lutning = 0,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  }

  return(gg_list)
  
  # Sparar till Excel om användaren vill det
  #if (spara_data == TRUE) write.csv(skyddadproduktivskogsmark, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  
}

