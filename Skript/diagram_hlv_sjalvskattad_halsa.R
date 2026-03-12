diagram_hlv_sjalvskattad_halsa <- function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                          returnera_data = FALSE,
                                          spara_figur = FALSE){
  
  # ===========================================================================================================
  
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 readxl)
  
  gg_list <- list()
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  
  hlv <- readxl::read_excel("G:/skript/projekt/data/uppfoljning_dalastrategin/Data/HLV_självskattad_hälsa_20250313.xlsx") %>% 
    rename(År = "year",
           Kommun = "region",
           Kön = "Kon") %>% 
    select(År,Kommun,Kön,Svar,Andel)
  
  if(returnera_data == TRUE){
    assign("hlv", hlv, envir = .GlobalEnv)
  }
    
  # Enbart Dalarna över tid uppdelat på kön
  diagram_titel <- "Självskattat allmänt hälsotillstånd i Dalarna"
  diagram_capt <- " Källa: Nationella folkhälsoenkäten - Hälsa på lika villkor, bearbetning av Samhällsanalys, Region Dalarna\nDiagramförklaring: Andel av befolkningen 16-84 år som uppger att de har en bra eller mycket bra hälsa"
  diagramfilnamn <- "halsa_dalarna_tid.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = hlv %>% 
                                 filter(Svar=="Bra eller mycket bra hälsa", 
                                        Kommun=="Dalarna",
                                        Kön != "Totalt"),
                               skickad_x_var = "År",
                               skickad_y_var = "Andel",
                               skickad_x_grupp = "Kön",
                               diagram_titel = diagram_titel,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               diagram_capt = diagram_capt,
                               manual_y_axis_title = "procent",
                               procent_0_100_10intervaller = TRUE,
                               x_axis_lutning = 0,
                               lagg_pa_logga = FALSE,
                               manual_color = diagramfarger("kon"),
                               skriv_till_diagramfil = spara_figur)

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  diagram_titel <- paste0("Självskattat allmänt hälsotillstånd i Dalarna ",max(hlv$År))
  diagram_capt = " Källa: Nationella folkhälsoenkäten - Hälsa på lika villkor, bearbetning av Samhällsanalys, Region Dalarna\nDiagramförklaring: Andel av befolkningen 16-84 år som uppger att de har en bra eller mycket bra hälsa"
  diagramfilnamn <- "halsa_senaste_jmf.png"
  
  
  gg_obj <- SkapaStapelDiagram(skickad_df = hlv %>% 
                                 filter(Svar=="Bra eller mycket bra hälsa", 
                                        Kön=="Totalt", 
                                        År==max(hlv$År), 
                                        Kommun!="Dalarna") %>% 
                                 mutate(fokus = ifelse(Kommun == "Riket", "1", "0" ) ),
                               skickad_x_var = "Kommun",
                               skickad_y_var = "Andel",
                               skickad_x_grupp = "Kön",
                               diagram_titel = diagram_titel,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               diagram_capt = diagram_capt,
                               manual_y_axis_title = "procent",
                               procent_0_100_10intervaller = TRUE,
                               x_axis_lutning = 45,
                               manual_x_axis_text_hjust = 1,
                               manual_x_axis_text_vjust = 1,
                               lagg_pa_logga = FALSE,
                               x_var_fokus="fokus",
                               x_axis_sort_value = TRUE,
                               manual_color =diagramfarger("rus_tva_fokus"),
                               skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")

  return(gg_list)
  
  # Sparar till Excel om användaren vill det
  #if (spara_data == TRUE) write.csv(skyddadproduktivskogsmark, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  
}

