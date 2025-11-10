diagram_skogsmark <- function(region = "20",
                              output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                              filnamn = "skogsmark.xlsx",
                              returnera_data = FALSE,
                              #spara_data = FALSE,
                              spara_figur = FALSE){
  
  # ===========================================================================================================
  # Skapar diagram över skyddad produktiv skogsmark för vald region
  # ===========================================================================================================  
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_skogsmark_region_tid_SkyddSkogFrivillig_scb_rus.R")
  
  gg_list <- list()
  
  skogsmark_df <- hamta_skogsmark_region_former_tid_scb (region = region,
                                                         overlapp_klartext = "Utan överlappande arealer",
                                                         former_klartext = "Formellt skyddad skogsmark",
                                                         typskogsmark_klartext = "Produktiv skogsmark")
  
  # Byter namn för att senare kunna slå ihop med andra dataframes samt tar bort onödiga variabler
  skogsmark_df <- skogsmark_df %>% 
    select(-c(Former,`Överlapp mellan former`,`Typ av skogsmark`)) %>% 
    rename(area_hektar = `Area i hektar`,
           area_procent = `Andel i procent`)
  
  if(returnera_data == TRUE){
    assign("skogsmark_df", skogsmark_df, envir = .GlobalEnv)
  }
  
  skapa_kortnamn_lan(unique(skogsmark_df$region))
  
  diagram_titel <- paste0("Areal skyddad produktiv skogsmark i ",skapa_kortnamn_lan(unique(skogsmark_df$region)))
  diagramfilnamn <- paste0("areal_skyddad_skogsmark_",skapa_kortnamn_lan(unique(skogsmark_df$region)),".png")
  diagram_capt = "Källa: SCB\nBearbetning: Samhällsanalys, Region Dalarna"

  gg_obj <- SkapaStapelDiagram(skickad_df = skogsmark_df %>%
                                 mutate(area_hektar=area_hektar/1000),
                               skickad_x_var = "år",
                               skickad_y_var = "area_hektar",
                               diagram_titel = diagram_titel,
                               diagram_capt = diagram_capt,
                               x_axis_lutning = 0,
                               manual_color= diagramfarger("rus_sex")[1],
                               manual_y_axis_title = "Hektar (tusental)",
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  diagram_titel <- paste0("Andel skyddad produktiv skogsmark i ",skapa_kortnamn_lan(unique(skogsmark_df$region)))
  diagramfilnamn <- paste0("andel_skyddad_skogsmark_",skapa_kortnamn_lan(unique(skogsmark_df$region)),".png")
  diagram_capt = "Källa: SCB\nBearbetning: Samhällsanalys, Region Dalarna"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = skogsmark_df,
                               skickad_x_var = "år",
                               skickad_y_var = "area_procent",
                               diagram_titel = diagram_titel,
                               diagram_capt = diagram_capt,
                               x_axis_lutning = 0,
                               manual_color= diagramfarger("rus_sex")[1],
                               manual_y_axis_title = "procent",
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  return(gg_list)
  
  # Sparar till Excel om användaren vill det
  #if (spara_data == TRUE) write.csv(skyddadproduktivskogsmark, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det

}
