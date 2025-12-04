diagram_betesmark <- function(region = "20",
                              output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                              tid = 2011:2100,
                              #filnamn = "skogsmark.xlsx",
                              returnera_data = FALSE,
                              ggobjektfilnamn_utan_tid = TRUE,
                              #spara_data = FALSE,
                              spara_figur = FALSE){
  
  # ===========================================================================================================
  # Skapar två diagram över skyddad produktiv skogsmark för vald region
  # ===========================================================================================================
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl,
                 glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_skogsmark_region_tid_SkyddSkogFrivillig_scb_rus.R")
  
  gg_list <- list()

  betesmark_df <- hamta_kolada_df(kpi = c("N00750", "N00751"),region,valda_ar = tid)   

  # Väljer bort variabler och ger mer rimliga namn.
  betesmark_df <- betesmark_df %>% 
    mutate(variabel_kort = case_when(
      variabelkod ==  "N00750" ~ "Total betesmark",
      variabelkod == "N00751" ~ "Slåtteräng")) %>% 
    select(ar,region,variabel_kort,varde) %>% 
    mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE))
  
  betesmark_df <- pivot_wider(betesmark_df, names_from=variabel_kort, values_from=varde) %>% 
    mutate("Övrig betesmark" = `Total betesmark`-`Slåtteräng`)%>% 
    pivot_longer(cols=3:5,names_to = "variabel_kort",values_to = "varde")
  
  if(returnera_data == TRUE){
    assign("betesmark_df", betesmark_df, envir = .GlobalEnv)
  }
    
    diagram_titel <- paste0("Arealen betesmark i ",skapa_kortnamn_lan(unique(betesmark_df$region)))
    diagramfilnamn <- glue("areal_betesmark_{unique(betesmark_df$region)}_ar_{min(betesmark_df$ar)}_{max(betesmark_df$ar)}.png")
    diagram_capt = "Källa: Jordbruksverket (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna"
    
    gg_obj <- SkapaStapelDiagram(skickad_df = betesmark_df %>%
                                   filter(variabel_kort != "Total betesmark"),
                                 skickad_x_var = "ar",
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "variabel_kort",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 x_axis_lutning = 0,
                                 geom_position_stack = TRUE,
                                 stodlinjer_avrunda_fem = TRUE,
                                 manual_color= diagramfarger("rus_sex"),
                                 manual_y_axis_title = "Hektar",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
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
