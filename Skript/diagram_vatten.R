
diagram_vatten <- function(region_vekt = "20", # Län eller kommunnivå, max 1 åt gången
                           output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                           filnamn = "utslapp.xlsx",
                           diag_tid = TRUE,
                           diag_jmf_senaste_ar = TRUE,
                           returnera_data = FALSE,
                           ggobjektfilnamn_utan_tid = TRUE,
                           #spara_data = FALSE,
                           spara_figur = FALSE){
  
  # ===========================================================================================================
   # Två diagram för andel vattendrag med god ekologisk status. Finns för både län och kommun
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl,
                 glue)
  
  gg_list <- list()
  diagram_capt = "Källa: VISS och Länsstyrelserna (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Klassning görs successivt under en flerårscykel.\nNyckeltalet uppdateras eftersom och alla siffror utom det sista i en cykel bör ses som preliminära."
  
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  
   vald_region = skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region)
  
   if(nchar(region_vekt) == 2){
     
     vatten_df <- hamta_kolada_df(kpi = c("N85052"),hamtaAllaLan(),valda_ar = c(2000:2100)) %>% 
       mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE))
     
     if(returnera_data == TRUE){
       assign("vatten_df", vatten_df, envir = .GlobalEnv)
     }
     
   } else{
     vatten_df <- hamta_kolada_df(kpi = c("N85052"),hamtakommuner(substr(region_vekt,1,2),tamedlan = TRUE,tamedriket = FALSE),valda_ar = c(2000:2100)) %>% 
       mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE))
     
     if(returnera_data == TRUE){
       assign("vatten_kommun_df", vatten_df, envir = .GlobalEnv)
     }
     
   }
  
  # Enbart ett län över tid för både elbilar och laddhybrider
   if(diag_tid == TRUE){
  
    diagram_titel <- paste0("Andelen vattendrag med god ekologisk status i ",vald_region)
    diagramfilnamn <- glue("vattendrag_{vald_region}_ar_{first(vatten_df$ar)}_{last(vatten_df$ar)}.png")

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
    
    # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
    if (ggobjektfilnamn_utan_tid) {
      names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfilnamn)
    }
  
   }
  
  # Jämför kommuner eller län för senaste år
   
  if(diag_jmf_senaste_ar == TRUE){ 
    
    if(nchar(region_vekt) == 2){
      region_fokus = "Sverige"
      diagram_titel <- paste0("Andelen vattendrag med god ekologisk status i Sverige år ",max(vatten_df$ar))
      diagramfilnamn <- glue("vattendrag_Sverige_ar_{last(vatten_df$ar)}.png")
    }else{
      region_fokus = skapa_kortnamn_lan(hamtaregion_kod_namn(substr(region_vekt,1,2))$region)
      diagram_titel <- paste0("Andelen vattendrag med god ekologisk status i ",region_fokus," år ",max(vatten_df$ar))
      diagramfilnamn <- glue("vattendrag_kommun",region_fokus,"_ar_{last(vatten_df$ar)}.png")
    }

    gg_obj <- SkapaStapelDiagram(skickad_df = vatten_df %>% 
                                   filter(ar == max(ar)) %>%
                                   mutate(fokus = ifelse(region == vald_region, "1", ifelse(region == region_fokus, "2", "0"))),
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
                                 manual_color = diagramfarger("rus_tre_fokus"),
                                 x_var_fokus = "fokus",
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
    # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
    if (ggobjektfilnamn_utan_tid) {
      names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfilnamn)
    }
  
  }
  return(gg_list)
  
  # Sparar till Excel om användaren vill det
  #if (spara_data == TRUE) write.csv(skyddadproduktivskogsmark, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  
}
