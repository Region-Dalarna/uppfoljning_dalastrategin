diagram_kollektivt_resande <- function(region_vekt = "20",
                                       output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                       #filnamn = "utslapp.xlsx",
                                       returnera_data = FALSE,
                                       diag_andel_per_invanare = TRUE,
                                       diag_marknadsandel = TRUE,
                                       startar_per_invanare = "2010",
                                       ggobjektfilnamn_utan_tid = TRUE,
                                       #spara_data = FALSE,
                                       spara_figur = FALSE){
  
  # ===========================================================================================================
  # Diverse diagram för kollektivt resande. Delvis hård
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
  
  if(diag_andel_per_invanare == TRUE){
    
    kollektivt_resande_df <- hamta_kolada_df(kpi = c("N60404"),region_vekt,valda_ar = c(2000:2100)) %>% 
      mutate(region = skapa_kortnamn_lan(region))
    
    kollektivt_resande_df <- kollektivt_resande_df %>% 
      mutate(variabel_kort = case_when(
        variabelkod == "N60404" ~ "Resor_per_invanare")) %>% 
      select(ar,region,variabel_kort,varde) %>% 
      mutate(region = skapa_kortnamn_lan(region))
    
    if(returnera_data == TRUE){
      assign("kollektivt_resande_df", kollektivt_resande_df, envir = .GlobalEnv)
    }

    diagram_titel <- paste0("Resor per invånare med kollektivtrafik i",vald_region)
    diagramfilnamn <- glue("resor_per_invanare_{vald_region}_ar_{startar_per_invanare}_{last(kollektivt_resande_df$ar)}.png")
    diagram_capt = "Källa: Trafikanalys (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna"
    
    gg_obj <- SkapaStapelDiagram(skickad_df = kollektivt_resande_df %>% 
                                   filter(region == vald_region,
                                          ar >= startar_per_invanare),
                                 skickad_x_var = "ar",
                                 skickad_y_var = "varde",
                                 diagram_titel = diagram_titel,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 diagram_capt = diagram_capt,
                                 #stodlinjer_avrunda_fem = TRUE,
                                 x_axis_lutning = 0,
                                 manual_y_axis_title = "",
                                 manual_color = diagramfarger("rus_sex"),
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
    # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
    if (ggobjektfilnamn_utan_tid) {
      names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfilnamn)
    }
    
  }
  
  if(diag_marknadsandel == TRUE){
    
    # Marknadsandel kollektivtrafik. Data har hämtats manuellt från Kollektivtrafikbarometern (Mats gjorde det)
    resande_marknadsandel <- read.xlsx("G:/skript/projekt/data/uppfoljning_dalastrategin/Data/resande_kollektivbarometern.xlsx")
    
    if(returnera_data == TRUE){
      assign("resande_marknadsandel_df", resande_marknadsandel, envir = .GlobalEnv)
    }
    
    # Enbart ett län över tid för både elbilar och laddhybrider
    
    diagram_titel = paste0("Marknadsandel för kollektivtrafik i Dalarna")
    diagramfilnamn <- glue("marknadsandel_resande_{vald_region}_ar_{first(resande_marknadsandel_df$year)}_{last(resande_marknadsandel_df$year)}.png")
    diagram_capt = "Källa: Kollektivtrafikbarometern\nBearbetning: Samhällsanalys, Region Dalarna\nAndelen resor med Kollektivtrafik (linjelagd buss, spårvagn, tunnelbana, pendeltåg, tåg och båt) och taxi\nav det totala antalet resor med Kollektivtrafik, taxi, bil (förare och passagerare) samt moped/MC."
    
    gg_obj <- SkapaStapelDiagram(skickad_df = resande_marknadsandel %>% 
                                   filter(kpi == "Marknadsandel_procent") %>% 
                                   mutate(year = year %>% as.character()),
                                 skickad_x_var = "year",
                                 skickad_y_var = "value",
                                 diagram_titel = diagram_titel,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 diagram_capt = diagram_capt,
                                 #stodlinjer_avrunda_fem = TRUE,x_axis_lutning = 45,
                                 manual_x_axis_text_hjust = 1,
                                 manual_x_axis_text_vjust = 1,
                                 manual_y_axis_title = "procent",
                                 manual_color = diagramfarger("rus_sex"),
                                 skriv_till_diagramfil = spara_figur,
                                 y_axis_100proc = TRUE)
    
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
