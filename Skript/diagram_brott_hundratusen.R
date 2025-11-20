diagram_brott_hundratusen <- function(region_vekt = "20",
                                      output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                      returnera_data = FALSE,
                                      kpi = c("N07538"), # Ekobrott, finns även andra, se Kolada
                                      #spara_data = FALSE,
                                      ggobjektfilnamn_utan_tid = TRUE,
                                      spara_figur = FALSE){
  
  # ===========================================================================================================
  # Skapar diagram för brott per 100 000 invånare för valda regioner. Går att välja flera typer av brott genom att lägga till 
  # ytterligare kpi från Kolada
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)
  
  gg_list <- list()
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  
  vald_region = skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region)
    
  brott_hundratusen_df <- hamta_kolada_df(kpi = kpi,hamta_kommunkoder(region_vekt)$regionkod,valda_ar = c(2000:2100))
  
  # Väljer bort variabler och ger mer rimliga namn.
  brott_hundratusen_df <- brott_hundratusen_df %>%
    select(ar,region,variabelkod,variabel,varde) %>% 
    mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE))
  
  
  if(returnera_data == TRUE){
    assign("brott_hundratusen_df", brott_hundratusen_df, envir = .GlobalEnv)
  }
  
  # Enbart ett län över tid för både elbilar och laddhybrider
  
  skapa_diagram <- function(df,valt_brott){
    
    df <- df %>% filter(variabelkod == valt_brott)
    
    variabel_fil <- tolower(gsub(" +", "_",stringi::stri_trans_general(sub(",.*", "", unique(df$variabel)), "Latin-ASCII")))
    
    diagram_titel <- paste0(sub(",.*", "", unique(df$variabel)) ," i ",vald_region," år ",max(df$ar))
    diagramfilnamn = paste0(variabel_fil,"_",vald_region,"_ar_",max(df$ar),".png")
    diagram_capt = "Källa: BRÅ och SCB (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna"
    
    gg_obj <- SkapaStapelDiagram(skickad_df = df %>% 
                                   filter(ar == max(ar)),
                                 skickad_x_var = "region",
                                 skickad_y_var = "varde",
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 x_axis_sort_value = TRUE,
                                 diagram_titel = diagram_titel,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 x_axis_lutning = 45,
                                 manual_y_axis_title = "Antal/100 000 inv",
                                 manual_color = diagramfarger("rus_sex"),
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
    # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
    if (ggobjektfilnamn_utan_tid) {
      names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfilnamn)
    }
    
    return(gg_list)
  }
  
  diag <- map(kpi, ~ skapa_diagram(brott_hundratusen_df, .x)) %>% flatten()
  
  return(diag)
  
  # Sparar till Excel om användaren vill det
  #if (spara_data == TRUE) write.csv(skyddadproduktivskogsmark, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  
}

