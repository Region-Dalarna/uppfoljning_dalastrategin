diagram_genomstromning_gymnasiet <- function(region_vekt = "20",
                                             output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                             #filnamn = "utslapp.xlsx",
                                             returnera_data = FALSE,
                                             ggobjektfilnamn_utan_tid = TRUE,
                                             #spara_data = FALSE,
                                             spara_figur = FALSE){
  
  # ===========================================================================================================
  
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)
  
  gg_list <- list()
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_gymn_avg_genomstromning_4ar_prg_skolverket.R")
  
  gymnasie <- hamta_gymn_avg_genomstromning_4ar_prg_skolverket(region_vekt = region_vekt,
                                                               huvudman = "Samtliga",
                                                               gymnasieprogram = "Gymnasieskolan totalt") %>% 
    mutate(region = skapa_kortnamn_lan(region))
  
  if(returnera_data == TRUE){
    assign("genomstromning_gymnasiet_df", gymnasie, envir = .GlobalEnv)
  }
  
  diagram_titel <- paste0("Andel i ",unique(gymnasie %>%.$region)," med fullföljd gymnasieutbildning inom fyra år")
  diagramfilnamn <- glue("genomstromning_gymnasiet_{unique(gymnasie %>% .$region)}_ar_{substr(last(gymnasie$läsår),1,4)}_{substr(first(gymnasie$läsår),1,4)}.png")
  diagram_capt = "Källa: Skolverket\nBearbetning: Samhällsanalys, Region Dalarna"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = gymnasie ,
                               skickad_x_var = "läsår",
                               skickad_y_var = "andel",
                               diagram_titel = diagram_titel,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               manual_x_axis_text_hjust = 1,
                               manual_x_axis_text_vjust = 1,
                               diagram_capt = diagram_capt,
                               stodlinjer_avrunda_fem = TRUE,
                               x_axis_lutning = 45,
                               manual_y_axis_title = "procent",
                               manual_x_axis_title = "Läsår då gymnasieutbildningen påbörjades",
                               procent_0_100_10intervaller = TRUE,
                               manual_color = diagramfarger("rus_sex"),
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
