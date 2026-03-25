diagram_brp_sysselsatt <- function(region_vekt = "20",
                                    output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                    returnera_data = FALSE,
                                    ggobjektfilnamn_utan_tid = TRUE,
                                    #spara_data = FALSE,
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
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_brp_lan_region_tid_NR0105ENS2010T01A_scb.R")
  
  vald_region = skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region)
  
  brp_df <- hamta_brp_lan_region_tid_scb(
    region_vekt = hamtaAllaLan(T),			# Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "RIKS1", "RIKS2", "RIKS3", "RIKS4", "RIKS5", "RIKS6", "RIKS7", "RIKS8", "90"
    cont_klartext = "BRP per sysselsatt, löpande priser, tkr",			 #  Finns: "BRP, löpande priser, mnkr", "BRP, volymutveckling i procent", "BRP per invånare, löpande priser, tkr", "BRP per sysselsatt, löpande priser, tkr", "Medelantal sysselsatta, personer i 1000-tal", "Egentlig lön, löpande priser, mnkr"
    tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"
    long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
    wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
    output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = "brp_lan.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
  ) %>% 
    mutate(region = region %>% skapa_kortnamn_lan(T))


  
    
    # brp_df <- hamta_kolada_df(kpi = c("N03701"),hamtaAllaLan(),valda_ar = c(2000:2100))
    # 
    # # Väljer bort variabler och ger mer rimliga namn.
    # brp_df <- brp_df %>%
    #   select(ar,region,variabel,varde) %>% 
    #   mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE))
    
    
    if(returnera_data == TRUE){
      assign("brp_df", brp_df, envir = .GlobalEnv)
    }
    
    # Enbart ett län över tid för både elbilar och laddhybrider
    diagram_titel <- paste0("BRP per sysselsatt")
    diagramfilnamn <- glue("brp_sysselsatt_tid_{vald_region}_ar_{first(brp_df$år)}_{last(brp_df$år)}.png")
    diagram_capt = "Källa: SCB\nBearbetning: Samhällsanalys, Region Dalarna"
    
    gg_obj <- SkapaLinjeDiagram(skickad_df = brp_df %>% 
                                   filter(region %in% c(vald_region,"Sverige")),
                                 skickad_x_var = "år",
                                 skickad_y_var = last(names(brp_df)),
                                 skickad_x_grupp = "region",
                                 diagram_titel = diagram_titel,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 x_axis_lutning = 45,
                                 manual_y_axis_title = last(names(brp_df)),
                                 manual_color = diagramfarger("rus_sex"),
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
    # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
    if (ggobjektfilnamn_utan_tid) {
      names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfilnamn)
    }
    
    # Jämför län för senaste år
    diagram_titel <- paste0("BRP per sysselsatt år ",max(brp_df$år))
    diagramfilnamn <- glue("brp_sysselsatt_Sverige_ar_{last(brp_df$år)}.png")
    diagram_capt = "Källa: SCB\nBearbetning: Samhällsanalys, Region Dalarna"

    gg_obj <- SkapaStapelDiagram(skickad_df = brp_df %>% 
                                   filter(år == max(år)) %>% 
                                   mutate(fokus = case_when(region == vald_region ~ "1",
                                                            region == "Sverige" ~ "2",
                                                              TRUE ~ "0")),
                                 skickad_x_var = "region",
                                 skickad_y_var = last(names(brp_df)),
                                 geom_position_stack = TRUE,
                                 diagram_titel = diagram_titel,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 output_mapp = output_mapp,
                                 x_var_fokus = "fokus",
                                 filnamn_diagram = diagramfilnamn,
                                 x_axis_sort_value = TRUE,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = FALSE,
                                 x_axis_lutning = 45,
                                 manual_y_axis_title = last(names(brp_df)),
                                 manual_color = diagramfarger("rus_tre_fokus"),
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
    # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
    if (ggobjektfilnamn_utan_tid) {
      names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfilnamn)
    }
    
  

  return(gg_list)

}

