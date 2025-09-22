diag_lag_ek_standard <- function(region = "20", # Enbart ett i taget.
                                 diag_alla_lan = TRUE, # Skapar ett diagram där länen jämförs
                                 diag_tidsserie = TRUE,
                                 fargvektor = diagramfarger("rus_sex"),                               # valbar färgvektor för diagrammet
                                 alder_klartext = "0-19 år",			 #  NA = tas inte med i uttaget,  Finns: "totalt ålder", "0-19 år", "20-64 år", "20-65 år", "65+ år", "66+ år"
                                 tid_koder = "*", # Finns från 2011
                                 cont_klartext = "Låg ekonomisk standard, procent",			 #Finns: "Låg ekonomisk standard, procent", "Hög ekonomisk standard, procent"
                                 visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                 logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                                 diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
                                 output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                 skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                 returnera_data_rmarkdown = FALSE
) {
  
  
  # =======================================================================================================================
  #
  # Två diagram kopplade till låg ekonomisk standard som används i Rus-uppföljningen
  #
  # 
  #
  # =======================================================================================================================
  
  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  # if (demo){
  #   demo_url <- 
  #     c("https://region-dalarna.github.io/utskrivna_diagram/medellivslangd_aterstaende_vid_30 år_alder_Dalarna_ar2012-2016_2019-2023.png")
  #   walk(demo_url, ~browseURL(.x))
  #   if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  #   stop_tyst()
  # }
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         glue,
         readxl)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_ek_standard_regso_region_alder_tid_regos_deso_scb.R")
  
  
  if(diag_alla_lan) {
    region_fokus <- region
    region <- hamtaAllaLan(tamedriket = FALSE)}else {
      region_fokus <- region
    }
  
  fokus_lan <- skapa_kortnamn_lan(hamtaregion_kod_namn(region_fokus)$region) 
  # Före 2022
  lag_ek_standard <- hamta_ek_standard_regso_region_alder_tid_scb (region_vekt = region,
                                                                   alder_klartext = alder_klartext,
                                                                   ej_regso_deso = TRUE,
                                                                   cont_klartext = cont_klartext,
                                                                   tid_koder = tid_koder) %>% 
    mutate(region = skapa_kortnamn_lan(region),
           region = str_remove(region, "^\\S+\\s+")) 
  
  
  if(returnera_data_rmarkdown == TRUE){
    assign("lag_ek_standard_df", lag_ek_standard, envir = .GlobalEnv)
  }
  
  #diag_fargvektor <- if (all(is.na(diag_fargvektor)) & exists("diagramfarger")) diagramfarger("rus_sex") else c("darkred", "yellow", "darkgreen")
  
  gg_list <- list()
  
  if(diag_tidsserie == TRUE){
    
    diagramtitel <- paste0("Andel invånare ",unique(lag_ek_standard$ålder)," med ", tolower(sub("\\s*,.*$", "", last(names(lag_ek_standard)))), " i ", fokus_lan)
    diagramfil <- paste0(tolower(str_replace_all(diagramtitel, "[\\s-]+", "_")),".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = lag_ek_standard %>% 
                                   filter(region == fokus_lan),
                                 skickad_x_var = "år",
                                 skickad_y_var = last(names(lag_ek_standard)),
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 x_axis_lutning = 0,
                                 manual_color = fargvektor,
                                 manual_y_axis_title = "procent",
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  }
  
  if(diag_alla_lan == TRUE){
    diagramtitel <- paste0("Andel invånare ",unique(lag_ek_standard$ålder)," med ", tolower(sub("\\s*,.*$", "", last(names(lag_ek_standard))))," år ", max(lag_ek_standard$år))
    # tar bort årtal
    titel_utan_artal <- sub("(?i)\\s*år\\s+\\d{4}(?:[–-]\\d{4})?\\s*$", "", diagramtitel , perl = TRUE)
    diagramfil <- paste0(tolower(str_replace_all(titel_utan_artal, "[\\s-]+", "_")),".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = lag_ek_standard %>% 
                                   mutate( fokus = ifelse(region == fokus_lan, "1", "0")) %>%
                                   filter(år==max(år)),
                                 skickad_x_var = "region",
                                 skickad_y_var = last(names(lag_ek_standard)),
                                 diagram_titel = diagramtitel,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 x_var_fokus = "fokus",
                                 diagram_capt = diagram_capt,
                                 x_axis_lutning = 45,
                                 manual_color = fargvektor,
                                 manual_y_axis_title = "procent",
                                 x_axis_sort_value = TRUE,
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  }
  
  return(gg_list)
  
}
