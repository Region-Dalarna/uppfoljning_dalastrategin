diagram_energieffektivitet <- function(region_vekt = "20",
                                       output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                       #filnamn = "utslapp.xlsx",
                                       returnera_data = FALSE,
                                       diag_tid = TRUE,
                                       diag_jmf_senaste_ar = TRUE,
                                       ggobjektfilnamn_utan_tid = TRUE,
                                       #spara_data = FALSE,
                                       spara_figur = FALSE){
  
  # ===========================================================================================================
  # Tar fram två diagram för energieffektivitet, ett över tid för valt län och ett för alla län för senaste året
  # Enbart på länsnivå då BRP inte finns på lägre nivå
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl,
                 pxweb)
  
  gg_list <- list()
  
  vald_region <- skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  
  pxweb_query_list <-
    list("Region" = hamtaAllaLan(tamedriket = FALSE),
         "ContentsCode" = c("NR0105AH"),
         "Tid" = "*")
  
  ### Sen använder vi listan som vi har skapat för att dra ner datan från SCB
  px_data <- 
    pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/NR/NR0105/NR0105A/NR0105ENS2010T01A",
              query = pxweb_query_list)
  
  ### Lägger datan i en dataframe och ger den ett namn
  brp <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text") %>% 
    rename(Region = region, År = år, BRP = `BRP, löpande priser, mnkr`)

  ########################
  ### Data för utsläpp ###
  ########################
  
  pxweb_query_list <- 
    list("Region" = hamtaAllaLan(tamedriket = FALSE),
         "SNI2007"=c("A01-F43","G45-T98"),
         "AmneMiljo"=c("GHG"),
         "ContentsCode"=c("0000015P"),
         "Tid" = "*")
  
  ### Sen använder vi listan som vi har skapat för att dra ner datan från SCB
  px_data <- 
    pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/MI/MI1301/MI1301B/UtslappLan",
              query = pxweb_query_list)
  
  ### Lägger datan i en dataframe och ger den ett namn
  utslapp <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
  
  ### Slår ihop utsläppen för varje år och region
  utslapp <-  utslapp %>% 
    group_by(region, år) %>% 
      summarise(Ämne = sum(Ämne)) %>% 
        ungroup() %>% 
          rename(Region = region, År = år, Utsläpp = Ämne)

  ### Slår ihop utsläpp och brp
  energieffektivitet <- merge(utslapp, brp, by=c("Region", "År"))
  
  ### Räkna ut energieffektiviteten och pivotera data
  energieffektivitet <- energieffektivitet %>% 
    mutate(effektivitet = BRP/Utsläpp) %>% 
      select(1,2,5) %>% 
        pivot_longer(cols=3,names_to = "variabel",values_to = "value") %>% 
         mutate(Region = skapa_kortnamn_lan(Region,byt_ut_riket_mot_sverige = TRUE))
  
  if(returnera_data == TRUE){
    assign("energieffektivitet_df", energieffektivitet, envir = .GlobalEnv)
  }
  
  ## Dalarna över tid
  
  if(diag_tid == TRUE){
  
    diagram_titel <- paste0("Energieffektivitet i ",vald_region)
    diagramfilnamn <- glue("energieffektivitet_{vald_region}_ar_{first(energieffektivitet$År)}_{last(energieffektivitet$År)}.png")
    diagram_capt = "Källa: SCB\nBearbetning: Samhällsanalys, Region Dalarna"
    
    gg_obj <- SkapaStapelDiagram(skickad_df = energieffektivitet %>% 
                                   filter(Region == vald_region),
                                 skickad_x_var = "År",
                                 skickad_y_var = "value",
                                 diagram_titel = diagram_titel,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 x_axis_lutning = 0,
                                 manual_y_axis_title = "BRP per ton utsläpp av CO2 (tusental)",
                                 manual_color = diagramfarger("rus_sex"),
                                 skriv_till_diagramfil = spara_figur)
  
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
    # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
    if (ggobjektfilnamn_utan_tid) {
      names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfilnamn)
    }
  
  }
  
  # Alla län för senaste år
  if(diag_jmf_senaste_ar == TRUE){
  
    diagram_titel = paste0("Energieffektivitet i Sverige år ", max(energieffektivitet$År))
    diagramfilnamn = paste0("energieffektivitet_alla_lan.png")
    diagramfilnamn <- glue("energieffektivitet_alla_lan_ar_{last(energieffektivitet$År)}.png")
    
    diagram_capt = "Källa: SCB\nBearbetning: Samhällsanalys, Region Dalarna"
    
    gg_obj <- SkapaStapelDiagram(skickad_df = energieffektivitet  %>% 
                                   filter(År == max(År)) %>%
                                   mutate(fokus = ifelse(Region == vald_region, "1", "0" )),
                                 skickad_x_var = "Region",
                                 skickad_y_var = "value",
                                 diagram_titel = diagram_titel,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 diagram_capt = diagram_capt,
                                 x_axis_sort_value = TRUE,
                                 stodlinjer_avrunda_fem = TRUE,
                                 manual_x_axis_text_hjust = 1,
                                 manual_x_axis_text_vjust = 1,
                                 x_axis_lutning = 45,
                                 x_var_fokus="fokus",
                                 manual_y_axis_title = "BRP per ton utsläpp av CO2 (tusental)",
                                 manual_color = diagramfarger("rus_tva_fokus"),
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
