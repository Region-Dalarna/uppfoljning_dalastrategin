diagram_avfall <- function(region_vekt = "20",
                           output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                           filnamn = "utslapp.xlsx",
                           returnera_data = FALSE,
                           #spara_data = FALSE,
                           spara_figur = FALSE){
  
  # ===========================================================================================================
  
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl,
                 pxweb)
  
  gg_list <- list()
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  
  
  avfall_df <- hamta_kolada_df(kpi =  c("U07801","U07485", "U07483", "U07484", "U07482"),region_vekt,valda_ar = c(2013:2100))
  invanare_df <- hamta_kolada_df(kpi =  c("N01951"),region_vekt,valda_ar = c(2013:2100),konsuppdelat = FALSE)
  
  avfall_df <- rbind(avfall_df,invanare_df)
  
  avfall_df <- avfall_df %>% 
    mutate(variabel_kort = case_when(
      variabelkod == "U07801" ~ "Insamlat kommunalt avfall totalt, kg/invånare (justerat)",
      variabelkod == "U07485" ~ "Insamlat farligt avfall (inkl. elavfall och batterier), kg/person",
      variabelkod == "U07483" ~ "Insamlat förpackningar och returpapper, kg/person",
      variabelkod == "U07484" ~ "Insamlat grovavfall, kg/person",
      variabelkod == "U07482" ~ "Insamlat mat- och restavfall, kg/person",
      variabelkod == "N01951" ~ "Invånare totalt, antal")) %>% 
    select(ar,region,variabel_kort,varde)
  
  
  if(returnera_data == TRUE){
    assign("avfall_df", avfall_df, envir = .GlobalEnv)
  }
  ################################
  # Insamlat avfall per invånare 
  ################################
  
  diagram_titel <- paste0("Insamlat avfall i ",unique(avfall_df%>% .$region))
  diagramfilnamn = paste0("avfall_",unique(avfall_df%>% .$region),".png")
  diagram_capt = "Källa: Avfall Sverige (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Antal kilogram kommunalt avfall per justerat invånarantal i Dalarna"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = avfall_df %>% 
                                 filter(variabel_kort %in% c("Insamlat kommunalt avfall totalt, kg/invånare (justerat)")),
                               skickad_x_var = "ar",
                               skickad_y_var = "varde",
                               diagram_titel = diagram_titel,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               diagram_capt = diagram_capt,
                               stodlinjer_avrunda_fem = FALSE,
                               x_axis_lutning = 0,
                               manual_y_axis_title = "Kg per invånare",
                               manual_color = diagramfarger("rus_sex"),
                               skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")

  #######################################
  # Insamlat avfall i relation till BRP 
  ######################################
  
  ### Gör datan wide istället för long. Detta för att enklare kunna beräkna avfall relaterat till BNP
  avfall_df <- pivot_wider(avfall_df, names_from=variabel_kort, values_from=varde) %>% 
    filter(!is.na(`Insamlat kommunalt avfall totalt, kg/invånare (justerat)`))
  
  # Tar bort år om det saknas data. Data för vissa variabler (invånare kommer sannolikt tidigare)
  avfall_df <- avfall_df %>% 
    filter(!is.na(`Insamlat kommunalt avfall totalt, kg/invånare (justerat)`))
  
  ### BRP-data från SCB för Dalarna. Används för att beräkna avfall/BRP
  ### Först skapar vi en lista på det vi vill ha
  pxweb_query_list <- 
    list("Region" = region_vekt,
         "ContentsCode" = c("NR0105AH"),
         "Tid" = "*")
  
  ### Sen använder vi den listan för att ta ut det vi vill ha från SCB
  px_data <- 
    pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/NR/NR0105/NR0105A/NR0105ENS2010T01A",
              query = pxweb_query_list)
  
  ### Få den i ett annat format
  brp <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text") %>% 
    select(-region) %>% 
      rename(ar = år,
             BRP = `BRP, löpande priser, mnkr`)

  ### Slå ihop avfall och BRP-data
  avfallbrp <- merge(avfall_df, brp, by="ar")
  
  ### Räkna ut totalt insamlat avfall
  avfallbrp$insamlat_avfall_totalt <- avfallbrp$`Insamlat kommunalt avfall totalt, kg/invånare (justerat)`*avfallbrp$`Invånare totalt, antal`
  
  ### Räkna ut avfall/BRP
  avfallbrp$avfallbrp <- (avfallbrp$insamlat_avfall_totalt/avfallbrp$BRP)

  ### Väljer ut data som behövs för avfall/brp
  avfallbrp <- avfallbrp %>%
    select(ar,avfallbrp)
  
  if(returnera_data == TRUE){
    assign("avfall_brp_df", avfallbrp, envir = .GlobalEnv)
  }
    
    diagram_titel <- paste0("Avfall/BRP i ",unique(avfall_df%>% .$region))
    diagramfilnamn = paste0("avfall_BRP_",unique(avfall_df%>% .$region),".png")
    diagram_capt = "Källa: Avfall Sverige (via Kolada) och SCB\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Antal kilogram kommunalt avfall dividerat med BRP (miljoner kronor) i Dalarna"
    
    gg_obj <- SkapaStapelDiagram(skickad_df = avfallbrp,
                                 skickad_x_var = "ar",
                                 skickad_y_var = "avfallbrp",
                                 diagram_titel = diagram_titel,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 x_axis_lutning = 0,
                                 manual_y_axis_title = "Avfall (kg)/ BRP (miljoner kronor)",
                                 manual_color = diagramfarger("rus_sex"),
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")

  return(gg_list)
  
  # Sparar till Excel om användaren vill det
  #if (spara_data == TRUE) write.csv(skyddadproduktivskogsmark, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  
}
