diagram_sjukdomar_andel <- function(region_vekt = "20",
                                          output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                          returnera_data = FALSE,
                                          kpi = c("U01760","N01452"), # Hjärtinfarkt
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
  
  sjukdomar_andel_df <- hamta_kolada_df(kpi = kpi,hamtaAllaLan(),valda_ar = c(2000:2100))
  
  # Väljer bort variabler och ger mer rimliga namn.
  sjukdomar_andel_df <- sjukdomar_andel_df %>%
    select(ar,region,kon,variabelkod,variabel,varde) %>% 
    mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE))
  
  
  if(returnera_data == TRUE){
    assign("sjukdomar_andel_df", sjukdomar_andel_df, envir = .GlobalEnv)
  }
  
  # Enbart ett län över tid för både elbilar och laddhybrider
  
  skapa_diagram <- function(df,valt_brott){
    
    df <- df %>% filter(variabelkod == valt_brott)
    
    variabel_fil <- tolower(gsub(" +", "_",stringi::stri_trans_general(sub(",.*", "", unique(df$variabel)), "Latin-ASCII")))
    
    diagram_titel <- paste0(sub(",.*", "", unique(df$variabel)) ," år ",max(df$ar))
    diagramfilnamn = paste0(variabel_fil,"_ar_",max(df$ar),".png")

    if(valt_brott == "U01760") diagram_capt = "Källa: Folkhälsomyndigheten, Hälsa på lika villkor (HLV) (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nAndelen räknas genom att ta antal enkätsvarande som beräknats ha ett BMI 25 eller högre dividerat\nmed det totala antalet som besvarat frågan. Extremvärden har exkluderats. 16 år och äldre, för 2020\noch tidigare omfattas 16-84 år, data avser år T-3 till T (flerårsmedelvärden)." 
    if(valt_brott == "N01452") diagram_capt = "Källa: Folkhälsomyndigheten, Hälsa på lika villkor (HLV) (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nAndel personer 16–84 år som uppger psykiskt påfrestning. Data avser år T-3 till T (flerårsmedelvärden)" 
    
    gg_obj <- SkapaStapelDiagram(skickad_df = df %>% 
                                   filter(ar == max(ar)),
                                 skickad_x_var = "region",
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "kon",
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 x_axis_sort_value = TRUE,
                                 diagram_titel = diagram_titel,
                                 output_mapp = output_mapp,
                                 x_axis_sort_grp = 2,
                                 vand_sortering = TRUE,
                                 filnamn_diagram = diagramfilnamn,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 x_axis_lutning = 45,
                                 manual_y_axis_title = "procent",
                                 manual_color = diagramfarger("kon"),
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
    # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
    if (ggobjektfilnamn_utan_tid & valt_brott != "N01452") {
      names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfilnamn)
    }
    
    variabel_fil <- tolower(gsub(" +", "_",stringi::stri_trans_general(sub(",.*", "", unique(df$variabel)), "Latin-ASCII")))

    diagram_titel <- paste0(sub(",.*", "", unique(df$variabel)) ," i ",vald_region)
    diagramfilnamn = paste0(variabel_fil,"_",vald_region,"_ar_",first(df$ar),"_",last(df$ar),".png")

    if(valt_brott == "U01760") df <- df %>% filter(ar %in% c(min(df$ar),max(df$ar),"2010","2015","2020")) # Finns inte för alla år, väljer första, sista och några emellan

    gg_obj <- SkapaStapelDiagram(skickad_df = df %>%
                                   filter(region == vald_region),
                                 skickad_x_var = "ar",
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "kon",
                                 diagram_titel = diagram_titel,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = FALSE,
                                 x_axis_lutning = 0,
                                 manual_y_axis_title = "procent",
                                 manual_color = diagramfarger("kon"),
                                 skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")

    # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
    if (ggobjektfilnamn_utan_tid & valt_brott != "N01452") {
      names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfilnamn)
    }
    
    return(gg_list)
  }
  
  diag <- map(kpi, ~ skapa_diagram(sjukdomar_andel_df, .x)) %>% flatten()
  
  return(diag)
  
  # Sparar till Excel om användaren vill det
  #if (spara_data == TRUE) write.csv(skyddadproduktivskogsmark, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
  
  # Data returneras som en DF om användaren vill det
  
}

