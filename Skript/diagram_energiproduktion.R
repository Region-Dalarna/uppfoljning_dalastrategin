diagram_energiproduktion <- function(region_vekt = "20",
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
                 readxl,
                 glue)
  
  gg_list <- list()
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  
  elproduktion_df <- hamta_kolada_df(kpi = c("N45926","N45904", "N45927","N45952"),region_vekt,valda_ar = c(2012:2100))
  
  # Väljer bort variabler och ger mer rimliga namn.
  elproduktion_df <- elproduktion_df %>% 
    mutate(variabel_kort = case_when(
      variabelkod == "N45926" ~ "Totalt",
      variabelkod == "N45904" ~ "Vindkraft",
      variabelkod == "N45927" ~ "Vattenkraft",
      variabelkod == "N45952" ~ "Solkraft")) %>% 
      select(ar,region,variabel_kort,varde)

  
  elproduktion_df <- pivot_wider(elproduktion_df, names_from=variabel_kort, values_from=varde) %>%
    mutate(Övrigt = Totalt - Vindkraft - Vattenkraft,
           `Vattenkraft och övrigt` = Totalt - Vindkraft) %>%
    pivot_longer(cols=3:8,names_to = "variabel_kort",values_to = "varde") 
  
  if(returnera_data == TRUE){
    assign("elproduktion_df", elproduktion_df, envir = .GlobalEnv)
  }
  
  diagram_titel <- paste0("Elproduktion i ",unique(elproduktion_df%>% .$region))
  diagramfilnamn <- glue("elproduktion_{unique(elproduktion_df%>% .$region)}_ar_{first(elproduktion_df$ar)}_{last(elproduktion_df$ar)}.png")
  diagram_capt = "Källa: Energimyndigheten och SCB (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Elproduktion inom det geografiska områdets gränser, oavsett producent.\nVattenkraft och övrigt har beräknats som skillnaden mellan total elproduktion och produktion från vindkraft."
  
  gg_obj <- SkapaStapelDiagram(skickad_df = elproduktion_df %>% 
                                 filter(variabel_kort%in%c("Vindkraft","Vattenkraft och övrigt")) %>% 
                                  mutate(varde = varde/1000),
                               skickad_x_var = "ar",
                               skickad_y_var = "varde",
                               geom_position_stack = TRUE,
                               skickad_x_grupp = "variabel_kort",
                               diagram_titel = diagram_titel,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               diagram_capt = diagram_capt,
                               stodlinjer_avrunda_fem = TRUE,
                               x_axis_lutning = 0,
                               manual_y_axis_title = "MWh (tusental)",
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
