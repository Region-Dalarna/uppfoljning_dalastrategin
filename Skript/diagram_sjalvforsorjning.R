diagram_sjalvforsorjning <- function(region_vekt = "20",
                                     output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                     diag_livsmedel = TRUE,
                                     diag_el = TRUE,
                                     #filnamn = "skogsmark.xlsx",
                                     returnera_data = FALSE,
                                     ggobjektfilnamn_utan_tid = TRUE,
                                     #spara_data = FALSE,
                                     spara_figur = FALSE){
  
  # ===========================================================================================================
  # 
  # ===========================================================================================================
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 readxl,
                 rKolada,
                 glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  
  
  if(diag_livsmedel == TRUE){

    source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_djur_lan_sjv.R")
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_konsumtion_sjv.R")
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_marknadsbalans_sjv.R")
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_mejeri_sjv.R")
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_skordar_lan_sjv.R")
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_slakt_sjv.R")
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_folkmangd_alder_kon_ar_scb.R")
    
    gg_list <- list()
    
    # ================================================== köttproduktion ==================================================
    
    slakt_df <- hamta_slakt_sjv(djurslag_klartext = c("Summa nötkreatur", "Summa gris", "Häst","Summa får")) %>% 
      mutate(Djurslag = case_when(Djurslag %in% "Summa nötkreatur" ~ "Nötkött",
                                  Djurslag %in% "Summa gris" ~ "Griskött",
                                  Djurslag %in% "Summa får" ~ "Fårkött",
                                  Djurslag %in% "Häst" ~ "Hästkött",
                                  TRUE ~ Djurslag),
             kvant_ton = `Kvantitet, 1 000 ton` * 1000) %>% 
      filter(!is.na(kvant_ton))
    
    # ================== invånare i Dalarna =============================
    
    bef_dalarna_df <- hamta_bef_folkmangd_alder_kon_ar_scb(region_vekt = "20",
                                                           kon_klartext = NA)
    
    # ================================================== mjölkproduktion ==================================================
    
    mjolk_df <- hamta_mejeri_sjv()
    mjolkkor_df <- hamta_djur_lan_jbv()
    
    mjolk_prod_dalarna <- mjolk_df %>% 
      left_join(mjolkkor_df %>% filter(Län == "00 Riket"), by = c("År")) %>% 
      mutate(Prod_volym = Mejeriproduktion / `Antal djur`) %>% 
      left_join(mjolkkor_df %>% filter(Län != "00 Riket"), by = c("År")) %>% 
      mutate(mjolk_dalarna = Prod_volym * `Antal djur.y` * 1000,
             enhet = "ton",
             kvant_ton = mjolk_dalarna,
             Vara = "Mjölkprodukter") %>% 
      select(År, Län = Län.y, Vara, enhet, mjolk_dalarna, kvant_ton)
    
    mjolkekvivalenter_df <- hamta_marknadsbalans_sjv(produkt = "mejeriprodukter", flik_sokord = "ekvivalenter") %>% 
      select(År, Mjölkekvivalenter = "Totalkonsumtion kg/capita")
    
    mjolk_ekv <- mjolkekvivalenter_df %>% 
      left_join(bef_dalarna_df, by = c("År" = "år")) %>% 
      filter(!is.na(Folkmängd)) %>%
      mutate(Mjölkekvivalenter = Mjölkekvivalenter %>% as.numeric(), 
             Mjölkprodukter = (Mjölkekvivalenter * Folkmängd),
             kvant_ton = Mjölkprodukter / 1000,
             Vara = "Mjölkprodukter")
    
    # ================================================== spannmål och potatis ==================================================
    
    skordar_df <- hamta_skordar_lan_sjv(år_klartext = c("2010":max(mjolk_df$År)))
    
    spannmal_df <- skordar_df %>% 
      filter(Gröda %in% c("Höstvete", "Vårvete", "Vårkorn", "Havre"),
             !is.na(`Totalskörd, ton`)) %>% 
      group_by(År, Län, Tabelluppgift) %>% 
      summarise(kvant_ton = sum(`Totalskörd, ton`), .groups = "drop") %>%
      mutate(enhet = "ton")
    
    potatis_df <- skordar_df %>%
      filter(Gröda == "Matpotatis", !is.na(`Totalskörd, ton`)) %>% 
      rename(kvant_ton = `Totalskörd, ton`)
    
    # ================================================== totalkonsumtion =====================================================
    
    tot_konsumtion_df <- hamta_konsumtion_sjv(vara_klartext = c("Konsumtionsmjölk, liter", "Nötkött inklusive kalvkött, vara med ben",
                                                                "Fårkött, vara med ben", "Hästkött, vara med ben", "Griskött, vara med ben",
                                                                "Renkött, vara med ben", "Fjäderfäkött, urtagen vara", "Kött av vilt", "Inälvor",
                                                                "Ost (inkl. margarinost)", "Ägg", "Smör", "SUMMA MJÖL OCH GRYN", "Matpotatis",
                                                                "Köksväxter, färska och frysta", "Köksväxter  beredda", "Frukter och bär, färska och frysta",
                                                                "Frukter och bär, beredda"),
                                              variabel_klartext = "Kilo eller liter per person och år") 
    
    konsumtion_produkter_df <- tot_konsumtion_df %>% 
      mutate(Vara = case_when(Vara %in% c("Köksväxter, färska och frysta", "Köksväxter  beredda") ~ "Köksväxter",
                              Vara %in% c("Frukter och bär, färska och frysta","Frukter och bär, beredda") ~ "Frukter och bär",
                              Vara %in% "Nötkött inklusive kalvkött, vara med ben" ~ "Nötkött",
                              Vara %in% "Fårkött, vara med ben" ~ "Fårkött",
                              Vara %in% "Hästkött, vara med ben" ~ "Hästkött",
                              Vara %in% "Griskött, vara med ben" ~ "Griskött",
                              Vara %in% "Renkött, vara med ben" ~ "Renkött",
                              Vara %in% "Fjäderfäkött, urtagen vara" ~ "Fjäderfäkött",
                              Vara %in% "Kött av vilt" ~ "Viltkött",
                              Vara %in% "Ost (inkl. margarinost)" ~ "Ost",
                              Vara %in% "SUMMA MJÖL OCH GRYN" ~ "Mjöl och gryn",
                              Vara %in% "Konsumtionsmjölk, liter" ~ "Konsumtionsmjölk",
                              TRUE ~ Vara),
             År = ifelse(str_detect(År, "prel"), År %>% str_remove(" prel."), År)) %>% 
      group_by(År, Vara) %>%
      summarise(kg_liter_per_capita = sum(`Kilo eller liter per person och år`, na.rm = TRUE), .groups = "drop")
    
    # ================== estimerad konsumtion i Dalarna =============================
    
    konsumtion_dalarna_df <- konsumtion_produkter_df %>% 
      bind_rows(mjolk_ekv %>% select(År, Vara, tot_kons_ton = kvant_ton)) %>%
      left_join(bef_dalarna_df, by = c("År" = "år")) %>% 
      mutate(kg_liter = kg_liter_per_capita * Folkmängd,
             tot_kons_ton = ifelse(is.na(tot_kons_ton), kg_liter / 1000, tot_kons_ton)) %>% 
      select(År, Vara, kg_liter, tot_kons_ton) %>% 
      filter(!is.na(tot_kons_ton))
    
    
    # =============================================== marknadsandel ==================================================
    
    
    marknadsandel_df <- konsumtion_dalarna_df %>% 
      left_join(slakt_df %>% select(År, Djurslag, kvant_ton), by = c("År", "Vara" = "Djurslag")) %>% 
      left_join(potatis_df %>% select(År, Gröda, kvant_ton), by = c("År", "Vara" = "Gröda")) %>%
      left_join(mjolk_prod_dalarna %>% select(År, Vara, kvant_ton), by = c("År", "Vara")) %>% 
      mutate(ny = coalesce(!!!select(., starts_with("kvant_ton")))) %>% 
      select(-starts_with("kvant_ton")) %>% 
      rename(kvant_ton = ny) %>% 
      filter(!is.na(kvant_ton)) %>% 
      mutate(marknadsandel = round((kvant_ton / tot_kons_ton * 100), 2)) 
    
    # ================== antal unika varor =============================
    antal_unika_varor <- length(unique(marknadsandel_df$Vara))
    
    marknadsandel_df <- marknadsandel_df %>% 
      group_by(År) %>%                                                    # behåll endast de år som det finns värden för samliga varor
      filter(n_distinct(Vara) == antal_unika_varor) %>%
      ungroup()
    
    marknadsandel_ut_df <- marknadsandel_df %>% 
      mutate(Andel = marknadsandel) %>% 
      select(Produkt = Vara, År, Andel)
    
    if(returnera_data == TRUE){
      assign("sjalvforsorjning_livsmedel_df", marknadsandel_ut_df, envir = .GlobalEnv)
    }
      
      diagram_titel <- "Teoretisk självförsörjning av livsmedel i Dalarna"
      diagramfilnamn <- glue("teoretisk_sjalvforsorjning_livsmedel_dalarna_ar_{first(marknadsandel_ut_df$År)}_{last(marknadsandel_ut_df$År)}.png")
      diagram_capt = "Källa: Jordbruksverket\nBearbetning: Samhällsanalys, Region Dalarna"
      
    gg_obj <- SkapaStapelDiagram(skickad_df = marknadsandel_ut_df,
                                 skickad_x_var = "Produkt",
                                 skickad_y_var = "Andel",
                                 skickad_x_grupp = "År",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 manual_x_axis_text_hjust = 1,
                                 manual_x_axis_text_vjust = 1,
                                 legend_byrow = TRUE,
                                 manual_color = diagramfarger("rus_gradient"),
                                 x_axis_sort_value = TRUE,
                                 manual_y_axis_title = "procent",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
    # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
    if (ggobjektfilnamn_utan_tid) {
      names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfilnamn)
    }
  
  }
  
  
  if(diag_el == TRUE){
    
    elproduktion_df <- hamta_kolada_df(kpi = c("N45926"),region_vekt,valda_ar = c(2012:2100))
    
    # Väljer bort variabler och ger mer rimliga namn.
    elproduktion_df <- elproduktion_df %>% 
      mutate(variabel_kort = case_when(
        variabelkod == "N45926" ~ "Totalt")) %>% 
      select(ar,region,variabel_kort,varde)
    
    # Av oklar anledning hämtas inte båda variablerna i en och samma funktionanrop, så två anrop krävs.
    elkonsumtion_df <- hamta_kolada_df(kpi = c("N45906"),region_vekt,valda_ar = c(2016:2100))
    invanare_df <- hamta_kolada_df(kpi = c("N01951"),konsuppdelat = FALSE,region_vekt,valda_ar = c(2016:2100))
    
    # Left join elkonsumtion och invånare
    elkonsumtion_df <- rbind(elkonsumtion_df,invanare_df %>% filter(ar %in% unique(elkonsumtion_df$ar))) %>% 
      select(-variabelkod)
    
    # Beräknar total konsumtion av el. Detta är enklare om data först görs om till wide
    elkonsumtion_df <- pivot_wider(elkonsumtion_df, names_from = variabel, values_from = varde) %>% 
      mutate(konsumtion = `Invånare totalt, antal`*`Slutanvändning av el inom det geografiska området, MWh/inv`) %>%
      select(-c(`Invånare totalt, antal`,`Slutanvändning av el inom det geografiska området, MWh/inv`,kon)) 
    
    # Binder ihop elproduktion och elkonsumtion för att beräkna självförsörjningsgrad
    sjalvforsorjning_el_df <- elproduktion_df %>% 
      filter(ar %in% unique(elkonsumtion_df$ar) ) %>% 
      left_join(elkonsumtion_df, by = c("ar","region")) %>% 
      mutate(sjalvforsorjning = round((varde / konsumtion)*100,2)) %>% 
      select(ar,region,sjalvforsorjning)
    
    if(returnera_data == TRUE){
      assign("sjalvforsorjning_el_df", sjalvforsorjning_el_df, envir = .GlobalEnv)
    }
    
    diagram_titel <- paste0("Teoretisk självförsörjning av el i ",unique(sjalvforsorjning_el_df$region))
    diagramfilnamn <- glue("teoretisk_sjalvforsorjning_el_{unique(sjalvforsorjning_el_df$region)}_ar_{first(sjalvforsorjning_el_df$ar)}_{last(sjalvforsorjning_el_df$ar)}.png")
    diagram_capt = "Källa: Kolada\nBearbetning: Samhällsanalys, Region Dalarna"
    
    gg_obj <- SkapaStapelDiagram(skickad_df = sjalvforsorjning_el_df,
                                 skickad_x_var = "ar",
                                 skickad_y_var = "sjalvforsorjning",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 manual_color = diagramfarger("rus_sex"),
                                 x_axis_lutning = 0,
                                 manual_y_axis_title = "procent",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
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
