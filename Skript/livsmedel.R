hamta_data_livsmedel = function(outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                filnamn = "livsmedel.csv"){  

  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse)
  
  # Text nedan gäller inte längre. Sedan 2024 hämtas data från jordbruksverket via skript som Peter skrivit
  #
  # Källa för data: https://jordbruksverket.se/mat-och-drycker/livsmedelsstrategi-for-sverige/statistik-om-livsmedelskedjan 
  # För tillfället är denna data dock inte uppdaterad, varför jag använder en fil (regionalstatistik_2023) som jag fått mailad från Jordbruksverket. 
  # Detta mail fick jag under våren 2023 och under våren 2024 mailar jag dom på nytt för att undersöka om det finns uppdaterad data. Sparas i så fall som regionalstatistik_2024

  # ======================= gammal metod =======================
  ### Marknadsandel
  livsmedel <- read_excel("G:/skript/projekt/data/uppfoljning_dalastrategin/Data/jordbruksverket/regionalstatistik_2023.xlsx",
                          col_names = FALSE, sheet=25, skip=42)
  colnames(livsmedel) <- c("Produkt",2016:2020)
  livsmedel <- tidyr::gather(livsmedel, År, Andel, as.character(2016:2020), factor_key=TRUE)
  
  # ======================= ny metod =======================
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_djur_lan_sjv.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_konsumtion_sjv.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_marknadsbalans_sjv.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_mejeri_sjv.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_skordar_lan_sjv.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_slakt_sjv.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_folkmangd_alder_kon_ar_scb.R")
  
  
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
  
  skordar_df <- hamta_skordar_lan_sjv()
  
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
  
  export_csv_fil_df <- marknadsandel_df %>% 
    mutate(Andel = marknadsandel / 100) %>% 
    select(Produkt = Vara, År, Andel)
  
  write.csv(export_csv_fil_df,paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)
}