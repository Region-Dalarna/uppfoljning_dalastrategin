
hamta_data_avfall = function(region_Kolada = "0020",
                             region_SCB = "20",
                             outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                             filnamn = c("avfall.csv","avfallbrp.csv"), # Två utdatafiler
                             kpi = c("U07801","U07485", "U07483", "U07484", "U07482", "N01951"),
                             senaste_ar = FALSE, # Om man bara vill ha senaste år
                             tid_kolada = 2013:2100, # Välj ett högt värde som sista värde om alla år skall vara med
                             tid_SCB = c("*")){ # c(*) om alla tillgängliga år skall väljas

  # ===========================================================================================================
  # 
  # Skript som hämtar data för avfall från Kolada och BRP från SCB. Denna används sedan för att beräkna bland annat BRP/capita
  # Parametrar som skickas med (= variabler i Kolada-tabellen) är:
  # - region_Kolada: Vald region. Måste vara samma som region SCB. Max 1 åt gången för tillfället
  # - region_SCB: Måste vara samma som region Kolada. Max 1 åt gången för tillfället
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - ta_med_riket: TRUE om man vill ta med riket också
  # - kpi:  
  # - U07801 - Insamlat hushållsavfall totalt, kg/person
  # - U07485 - Insamlat farligt avfall (inkl. elavfall och batterier), kg/person
  # - U07483 - Insamlat förpackningar och returpapper, kg/person
  # _ U07484 - Insamlat grovavfall, kg/person
  # _ U07482 - Insamlat mat- och restavfall, kg/person
  # - filnamn : Vad skall filen heta
  # - senaste_ar: Sätts till TRUE om man bara vill ha data för senaste år
  # - tid: Vilka år vill man ha? Välj ett högt senaste år om man vill ha alla
  # - returnera_data: TRUE om data skall returneras som en df
  # - spara_data: TRUE om data skall sparas till Excel
  
  # ===========================================================================================================

  api_scb = "https://api.scb.se/OV0104/v1/doris/sv/ssd/NR/NR0105/NR0105A/NR0105ENS2010T01A"
  
  source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")
  
  # Väljer senaste år
  if(senaste_ar == TRUE){
  tid_kolada <- max(unique(hamta_kolada_giltiga_ar("U07801",vald_region = region_Kolada)))
  tid_SCB = max(hamta_giltiga_varden_fran_tabell(api_scb, "tid"))
  }

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 pxweb,
                 readxl)
  
  #### Dra hem variablerna från Kolada
  avfall <- get_values(
    kpi = kpi,
    municipality = region_Kolada,
    period = tid_kolada
  )
  
  # Väljer bort variabler och ger mer rimliga namn.
  avfall <- avfall %>% 
    filter(gender == "T") %>% 
      select(-c(gender,count,municipality_type,municipality_id)) %>% 
        mutate(kpi = case_when(
          kpi == "U07801" ~ "Insamlat kommunalt avfall totalt, kg/invånare (justerat)",
          kpi == "U07485" ~ "Insamlat farligt avfall (inkl. elavfall och batterier), kg/person",
          kpi == "U07483" ~ "Insamlat förpackningar och returpapper, kg/person",
          kpi == "U07484" ~ "Insamlat grovavfall, kg/person",
          kpi == "U07482" ~ "Insamlat mat- och restavfall, kg/person",
          kpi == "N01951" ~ "Invånare totalt, antal"))
  
  ### Gör datan wide istället för long. Detta för att enklare kunna beräkna avfall relaterat till BNO
  avfall <- pivot_wider(avfall, names_from=kpi, values_from=value)
  
  ### BRP-data från SCB för Dalarna. Används för att beräkna avfall/BRP
  ### Först skapar vi en lista på det vi vill ha
  pxweb_query_list <- 
    list("Region" = region_SCB,
         "ContentsCode" = c("NR0105AH"),
         "Tid" = tid_SCB)
  

  ### Sen använder vi den listan för att ta ut det vi vill ha från SCB
  px_data <- 
    pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/NR/NR0105/NR0105A/NR0105ENS2010T01A",
              query = pxweb_query_list)
  
  ### Få den i ett annat format
  brp <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
  
  ### Ta bort kolumn som vi inte behöver
  brp$region <- NULL
  
  ### Döp om kolumner
  colnames(brp) <- c("year","BRP")
  
  ### Slå ihop avfall och BRP-data
  avfall <- merge(avfall, brp, by="year")
  
  ### Räkna ut totalt insamlat avfall
  avfall$insamlat_avfall_totalt <- avfall$`Insamlat kommunalt avfall totalt, kg/invånare (justerat)`*avfall$`Invånare totalt, antal`
  
  ### Räkna ut avfall/BRP
  avfall$avfallbrp <- (avfall$insamlat_avfall_totalt/avfall$BRP)
  
  ### Pivoterar data igen för att få det i long-format
  avfall <- avfall %>% 
    pivot_longer(3:length(names(avfall)))
  
  ### All data behövs inte. Dessutom ett onödigt långt namn
  avfall <- avfall %>% 
    select(year,`Insamlat kommunalt avfall totalt, kg/invånare (justerat)`) %>% 
      rename(Avfall_totalt_invanare = `Insamlat kommunalt avfall totalt, kg/invånare (justerat)`)
  
  avfallbrp <- avfallbrp %>% 
    select(year,avfallbrp)
  
  write.csv(avfall,paste0(outputmapp,filnamn[1]), fileEncoding="UTF-8", row.names = FALSE)
  write.csv(avfallbrp,paste0(outputmapp,filnamn[2]), fileEncoding="UTF-8", row.names = FALSE)
  #write.csv(avfall_region,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/avfall_region.csv", fileEncoding="UTF-8", row.names = FALSE)
}
