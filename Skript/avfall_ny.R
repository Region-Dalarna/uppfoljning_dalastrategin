###################################################################
### Indikator 4 och 5 - Insamlat hushållsavfall samt avfall/brp ###
###################################################################

#### Lista på variabler och koder 
#### U07801 - Insamlat hush?llsavfall totalt, kg/person
#### U07485 - Insamlat farligt avfall (inkl. elavfall och batterier), kg/person
#### U07483 - Insamlat f?rpackningar och returpapper, kg/person
#### U07484 - Insamlat grovavfall, kg/person
#### U07482 - Insamlat mat- och restavfall, kg/person

source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

#### Dra hem variablerna från Kolada
avfall <- get_values(
  kpi = c("U07801","U07485", "U07483", "U07484", "U07482", "N01951"),
  municipality = c("0020"),
  period = 2013:2100
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

### Gör datan wide istället för long
avfall <- pivot_wider(avfall, names_from=kpi, values_from=value)

### BRP-data från SCB för Dalarna. Används för att beräkna avfall/BRP
### Först skapar vi en lista på det vi vill ha
pxweb_query_list <- 
  list("Region"="20",
       "ContentsCode"=c("NR0105AH"),
       "Tid"=c("*"))

# Stod 2012-2019 tidigare

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
avfallbrp <- merge(avfall, brp, by="year")

### Räkna ut totalt insamlat avfall
avfallbrp$insamlat_avfall_totalt <- avfallbrp$`Insamlat kommunalt avfall totalt, kg/invånare (justerat)`*avfallbrp$`Invånare totalt, antal`

### Räkna ut avfall/BRP
avfallbrp$avfallbrp <- (avfallbrp$insamlat_avfall_totalt/avfallbrp$BRP)

### All data behövs inte. Dessutom ett onödigt långt namn
avfall <- avfall %>% 
  select(year,`Insamlat kommunalt avfall totalt, kg/invånare (justerat)`) %>% 
    rename(Avfall_totalt_invanare = `Insamlat kommunalt avfall totalt, kg/invånare (justerat)`)

avfallbrp <- avfallbrp %>% 
  select(year,avfallbrp)

write.csv(avfall,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/avfall.csv", fileEncoding="UTF-8", row.names = FALSE)
write.csv(avfallbrp,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/avfallbrp.csv", fileEncoding="UTF-8", row.names = FALSE)
write.csv(avfall_region,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/avfall_region.csv", fileEncoding="UTF-8", row.names = FALSE)