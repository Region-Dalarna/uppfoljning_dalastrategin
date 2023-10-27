####################################
### Indikator - Energikonsumtion ###
####################################

#### Lista på variabler och koder 
#### N45926 - Elproduktion totalt inom det geografiska området, MWh
#### N45904 - Elproduktion av vindkraft inom det geografiska området, MWh
#### N45927 - Elproduktion av vattenkraft inom det geografiska området, MWh

#### Dra hem variablerna från Kolada
elkonsumtion <- get_values(
  kpi = c("N45906", "N01951"),
  municipality = c("0020"),
  period = 2016:2100
)

### Ta bort kolumner som vi inte behöver
elkonsumtion$municipality_id <- NULL
elkonsumtion$count <- NULL
elkonsumtion$municipality_type <- NULL

### Byter namn från kpi/kolada-koder till mer beskrivande namn
elkonsumtion[elkonsumtion=="N45906"] <- "Slutanvändning av el inom det geografiska området, MWh/inv"
elkonsumtion[elkonsumtion=="N01951"] <- "Invånare totalt, antal"

### Gör datan wide istället för long
elkonsumtion <- elkonsumtion %>% filter(gender=="T") %>%  pivot_wider(names_from=kpi, values_from=value) %>% unchop(everything())

elkonsumtion$konsumtion <- elkonsumtion$`Invånare totalt, antal`*elkonsumtion$`Slutanvändning av el inom det geografiska området, MWh/inv`

elkonsumtion$`Invånare totalt, antal` <- NULL
elkonsumtion$`Slutanvändning av el inom det geografiska området, MWh/inv` <- NULL
elkonsumtion$gender <- NULL

colnames(elkonsumtion) <- c("Ar", "Region", "Elkonsumtion (MWh)")

#elproduktion <- read.csv("G:/Uppföljning och Utvärdering/Analys/RUS-Indikatorer/Data/elproduktion.csv",encoding="UTF-8")

elproduktion <- read.csv("G:/skript/projekt/data/uppfoljning_dalastrategin/Data/elproduktion.csv",encoding="UTF-8")

elproduktion <- pivot_wider(elproduktion, names_from=Kategori, values_from=Producerat)

el <- elproduktion %>%  filter(Region=="Region Dalarna") %>% select(Ar, Region, 'Elproduktion totalt inom det geografiska området, MWh')

### Slår ihop konsumtion och produktion
el <- merge(el, elkonsumtion, by=c("Region", "Ar"))

### Räkna ut självförsörjningsgraden av el
el$elgrad <- el$`Elproduktion totalt inom det geografiska området, MWh`/el$`Elkonsumtion (MWh)`

#write.csv(el,"G:/Uppföljning och Utvärdering/Analys/RUS-Indikatorer/Data/el.csv", fileEncoding="UTF-8", row.names = FALSE)
write.csv(el,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/el.csv", fileEncoding="UTF-8", row.names = FALSE)
