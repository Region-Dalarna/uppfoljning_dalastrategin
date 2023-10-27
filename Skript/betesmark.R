#################
### Betesmark ###
#################

#### Lista på variabler och koder 
#### N45926 - Elproduktion totalt inom det geografiska området, MWh
#### N45904 - Elproduktion av vindkraft inom det geografiska området, MWh
#### N45927 - Elproduktion av vattenkraft inom det geografiska området, MWh

#### Dra hem variablerna från Kolada
betesmark <- get_values(
  kpi = c("N00750"),
  municipality = c("0020"),
  period = 2011:2100
)

### Ta bort kolumner som vi inte behöver
betesmark$municipality_id <- NULL
betesmark$gender <- NULL
betesmark$count <- NULL
betesmark$municipality_type <- NULL

### Byter namn från kpi/kolada-koder till mer beskrivande namn
betesmark[betesmark=="N00750"] <- "Total betesmark, hektar"

### Gör datan wide istället för long
#elproduktion <- pivot_wider(elproduktion, names_from=kpi, values_from=value)

#colnames(betesmark) <- c("Kategori", "Ar", "Producerat", "Region")

write.csv(betesmark,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/betesmark.csv", fileEncoding="UTF-8", row.names = FALSE)
