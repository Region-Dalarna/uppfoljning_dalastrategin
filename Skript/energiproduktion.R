#######################################
### Indikator 2 -  Energiproduktion ###
#######################################

#### Lista på variabler och koder 
#### N45926 - Elproduktion totalt inom det geografiska området, MWh
#### N45904 - Elproduktion av vindkraft inom det geografiska området, MWh
#### N45927 - Elproduktion av vattenkraft inom det geografiska området, MWh

#### Dra hem variablerna från Kolada
elproduktion <- get_values(
  kpi = c("N45926","N45904", "N45927"),
  municipality = c("0020", "0000"),
  period = 2012:2100
)

### Ta bort kolumner som vi inte behöver
elproduktion$municipality_id <- NULL
elproduktion$gender <- NULL
elproduktion$count <- NULL
elproduktion$municipality_type <- NULL

### Byter namn från kpi/kolada-koder till mer beskrivande namn
elproduktion[elproduktion=="N45926"] <- "Elproduktion totalt inom det geografiska området, MWh"
elproduktion[elproduktion=="N45904"] <- "Elproduktion av vindkraft inom det geografiska området, MWh"
elproduktion[elproduktion=="N45927"] <- "Elproduktion av vattenkraft inom det geografiska området, MWh"

### Gör datan wide istället för long
#elproduktion <- pivot_wider(elproduktion, names_from=kpi, values_from=value)

colnames(elproduktion) <- c("Kategori", "Ar", "Producerat", "Region")

write.csv(elproduktion,"Data/elproduktion.csv", fileEncoding="UTF-8", row.names = FALSE)
