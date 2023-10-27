#### Dra hem variablerna från Kolada
service <- get_values(
  kpi = c("N07530", "N07531"),
  municipality = c("0020"),
  period = 2000:2100
)

## Ta bort kolumner som inte är relevanta
service$count <- NULL
service$municipality_type <- NULL
service$municipality_id <- NULL
service$gender <- NULL

## Döp om variabler till mer begripliga namn
service[service=="N07530"] <- "Invånare med tillgång till dagligvarubutik inom 2 km, andel (%)"
service[service=="N07531"] <- "Invånare 0-16 år med tillgång till grundskola inom 2 km, andel (%)"

#service <- pivot_wider(service, names_from=kpi, values_from=value)

write.csv(service,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/service.csv", fileEncoding="UTF-8", row.names = FALSE)
