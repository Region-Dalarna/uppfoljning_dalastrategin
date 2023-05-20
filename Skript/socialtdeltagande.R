#### Dra hem variablerna från Kolada
deltagande <- get_values(
  kpi = c("U60423"),
  municipality = c("0020"),
  period = 2007:2100
)

## Ta bort kolumner som inte är relevanta
deltagande$count <- NULL
deltagande$municipality_type <- NULL
deltagande$municipality_id <- NULL

## Döp om variabler till mer begripliga namn
deltagande[deltagande=="U60423"] <- "Invånare 16-84 år med lågt socialt deltagande, andel (%)"

#deltagande <- pivot_wider(deltagande, names_from=kpi, values_from=value)

write.csv(deltagande,"Data/deltagande.csv", fileEncoding="UTF-8", row.names = FALSE)
