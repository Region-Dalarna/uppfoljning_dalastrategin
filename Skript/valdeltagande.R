#### Dra hem variablerna från Kolada
valdeltagande <- get_values(
  kpi = c("N05401", "N05402", "N05403"),
  municipality = c("0020"),
  period = 1998:2100
)

## Ta bort kolumner som inte är relevanta
valdeltagande$gender <- NULL
valdeltagande$count <- NULL
valdeltagande$municipality_type <- NULL
valdeltagande$municipality_id <- NULL

## Döp om variabler till mer begripliga namn
valdeltagande[valdeltagande=="N05401"] <- "Valdeltagande i senaste kommunalvalet, andel (%)"
valdeltagande[valdeltagande=="N05402"] <- "Valdeltagande i senaste regionvalet, andel (%)"
valdeltagande[valdeltagande=="N05403"] <- "Valdeltagande i senaste riksdagsvalet, andel (%)"

valdeltagande <- pivot_wider(valdeltagande, names_from=kpi, values_from=value)

write.csv(valdeltagande,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/valdeltagande.csv", fileEncoding="UTF-8", row.names = FALSE)