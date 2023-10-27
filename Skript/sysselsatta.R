#### Dra hem variablerna från Kolada
sysselsatta <- get_values(
  kpi = c("N00914"),
  municipality = c("0020"),
  period = 2010:2100
)

## Ta bort kolumner som inte är relevanta
sysselsatta$count <- NULL
sysselsatta$municipality_type <- NULL
sysselsatta$municipality_id <- NULL

## Döp om variabler till mer begripliga namn
sysselsatta[sysselsatta=="N00914"] <- "Förvärvsarbetande invånare 20-64 år, andel (%)"

sysselsatta <- pivot_wider(sysselsatta, names_from=kpi, values_from=value)

#write.csv(sysselsatta,"G:/Uppföljning och Utvärdering/Analys/RUS-Indikatorer/Data/sysselsatt.csv", fileEncoding="UTF-8", row.names = FALSE)
write.csv(sysselsatta,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/sysselsatt.csv", fileEncoding="UTF-8", row.names = FALSE)
