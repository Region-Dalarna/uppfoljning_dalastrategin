#### Dra hem variablerna från Kolada
tillit <- get_values(
  kpi = c("U01413"),
  municipality = c("0020"),
  period = 2007:2100
)

## Ta bort kolumner som inte är relevanta
tillit$count <- NULL
tillit$municipality_type <- NULL
tillit$municipality_id <- NULL

## Döp om variabler till mer begripliga namn
tillit[tillit=="U01413"] <- "Invånare 16-84 år med avsaknad av tillit till andra, andel (%)"

tillit <- pivot_wider(tillit, names_from=kpi, values_from=value)

write.csv(tillit,"Data/tillit.csv", fileEncoding="UTF-8", row.names = FALSE)