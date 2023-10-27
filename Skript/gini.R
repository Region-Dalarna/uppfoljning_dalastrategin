#### Dra hem variablerna från Kolada
gini <- get_values(
  kpi = c("N00997"),
  municipality = c("0020", "0000"),
  period = 2011:2100
)

## Ta bort kolumner som inte är relevanta
gini$gender <- NULL
gini$count <- NULL
gini$municipality_type <- NULL
gini$municipality_id <- NULL

## Döp om variabler till mer begripliga namn
gini[gini=="N00997"] <- "Ginikoefficient – hushållens disponibla inkomster, index"

gini <- pivot_wider(gini, names_from=kpi, values_from=value)

write.csv(gini,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/gini.csv", fileEncoding="UTF-8", row.names = FALSE)
