#### Dra hem variablerna från Kolada
eftergymnasial <- get_values(
  kpi = c("N01982"),
  municipality = c("0020"),
  period = 2013:2100
)

## Ta bort kolumner som inte är relevanta
eftergymnasial$count <- NULL
eftergymnasial$municipality_type <- NULL
eftergymnasial$municipality_id <- NULL

## Döp om variabler till mer begripliga namn
eftergymnasial[eftergymnasial=="N01982"] <- "eftergymnasial 25-64 år, årsmedelvärde, andel (%) av bef."

eftergymnasial <- pivot_wider(eftergymnasial, names_from=kpi, values_from=value)

write.csv(eftergymnasial,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/eftergym.csv", fileEncoding="UTF-8", row.names = FALSE)