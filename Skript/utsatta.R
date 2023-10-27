#### Dra hem variablerna från Kolada
utsatta <- get_values(
  kpi = c("N02904"),
  municipality = c("0001", "0002", "0003", "0004", "0005", "0006", "0007", "0008", "0009",
                   "0010", "0011", "0012", "0013", "0014", "0015", "0016", "0017", "0018", "0019",
                   "0020", "0021", "0022", "0023", "0024", "0025"),
  period = 2007:2100
)

## Ta bort kolumner som inte är relevanta
utsatta$count <- NULL
utsatta$municipality_type <- NULL
#utsatta$municipality_id <- NULL
utsatta$gender <- NULL

## Döp om variabler till mer begripliga namn
utsatta[utsatta=="N02904"] <- "Invånare 0-19 år i ekonomiskt utsatta hushåll, andel (%)"

#utsatta <- pivot_wider(utsatta, names_from=kpi, values_from=value)

write.csv(utsatta,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/utsatthet.csv", fileEncoding="UTF-8", row.names = FALSE)