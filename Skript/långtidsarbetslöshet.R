#### Dra hem variablerna från Kolada
långtidsarbetslöshet <- get_values(
  kpi = c("N03926"),
  municipality = c("0020"),
  period = 2011:2100
)

## Ta bort kolumner som inte är relevanta
långtidsarbetslöshet$count <- NULL
långtidsarbetslöshet$municipality_type <- NULL
långtidsarbetslöshet$municipality_id <- NULL

## Döp om variabler till mer begripliga namn
långtidsarbetslöshet[långtidsarbetslöshet=="N03926"] <- "Långtidsarbetslöshet 25-64 år, årsmedelvärde, andel (%) av bef."

långtidsarbetslöshet <- pivot_wider(långtidsarbetslöshet, names_from=kpi, values_from=value)

write.csv(långtidsarbetslöshet,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/arbetsloshet.csv", fileEncoding="UTF-8", row.names = FALSE)