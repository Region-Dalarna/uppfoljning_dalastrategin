#### Dra hem variablerna från Kolada
forskning <- get_values(
  kpi = c("N85085", "N85086", "N85087"),
  municipality = c("0001", "0002", "0003", "0004", "0005", "0006", "0007", "0008", "0009",
                   "0010", "0011", "0012", "0013", "0014", "0015", "0016", "0017", "0018", "0019",
                   "0020", "0021", "0022", "0023", "0024", "0025"),
  period = 2011:2100
)

## Ta bort kolumner som inte är relevanta
forskning$gender <- NULL
forskning$count <- NULL
forskning$municipality_type <- NULL
forskning$municipality_id <- NULL

## Döp om variabler till mer begripliga namn
forskning[forskning=="N85085"] <- "Företagens utgifter för egen FoU"
forskning[forskning=="N85086"] <- "Offentliga sektorns utgifter för FoU"
forskning[forskning=="N85087"] <- "Universitets- och högskolesektorns utgifter för FoU"

#forskning <- pivot_wider(forskning, names_from=kpi, values_from=value)

forskning$municipality[forskning$municipality=="Region Stockholm"] = "Stockholms län"
forskning$municipality[forskning$municipality=="Region Uppsala"] = "Uppsala län"             
forskning$municipality[forskning$municipality=="Region Sörmland"] = "Södermanlands län"            
forskning$municipality[forskning$municipality=="Region Östergötland"] = "Östergötlands län"
forskning$municipality[forskning$municipality=="Region Jönköpings län"] = "Jönköpings län"
forskning$municipality[forskning$municipality=="Region Kronoberg"] = "Kronobergs län"
forskning$municipality[forskning$municipality=="Region Kalmar"] = "Kalmar län"
forskning$municipality[forskning$municipality=="Region Gotland"] = "Gotlands län"
forskning$municipality[forskning$municipality=="Region Blekinge"] = "Blekinge län"
forskning$municipality[forskning$municipality=="Region Skåne"] = "Skåne län"
forskning$municipality[forskning$municipality=="Region Halland"] = "Hallands län"
forskning$municipality[forskning$municipality=="Västra Götalandsregionen"] = "Västra Götalands län"
forskning$municipality[forskning$municipality=="Region Värmland"] = "Värmlands län"
forskning$municipality[forskning$municipality=="Region Örebro län"] = "Örebro län"
forskning$municipality[forskning$municipality=="Region Västmanland"] = "Västmanlands län"
forskning$municipality[forskning$municipality=="Region Dalarna"] = "Dalarnas län"
forskning$municipality[forskning$municipality=="Region Gävleborg"] = "Gävleborgs län"
forskning$municipality[forskning$municipality=="Region Västernorrland"] = "Västernorrlands län"
forskning$municipality[forskning$municipality=="Region Jämtland Härjedalen"] = "Jämtlands län"
forskning$municipality[forskning$municipality=="Region Västerbotten"] = "Västerbottens län"
forskning$municipality[forskning$municipality=="Region Norrbotten"] = "Norrbottens län"

#write.csv(forskning,"G:/Uppföljning och Utvärdering/Analys/RUS-Indikatorer/Data/forskning.csv", fileEncoding="UTF-8", row.names = FALSE)

write.csv(forskning,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/forskning.csv", fileEncoding="UTF-8", row.names = FALSE)