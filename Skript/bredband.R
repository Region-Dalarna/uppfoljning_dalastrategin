#### Dra hem variablerna från Kolada
bredband <- get_values(
  kpi = c(" N07918"),
  municipality = c("0020"),
  period = 2000:2100
)

### Ta bort kolumner som vi inte behöver
bredband$gender <- NULL
bredband$count <- NULL
bredband$municipality_type <- NULL
bredband$municipality_id <- NULL

### Byter namn från kpi/kolada-koder till mer beskrivande namn
#bredband[bredband=="N07918"] <- "Hushåll med tillgång till eller möjlighet att ansluta till bredband om minst 1 Gbit/s, andel (%)"
### Gör datan wide istället för long
bredband <- pivot_wider(bredband, names_from=kpi, values_from=value)

bredband<-bredband %>% 
  rename("andel_bredband"=N07918)

write.csv(bredband,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/bredband.csv", fileEncoding="UTF-8", row.names = FALSE)
