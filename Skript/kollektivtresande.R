#######################################
### Indikator 7 - Kollektiv Resande ###
#######################################

#### Lista på variabler och koder 
#### U85427 - Marknadsandel för kollektivtrafik, andel (%)
#### N60404 - Resor med kollektivtrafik, resor/inv

#### Dra hem variablerna från Kolada
resande <- get_values(
  kpi = c("U85427", "N60404"),
  municipality = c("0020"),
  period = 2010:2100
)

### Ta bort kolumner som vi inte behöver
resande$municipality_id <- NULL
resande$gender <- NULL
resande$count <- NULL
resande$municipality_type <- NULL

### Byter namn från kpi/kolada-koder till mer beskrivande namn
resande[resande=="U85427"] <- "Marknadsandel för kollektivtrafik, andel (%)"
resande[resande=="N60404"] <- "Resor med kollektivtrafik, resor/inv"

### Gör datan wide istället för long
resande <- pivot_wider(resande, names_from=kpi, values_from=value)

names(resande)[names(resande) == 'year'] <- 'År'
names(resande)[names(resande) == 'municipality'] <- 'Region'
resande$Region[resande$Region=="Region Dalarna"] <- "Dalarnas län"

### Skriv ut filen
write.csv(resande,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/resande.csv", fileEncoding="UTF-8", row.names = FALSE)