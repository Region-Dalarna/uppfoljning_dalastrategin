### Vi måste ha BRP-data för att kunna räkna
### BRP-data från SCB för Dalarna för åren 2012-2019
### Först skapar vi en lista på det vi vill ha
pxweb_query_list <- 
  list("Region"=c("20"),
       "ContentsCode"=c("NR0105AH"),
       "Tid"=c("*"))

# Stod 2012-2019 tidigare

### Sen använder vi den listan för att ta ut det vi vill ha från SCB
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/NR/NR0105/NR0105A/NR0105ENS2010T01A",
            query = pxweb_query_list)

### Få den i ett annat format
brp <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

### Ta bort kolumn som vi inte behöver
brp$region <- NULL

### Döp om kolumner
colnames(brp) <- c("year","BRP")

### Läs in Avfallsdata
avfall <- read.csv("Data/avfall.csv",encoding="UTF-8")

### Slå ihop avfall och BRP-data
avfallbrp <- merge(avfall, brp, by="year")

### Räkna ut totalt insamlat avfall
avfallbrp$insamlatavfalltotalt <- avfallbrp$Insamlat.kommunalt.avfall.totalt..kg.invånare..justerat.*avfallbrp$Invånare.totalt..antal

### Räkna ut avfall/BRP
avfallbrp$avfallbrp <- (avfallbrp$insamlatavfalltotalt/avfallbrp$BRP)

### Skriv ut filen
write.csv(avfallbrp,"Data/avfallbrp.csv", fileEncoding="UTF-8", row.names = FALSE)
