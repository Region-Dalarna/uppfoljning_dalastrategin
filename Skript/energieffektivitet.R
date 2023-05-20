#####################################################################
################ Indikator 3 - Energieffektivitet ###################
#####################################################################

### Energieffektivitet mäts som förädlingsvärde per ton koldioxekvivalenter från ett produktionsperspektiv.

##################################################################################
######### BRP-data från SCB för Dalarna för åren 2012-2019 #######################
### Använder PX Web API för att skapa en lista på variablerna vi vill ha #########
##################################################################################

### Region = geografierna som ska ingå
### Contentscode = vilken variabel vi vill ha ut
### Tid = åren vi vill ha

pxweb_query_list <- 
  list("Region"=c("00","01","03","04","05","06","07","08","09","10","12","13","14","17","18","19","20","21","22","23","24","25","RIKS1","RIKS2","RIKS3","RIKS4","RIKS5","RIKS6","RIKS7","RIKS8","90"),
       "ContentsCode"=c("NR0105AH"),
       "Tid"=c("*"))

### Sen använder vi listan som vi har skapat för att dra ner datan från SCB
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/NR/NR0105/NR0105A/NR0105ENS2010T01A",
            query = pxweb_query_list)

### Lägger datan i en dataframe och ger den ett namn
brp <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

### Döp om kolumner
colnames(brp) <- c("Region", "År", "BRP")

########################
### Data för utsläpp ###
########################

### Region = geografierna som ska ingå
### Contentscode = vilken variabel vi vill ha ut
### Tid = åren vi vill ha

pxweb_query_list <- 
  list("Region"=c("01","03","04","05","06","07","08","09","10","12","13","14","17","18","19","20","21","22","23","24","25"),
       "SNI2007"=c("A01-F43","G45-T98"),
       "AmneMiljo"=c("GHG"),
       "ContentsCode"=c("0000015P"),
       "Tid"=c("*"))

### Sen använder vi listan som vi har skapat för att dra ner datan från SCB
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/MI/MI1301/MI1301B/UtslappLan",
            query = pxweb_query_list)

### Lägger datan i en dataframe och ger den ett namn
utslapp <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

### Tar bort dataframes som vi inte behöver
px_data <- NULL
pxweb_query_list <- NULL

### Slår ihop utsläppen för varje år och region
utslapp <-  utslapp %>% 
            group_by(region, år) %>% 
            summarise(Ämne = sum(Ämne))

### Byter namn på kolumnerna i utsläpp för att matcha kolumnerna i BRP
colnames(utslapp) <- c("Region", "År", "Utsläpp")

### Slår ihop utsläpp och brp
energieffektivitet <- merge(utslapp, brp, by=c("Region", "År"))

### Räkna ut energieffektiviteten
energieffektivitet$effektivitet <- energieffektivitet$BRP/energieffektivitet$Utsläpp

write.csv(energieffektivitet,"Data/effektivitet.csv", fileEncoding="UTF-8", row.names = FALSE)
