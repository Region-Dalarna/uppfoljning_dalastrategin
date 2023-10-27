#######################################################################
### Indikator 9 - Andel hektar formellt skyddad produktiv skogsmark ###
#######################################################################

############################################
### Formellt skyddad produktiv skogsmark ###
############################################
# Länk SCB
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__MI__MI0605/SkyddSkogFrivillig/

pxweb_query_list <- 
  list("Region"=c("20"),
       "Overlapp"=c("UÖA"),
       "TypSkogsmark"=c("PRS"),
       "Former"=c("FSS"),
       "ContentsCode"=c("0000021D", "0000024O"),
       "Tid"=c("*"))

# Download data 
px_data <-  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/MI/MI0605/SkyddSkogFrivillig",
            query = pxweb_query_list)

# Convert to data.frame 
skyddadproduktivskogsmark <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

### Byt namn för att senare kunna slå ihop med andra dataframes
names(skyddadproduktivskogsmark)[names(skyddadproduktivskogsmark) == 'år'] <- 'År'
names(skyddadproduktivskogsmark)[names(skyddadproduktivskogsmark) == 'region'] <- 'Region'

skyddadproduktivskogsmark$Former <- NULL
skyddadproduktivskogsmark$`Överlapp mellan former` <- NULL
skyddadproduktivskogsmark$`Typ av skogsmark` <- NULL

write.csv(skyddadproduktivskogsmark,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/skogsmark.csv", fileEncoding="UTF-8", row.names = FALSE)
