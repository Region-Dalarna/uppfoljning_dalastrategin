####################################
### Etableringstid för nyanlända ###
####################################

# PXWEB query 
pxweb_query_list <- 
  list("Region"=c("01","03","04","05","06","07","08","09","10","12","13","14","17","18","19","20","21","22","23","24","25"),
       "Kon"=c("1+2", "1","2"),
       "UtbNiv"=c("000"),
       "BakgrVar"=c("INT010","INT020","INT030","INT040"),
       "ContentsCode"=c("0000001X"),
       "Tid"=c("*"))

# Download data 
etablering <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1LanKonUtb",
            query = pxweb_query_list)

# Convert to data.frame 
etablering <- as.data.frame(etablering, column.name.type = "text", variable.value.type = "text")

etablering<-etablering %>% 
  filter(år%in%c(as.character(2010:max(etablering$år))))

write.csv(etablering,"Data/etablering.csv", fileEncoding="UTF-8", row.names = FALSE)
