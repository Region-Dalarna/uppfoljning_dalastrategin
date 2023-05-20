# PXWEB query 
# Thomas tidigare kod. Behåller tillsvidare
# pxweb_query_list <- 
#   list("Region"=c("20"),
#        "Kon"=c("1","2"),
#        "UtbNiv"=c("000"),
#        "BakgrVar"=c("TOT","SE","SAMUTF"),
#        "ContentsCode"=c("0000001Y"),
#        "Tid"=c("*"))
# "ContentsCode"=c("0000001Y"),

pxweb_query_list <- 
  list("Region"=c("20"),
       "Kon"=c("1","2"),
       "UtbNiv"=c("000"),
       "BakgrVar"=c("SE","INTTOT"),
       "ContentsCode"=c("0000001Y"),
       "Tid"=c("*"))
# Download data 
chefspositioner <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1LanKonUtb",
            query = pxweb_query_list)

# Convert to data.frame 
chefspositioner <- as.data.frame(chefspositioner, column.name.type = "text", variable.value.type = "text")

chefspositioner$utbildningsnivå <- NULL

chefspositioner <- chefspositioner %>% 
  filter(!(is.na(`Andel i chefsposition, procent`)))

chefspositioner$bakgrundsvariabel<-ifelse(chefspositioner$bakgrundsvariabel=="födelseregion: Sverige","Inrikes född","Utrikes född")

write.csv(chefspositioner,"Data/chefsrepresentation_ny.csv", fileEncoding="UTF-8", row.names = FALSE)

#chefspositioner <- pivot_wider(chefspositioner, names_from=bakgrundsvariabel, values_from=`Andel i chefsposition, procent`)
