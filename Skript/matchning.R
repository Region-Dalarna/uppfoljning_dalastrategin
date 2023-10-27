# Skapa en lista med information som vi vill ha hem -- skapas lättast via pxweb_interactive()
pxweb_query_list <- 
  list("Region"=c("01","03","04","05","06","07","08","09","10","12","13","14","17","18","19","20","21","22","23","24","25"),
       "Kon"=c("1","2","SAMANST"),
       "AlderFodelselandgr"=c("20-64","20-39","Sverige","Norden/EU","Afrika","Asien","Övriga_världen","totalt"),
       "ContentsCode"=c("000005SF"),
       "Tid"=c("*"))

# Download data 
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM9906/AM9906A/RegionInd19M2N",
            query = pxweb_query_list)

# Convert to data.frame 
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

matchning <- pivot_wider(px_data_frame, names_from=`ålder/födelselandgrupp`, values_from=`Matchningsgrad, procent `)

write.csv(matchning,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/matchning.csv", fileEncoding="UTF-8", row.names = FALSE)
