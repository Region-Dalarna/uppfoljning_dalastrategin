# Skapa en lista med information som vi vill ha hem -- skapas lättast via pxweb_interactive()
pxweb_query_list <- 
  list("Region"=c("20"),
       "Hushallstyp"=c("SAMTLH"),
       "Boendeform"=c("SMAG","SMBO","SMHY0","FBBO","FBHY0","SPBO","OB","OVR","TOT"),
       "ContentsCode"=c("HE0111DI"),
       "Tid"=c("*"))

# Dra hem datan
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/HE/HE0111/HE0111A/HushallT22",
            query = pxweb_query_list)

# Lägg i en dataframe
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Wide istället för longb
boende <- pivot_wider(px_data_frame, names_from=boendeform, values_from='Antal hushåll')

write.csv(boende,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/boendetyper.csv", fileEncoding="UTF-8", row.names = FALSE)
