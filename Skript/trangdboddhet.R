pxweb_query_list <- 
  list("Region"=c("20"),
       "Trangboddhet"=c("TRÅNGB N2"),
       "Fodelseregion"=c("200","020","030","040","050","010","100"),
       "Alder"=c("-4","00-04","5-9","5-14","10-14","15-19","15-24","20-24","25-29","25-34","30-34","35-39","35-44","40-44","45-49","45-54","50-54","55-59","55-64","60-64","65-69","65-74","70-74","75-79","75-84","80-84","85-89","85-94","90-94","95-99","95+","100+","totalt"),
       "Kon"=c("TOT2","100","200"),
       "ContentsCode"=c("000004PS"),
       "Tid"=c("*"))

# Download data 
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/LE/LE0105/LE0105B/LE0105Boende02",
            query = pxweb_query_list)

# Convert to data.frame 
trångdboddhet <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

write.csv(trångdboddhet,"Data/trang.csv", fileEncoding="UTF-8", row.names = FALSE)
