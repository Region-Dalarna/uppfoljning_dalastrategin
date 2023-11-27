hamta_data_energieffektivitet = function(region = "20", 
                                         alla_regioner = TRUE, # True om man vill ha alla regioner
                                         ta_med_riket = FALSE, 
                                         trang = c("TRÅNGB N2"), # c("TOT"),c("ej trångbodda enligt norm 2"),c("US"),
                                         fodelseregion = c("*"), # "200","020","030","040","050","010","100" Använd pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/LE/LE0105/LE0105B/LE0105Boende02", "Fodelseregion") för förklaring
                                         Alder = c("*"), # "-4","00-04","5-9","5-14","10-14","15-19","15-24","20-24","25-29","25-34","30-34","35-39","35-44","40-44","45-49","45-54","50-54","55-59","55-64","60-64","65-69","65-74","70-74","75-79","75-84","80-84","85-89","85-94","90-94","95-99","95+","100+","totalt"
                                         Kon = c("*"), # "TOT2" - totalt,"100" - kvinnor ,"200" - män
                                         outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                         filnamn = "effektivitet.csv", 
                                         senaste_ar = FALSE, # True om man enbart vill ha senaste år
                                         tid = c("*")){ # c("*") ger alla år


  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb,
                 readxl) 
  
  if(alla_regioner == TRUE){
    region = hamtaAllaLan(tamedriket = FALSE)
  }
  
  if(ta_med_riket == TRUE){
    region = c("00",region)
  }
  
  # Senaste år
  if (senaste_ar == TRUE){
    tid = max(hamta_giltiga_varden_fran_tabell("https://api.scb.se/OV0104/v1/doris/sv/ssd/LE/LE0105/LE0105B/LE0105Boende02", "tid"))
  }
  
  pxweb_query_list <- 
  list("Region" = region,
       "Trangboddhet" = trang,
       "Fodelseregion"= fodelseregion,
       "Alder" = Alder,
       "Kon" = Kon,
       "ContentsCode"=c("000004PS"),
       "Tid"=tid)

# Download data 
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/LE/LE0105/LE0105B/LE0105Boende02",
            query = pxweb_query_list)

# Convert to data.frame 
trångdboddhet <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

write.csv(trångdboddhet, paste0(outputmapp,filnamn), fileEncoding="UTF-8", row.names = FALSE)

}