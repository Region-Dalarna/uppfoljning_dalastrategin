# Data från Folkhälsomyndigheten för lågt socialt deltagande:
# Länk: http://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/pxweb/sv/A_Folkhalsodata/A_Folkhalsodata__B_HLV__eSocialarel__aSocialarel/hlv1socxreg.px/
pacman::p_load(pxweb,httr,askpass,tidyverse)

source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)

vald_region="20"

pxweb_query_list <- 
  list("Region"=c(vald_region),
       "Sociala relationer"=c("35"),
       "Andel och konfidensintervall"=c("01"),
       "Kön"=c("00","01","02"),
       "År"=c("*")
       )

# Download data 
px_data <- 
  pxweb_get(url = "http://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/api/v1/sv/A_Folkhalsodata/B_HLV/eSocialarel/aSocialarel/hlv1socxreg.px",
            query = pxweb_query_list)

# Convert to data.frame 
deltagande <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text") %>%
  rename("Andel"=`Sociala relationer efter region, kön och år`)

write.csv(deltagande,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/deltagande_ny.csv", fileEncoding="UTF-8", row.names = FALSE)
