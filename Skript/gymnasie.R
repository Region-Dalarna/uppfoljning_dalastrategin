#library(dplyr)
#library(readxl)
library(tidyverse)
#library(data.table)

# gammal kod

# #### Ta reda på vad CSV-filerna heter
# filenames <- list.files(path = getwd(), pattern = "xlsx")
# 
# gymnasie <-  map(filenames, ~ read_xlsx(.x, sheet=2, skip=8)) 
# 
# gymnasie <- lapply(gymnasie, function(df){
#   df %>%
#     filter(Län == "Dalarnas län", `Typ av huvudman`=="Samtliga") %>% 
#     select("Kommun", "Kommun-kod", "Totalt antal", "Inom 4 år...8") %>% 
#     mutate(`Antal med examen`=as.numeric((`Totalt antal`))*(as.numeric(`Inom 4 år...8`)/100))
# })
# 
# gymnasie <- Map(function(x, y) {
#   x$År <- y
#   x},
#   gymnasie, 
#   c('2011', '2012', '2013', '2014', '2015', '2016','2017'))
# 
# gymnasie <- rbindlist(gymnasie)

# ny kod för ny metod
source("G:/skript/peter/func_hamta_slutfort_3_4_5_ar_gymn_skolverket.R", encoding = "utf-8")
gymnasie <- hamta_skolverket_gymn_genomstromning(vald_region = "20")

write.csv(gymnasie,"Data/gymnasie.csv", fileEncoding="UTF-8", row.names = FALSE)
