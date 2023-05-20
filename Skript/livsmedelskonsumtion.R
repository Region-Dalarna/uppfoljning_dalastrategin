# Källa för data: https://jordbruksverket.se/mat-och-drycker/livsmedelsstrategi-for-sverige/statistik-om-livsmedelskedjan
# Sannolikt gammal kod som inte används. Använd istället livsmedel.R
library(dplyr)
library(readxl)
library(tidyverse)
library(data.table)

#setwd("G:/Uppföljning och Utvärdering/Analys/RUS-Indikatorer/Data/jordbruksverket/")

#regionalstatistik <- read_excel("G:/Uppföljning och Utvärdering/Analys/RUS-Indikatorer/Data/jordbruksverket/regionalstatistik.xlsx", 
#                                  col_names = FALSE, sheet=23, skip=3)

#totalkonsumtion <- regionalstatistik[-c(13:116),-c(1:25)]

#colnames(totalkonsumtion) <- c("Produkt",1999:2018)

#totalkonsumtion <- gather(totalkonsumtion, år, measurement, c("1999", "2000", "2001", "2002", "2003", "2004", 
#                                                          "2005", "2006", "2007", "2008", "2009", "2010",
#                                                           "2011", "2012", "2013", "2014", "2015", "2016",
#                                                           "2017", "2018"), factor_key=TRUE)

### Marknadsandel
# Marknadsandel <- read_excel("G:/Uppföljning och Utvärdering/Analys/RUS-Indikatorer/Data/jordbruksverket/regionalstatistik.xlsx",
#                                 col_names = FALSE, sheet=24, skip=42)

Marknadsandel <- read_excel("G:/skript/projekt/uppfoljning_rus/Data/jordbruksverket/regionalstatistik_2023.xlsx", 
                            col_names = FALSE, sheet=25, skip=42)

Marknadsandel <- Marknadsandel(-c(7:10,13:15,18:22,),-c(1, 6:11))
colnames(Marknadsandel) <- c("Produkt",2016:2020)

livsmedel <- gather(Marknadsandel, År, Andel, as.character(2016:2020), factor_key=TRUE)
