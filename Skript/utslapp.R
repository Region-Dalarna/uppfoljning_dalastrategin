#### N07702 - Utsläpp till luft av växthusgaser totalt, ton CO2-ekv ####
#### N00401 - Utsläpp till luft av växthusgaser totalt, ton CO2-ekv/inv ####
#### N85535 - Utsläpp till luft av växthusgaser, arbetsmaskiner, ton CO2e ####
#### N85532 - Utsläpp till luft av växthusgaser, egen uppvärmning, ton CO2e ####
#### N85536 - Utsläpp till luft av växthusgaser, el och fjärrvärme, ton CO2e
#### N85537 - Utsläpp till luft av växthusgaser, industri, ton CO2e
#### N85538 - Utsläpp till luft av växthusgaser, jordbruk, ton CO2e
#### N85533 - Utsläpp till luft av växthusgaser, transporter, ton CO2e
#### N85534 - Utsläpp till luft av växthusgaser, utrikes transporter, ton CO2e 

### Dra hem variablerna från Kolada
vaxthusgaser <- get_values(
  kpi = c("N85533", "N85534", "N85538", "N85537", "N85536", "N85532", "N85535", "N07702", "N00401"),
  municipality = c("0020", "0000"),
  period = 2008:2100
)

### Vi behöver inte dessa kolumner ###
vaxthusgaser$gender <- NULL
vaxthusgaser$count <- NULL
vaxthusgaser$municipality_type <- NULL

### Byter namn från kpi/kolada-koder till mer beskrivande namn
vaxthusgaser[vaxthusgaser=="N07702"] <- "Utsläpp till luft av växthusgaser totalt"
vaxthusgaser[vaxthusgaser=="N85535"] <- "Utsläpp till luft av växthusgaser, arbetsmaskiner"
vaxthusgaser[vaxthusgaser=="N85532"] <- "Utsläpp till luft av växthusgaser, egen uppvärmning"
vaxthusgaser[vaxthusgaser=="N85536"] <- "Utsläpp till luft av växthusgaser, el och fjärrvärme"
vaxthusgaser[vaxthusgaser=="N85537"] <- "Utsläpp till luft av växthusgaser, industri"
vaxthusgaser[vaxthusgaser=="N85538"] <- "Utsläpp till luft av växthusgaser, jordbruk"
vaxthusgaser[vaxthusgaser=="N85533"] <- "Utsläpp till luft av växthusgaser, transporter"
vaxthusgaser[vaxthusgaser=="N85534"] <- "Utsläpp till luft av växthusgaser, utrikes transporter"
vaxthusgaser[vaxthusgaser=="N00401"] <- "Utsläpp till luft av växthusgaser totalt/inv"

### Gör datan wide istället för long
vaxthusgaser <- pivot_wider(vaxthusgaser, names_from=kpi, values_from=value)

# Byt namn på kolumner
colnames(vaxthusgaser) <- c("Id", "Ar", "Geografi", "UtslappPerInv", "TotaltUtslapp", "UppvarmningUtslapp", 
                            "TransporterUtslapp", "ArbetsmaskinerUtslapp", "ElFjarrvarme", "IndustriUtslapp", 
                            "JordbrukUtslapp")

vaxthusgaser$Geografi[vaxthusgaser$Geografi=="Region Dalarna"] <- "Dalarnas län"

write.csv(vaxthusgaser,"Data/vaxthusgaser.csv", fileEncoding="UTF-8", row.names = FALSE)
