# Källa för data: https://jordbruksverket.se/mat-och-drycker/livsmedelsstrategi-for-sverige/statistik-om-livsmedelskedjan 
# För tillfället är denna data dock inte uppdaterad, varför jag använder en fil (regionalstatistik_2023) som jag fått mailad från Jordbruksverket. 
# Detta mail fick jag under våren 2023 och under våren 2024 mailar jag dom på nytt för att undersöka om det finns uppdaterad data. Sparas i så fall som regionalstatistik_2024
 
### Marknadsandel
livsmedel <- read_excel("G:/skript/projekt/data/uppfoljning_dalastrategin/Data/jordbruksverket/regionalstatistik_2023.xlsx", 
                        col_names = FALSE, sheet=25, skip=42)

colnames(livsmedel) <- c("Produkt",2016:2020)

livsmedel <- gather(livsmedel, År, Andel, as.character(2016:2020), factor_key=TRUE)

write.csv(livsmedel,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/livsmedel.csv", fileEncoding="UTF-8", row.names = FALSE)
