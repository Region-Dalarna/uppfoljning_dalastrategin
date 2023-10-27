# Källa för data: https://jordbruksverket.se/mat-och-drycker/livsmedelsstrategi-for-sverige/statistik-om-livsmedelskedjan 
 
### Marknadsandel
livsmedel <- read_excel("G:/skript/projekt/uppfoljning_rus/Data/jordbruksverket/regionalstatistik_2023.xlsx", 
                        col_names = FALSE, sheet=25, skip=42)

colnames(livsmedel) <- c("Produkt",2016:2020)

livsmedel <- gather(livsmedel, År, Andel, as.character(2016:2020), factor_key=TRUE)

write.csv(livsmedel,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/livsmedel.csv", fileEncoding="UTF-8", row.names = FALSE)


