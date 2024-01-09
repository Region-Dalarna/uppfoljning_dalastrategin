
output_mapp_figur = "Diagram/"

source("https://raw.githubusercontent.com/Region-Dalarna/sarbarhetsanalys/main/Skript/diagram_branschbredd.R")
gg_branschbredd = diag_branschbredd(output_mapp_figur = output_mapp_figur,
                                    valda_farger = diagramfarger("rus_sex"),
                                    spara_figur = TRUE,
                                    returnera_data = TRUE)

# Andel förvärvsarbetande i olika branscher (såväl län som kommun)
source("https://raw.githubusercontent.com/Region-Dalarna/sarbarhetsanalys/main/Skript/diagram_andel_forvarvsarbetande_bransch.R")
gg_andel_forv <- diag_sysselsatta_andel(output_mapp_figur = output_mapp_figur,
                                        returnera_figur = TRUE,
                                        spara_figur = TRUE, 
                                        diag_lan = TRUE, 
                                        diag_kommun = FALSE,
                                        returnera_data = TRUE)

# Antal nystartade företag och antalet konkurser
source("https://github.com/Region-Dalarna/sarbarhetsanalys/raw/main/Skript/diagram_nyst_arbetslosa.R")
gg_nyst_konk <- diagram_nystartade_konkurser(output_mapp_figur = output_mapp_figur,
                                             spara_figur = TRUE,
                                             returnera_figur = TRUE,
                                             returnera_data = TRUE,
                                             vald_farg = diagramfarger("rus_sex"),
                                             cont_cod = c("N00999"))

rmarkdown::render(
  input = 'uppfoljning_dalastrategin_ny.Rmd',
  output_file = 'uppfoljning_dalastrategin_ny.html',
  envir = parent.frame()
)
