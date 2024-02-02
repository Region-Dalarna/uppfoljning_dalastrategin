
output_mapp_figur = "Diagram/"

source("https://raw.githubusercontent.com/Region-Dalarna/sarbarhetsanalys/main/Skript/diagram_branschbredd.R")
gg_branschbredd = diag_branschbredd(output_mapp_figur = output_mapp_figur,
                                    valda_farger = diagramfarger("rus_sex"),
                                    spara_figur = TRUE,
                                    returnera_data = TRUE)

# Andel förvärvsarbetande i olika branscher (såväl län som kommun)
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_andel_forvarvsarbetande_bransch.R")
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

source("G:/skript/diagram/diag_utbniva_over_tid_och_andel_specifikt_ar.R")
gg_utbniva_85 <- diag_utbniva_lang_tidserie(region_vekt = c("20"),
                                            output_mapp = output_mapp_figur,
                                            diagram_capt = "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna",
                                            skapa_fil = TRUE,
                                            diag_hogutb_over_tid = TRUE,
                                            diag_lagutb_over_tid = FALSE,
                                            diag_andel_alla_utbnivaer = FALSE,
                                            diag_andel_eftergymn_jmfr_lan = FALSE)

rmarkdown::render(
  input = 'uppfoljning_dalastrategin_ny.Rmd',
  output_file = 'uppfoljning_dalastrategin_ny.html',
  envir = parent.frame()
)
