
# OBS! Ska sättas till FALSE när skriptet går i produktion - men kan stängas av genom att sätta till TRUE för att se att alla skript fungerar som de ska
# skriptet är till för att hantera rcurl-fel och inte vanliga fel som ju inte blir bättre av att man försöker flera gånger. =)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
hoppa_over_felhantering = FALSE

output_mapp_figur = "Diagram/"

# Genomströmning gymnasiet

source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_gymn_avg_genomstromning_4ar_prg_skolverket.R")

gymnasie <- gg_branschbredd <- funktion_upprepa_forsok_om_fel( function() {
  hamta_gymn_avg_genomstromning_4ar_prg_skolverket(region_vekt = "20",
                                                   huvudman = "Samtliga",
                                                   gymnasieprogram = "Gymnasieskolan totalt")
}, hoppa_over = hoppa_over_felhantering)


# Branschbredd
source("https://raw.githubusercontent.com/Region-Dalarna/sarbarhetsanalys/main/Skript/diagram_branschbredd.R")

gg_branschbredd <- funktion_upprepa_forsok_om_fel( function() {
  diag_branschbredd(output_mapp_figur = output_mapp_figur,
                   valda_farger = diagramfarger("rus_sex"),
                   spara_figur = TRUE,
                   returnera_data = TRUE)
}, hoppa_over = hoppa_over_felhantering)


# Andel förvärvsarbetande i olika branscher (såväl län som kommun)
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_andel_forvarvsarbetande_bransch.R")
gg_andel_forv <- funktion_upprepa_forsok_om_fel( function() {
  diag_sysselsatta_andel(output_mapp_figur = output_mapp_figur,
                         returnera_figur = TRUE,
                         spara_figur = TRUE, 
                         diag_lan = TRUE, 
                         diag_kommun = FALSE,
                         returnera_data = TRUE)
}, hoppa_over = hoppa_over_felhantering)

# Antal nystartade företag och antalet konkurser
source("https://github.com/Region-Dalarna/sarbarhetsanalys/raw/main/Skript/diagram_nyst_arbetslosa.R")
gg_nyst_konk <- funktion_upprepa_forsok_om_fel( function() {
  diagram_nystartade_konkurser(output_mapp_figur = output_mapp_figur,
                               spara_figur = TRUE,
                               returnera_figur = TRUE,
                               returnera_data = TRUE,
                               vald_farg = diagramfarger("rus_sex"),
                               cont_cod = c("N00999"))
}, hoppa_over = hoppa_over_felhantering)

source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_utbniva_flera_diagram_scb.R")
gg_utbniva_85 <- funktion_upprepa_forsok_om_fel( function() {
  diag_utbniva_tidserie_och_lansjmfr (region_vekt = c("20"),
                                      output_mapp = output_mapp_figur,
                                      diagram_capt = "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna",
                                      skapa_fil = TRUE,
                                      diag_hogutb_over_tid = TRUE,
                                      diag_lagutb_over_tid = FALSE,
                                      diag_andel_alla_utbnivaer = FALSE)
}, hoppa_over = hoppa_over_felhantering)

source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_syss_rams_bas_fran_ar_1993_region_inrikesutrikes_kon_tid_RAMSForvInt03_RAMSForvInt04_RamsForvInt04N_ArRegArbStatus_scb.R")
sysselsatt <- funktion_upprepa_forsok_om_fel( function() {
  hamta_rams_bas_region_inrikesutrikes_kon_tid_scb(region_vekt = "20",
                                                   inrikesutrikes_klartext = "inrikes och utrikes födda") 
}, hoppa_over = hoppa_over_felhantering)

source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_arbetsmarknadsstatus_senastear.R")
gg_arbetsmaknadsstatus <- funktion_upprepa_forsok_om_fel( function() {
  diagram_arbetsmarknadsstatus(output_mapp_figur = output_mapp_figur,
                               spara_figur = FALSE,
                               diag_arbetslosthet = FALSE,
                               diag_arbetskraftsdeltagande = FALSE,
                               returnera_figur = TRUE,
                               returnera_data = TRUE)
}, hoppa_over = hoppa_over_felhantering)

rmarkdown::render(
  input = 'uppfoljning_dalastrategin_ny.Rmd',
  output_file = 'uppfoljning_dalastrategin_ny.html',
  envir = parent.frame()
)
