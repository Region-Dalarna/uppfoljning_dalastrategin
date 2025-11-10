# OBS! Ska sättas till FALSE när skriptet går i produktion - men kan stängas av genom att sätta till TRUE för att se att alla skript fungerar som de ska
# skriptet är till för att hantera rcurl-fel och inte vanliga fel som ju inte blir bättre av att man försöker flera gånger. =)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
hoppa_over_felhantering = FALSE

if (!require("pacman")) install.packages("pacman")
p_load(here,
       tidyverse)

output_mapp_figur = here("Diagram","/")

spara_figurer = FALSE

#########################################
####    Ett klimatsmart Dalarna     #####
#########################################

## Skogsmark
source(here("Skript","diagram_skogsmark.R"))
gg_skogsmark <- diagram_skogsmark(region = "20",
                                 output_mapp = output_mapp_figur,
                                 returnera_data = TRUE,
                                 spara_figur = spara_figurer)

ar_min_skogsmark <- as.character(min(skogsmark_df$år))
ar_max_skogsmark <- as.character(max(skogsmark_df$år))

areal_skogsmark_forandring_procent <-round(((skogsmark_df %>% filter(år==max(år)) %>% .$area_hektar/skogsmark_df %>% filter(år==min(år)) %>% .$area_hektar)-1)*100,0)
areal_skogsmark_min_ar <- format(skogsmark_df %>% filter(år==min(år)) %>% .$area_hektar,big.mark=" ")
areal_skogsmark_max_ar <- format(skogsmark_df %>% filter(år==max(år)) %>% .$area_hektar,big.mark=" ")

andel_skogsmark_min_ar <- round(skogsmark_df %>% filter(år==min(år)) %>% .$area_procent,0)
andel_skogsmark_max_ar <- round(skogsmark_df %>% filter(år==max(år)) %>% .$area_procent,0)

## Utsläpp
source(here("Skript","diagram_vaxthusgaser.R"))
gg_utslapp <- diagram_vaxthusgaser(region_vekt = "20",
                                     output_mapp = output_mapp_figur,
                                     returnera_data = TRUE,
                                     spara_figur = spara_figurer)

utslapp_min_ar <- min(vaxthusgaser_df$ar)
utslapp_max_ar <- max(vaxthusgaser_df$ar)
utslapp_min_ar_per_person <- gsub("\\.",",",round(vaxthusgaser_df %>% filter(ar == min(ar),variabel_kort == "Utsläpp per invånare",region == "Dalarna") %>% .$varde,1))
utslapp_max_ar_per_person <- gsub("\\.",",",round(vaxthusgaser_df %>% filter(ar == max(ar),variabel_kort == "Utsläpp per invånare",region == "Dalarna") %>% .$varde,1))

# Beräknar förändringar som krävs för att uppnå mål samt hur vi förändrats sedan 2015
years <- 2045 - as.integer(max(vaxthusgaser_df$ar))
slut_ar_varde = 1.25
borjan_varde = vaxthusgaser_df %>% filter(ar == max(ar),variabel_kort == "Utsläpp per invånare",region == "Dalarna") %>% .$varde
behovd_minskning <- round(abs(((slut_ar_varde / borjan_varde)^(1 / years) - 1) * 100),0)

utslapp_2015 = vaxthusgaser_df %>% filter(ar == 2015,variabel_kort == "Utsläpp per invånare",region == "Dalarna") %>% .$varde
ar_sedan_2015 <- as.integer(max(vaxthusgaser_df$ar))-2015

forandring_sedan_2015 <- round(abs(((borjan_varde / utslapp_2015)^(1 / ar_sedan_2015) - 1) * 100),0)

################################################
####    Ett konkurrenskraftigt Dalarna     #####
################################################

# Invandringsetablering
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_etableringstid_kon_lan_tidsserie_RUS.R")
gg_etableringstid <- funktion_upprepa_forsok_om_fel( function() {
  diag_etablering_diverse_scb(output_mapp = output_mapp_figur,
                              returnera_data_rmarkdown = TRUE,
                              startar = 2012,
                              skriv_diagrambildfil = TRUE)
}, hoppa_over = hoppa_over_felhantering)

# Diagram 1-2 jämförelse över tid
etablering_min_ar <- min(etablering$år)
etablering_max_ar <- max(etablering$år)
etablering_0_1_min_ar_alla <- round(etablering %>% filter(region == "Dalarna", utbildningsnivå == "samtliga utbildningsnivåer", kön == "män och kvinnor", bakgrundsvariabel == "vistelsetid 0-1 år",år == min(år)) %>% .$Andel_forvarvsarbetande,0)
etablering_0_1_max_ar_alla <- round(etablering %>% filter(region == "Dalarna", utbildningsnivå == "samtliga utbildningsnivåer", kön == "män och kvinnor", bakgrundsvariabel == "vistelsetid 0-1 år",år == max(år)) %>% .$Andel_forvarvsarbetande,0)
etablering_10_max_ar_alla <- round(etablering %>% filter(region == "Dalarna", utbildningsnivå == "samtliga utbildningsnivåer", kön == "män och kvinnor", bakgrundsvariabel == "vistelsetid 10- år", år == max(år)) %>% .$Andel_forvarvsarbetande,0)
etablering_10_max_ar_kvinnor <- gsub("\\.",",",etablering %>% filter(region == "Dalarna", utbildningsnivå == "samtliga utbildningsnivåer", kön == "kvinnor", bakgrundsvariabel == "vistelsetid 10- år",år == max(år)) %>% .$Andel_forvarvsarbetande)
etablering_10_max_ar_man <- gsub("\\.",",",etablering %>% filter(region == "Dalarna", utbildningsnivå == "samtliga utbildningsnivåer", kön == "män", bakgrundsvariabel == "vistelsetid 10- år",år == max(år)) %>% .$Andel_forvarvsarbetande)
# Diagram 3 jämförelse mellan län
etablering_lan_10_hogst_andel_lan <- etablering %>% filter(utbildningsnivå == "samtliga utbildningsnivåer", kön == "män och kvinnor", bakgrundsvariabel == "vistelsetid 10- år",år == max(år)) %>% filter(Andel_forvarvsarbetande == max(Andel_forvarvsarbetande)) %>% .$region
etablering_lan_10_hogst_andel_varde <- round(etablering %>% filter(utbildningsnivå == "samtliga utbildningsnivåer", kön == "män och kvinnor", bakgrundsvariabel == "vistelsetid 10- år",år == max(år)) %>% filter(Andel_forvarvsarbetande == max(Andel_forvarvsarbetande)) %>% .$Andel_forvarvsarbetande,0)
etablering_lan_10_lagst_andel_lan <- etablering %>% filter(utbildningsnivå == "samtliga utbildningsnivåer", kön == "män och kvinnor", bakgrundsvariabel == "vistelsetid 10- år",år == max(år)) %>% filter(Andel_forvarvsarbetande == min(Andel_forvarvsarbetande)) %>% .$region
etablering_lan_10_lagst_andel_varde <- round(etablering %>% filter(utbildningsnivå == "samtliga utbildningsnivåer", kön == "män och kvinnor", bakgrundsvariabel == "vistelsetid 10- år",år == max(år)) %>% filter(Andel_forvarvsarbetande == min(Andel_forvarvsarbetande)) %>% .$Andel_forvarvsarbetande,0)

#########################################
####    Ett sammanhållet Dalarna     ####
#########################################

# Gini-koefficienten
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_gini_tidsserie_jmf_SCB.R")
gg_gini <- funktion_upprepa_forsok_om_fel( function() {
  diag_gini_SCB(region_vekt = c("00","20"),
                region_vekt_linje =  c("00","20"),
                diag_tidsserie = TRUE,
                diag_jmfr_senastear = FALSE,
                spara_diagrambildfil = TRUE,
                output_mapp = output_mapp_figur,
                returnera_data = TRUE)
}, hoppa_over = hoppa_over_felhantering)

gini_min_ar <- min(gini_df$år)
gini_max_ar <- max(gini_df$år)

