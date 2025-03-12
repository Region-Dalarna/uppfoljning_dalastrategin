
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


# Sysselsättningsgrad från 1993 och framåt
source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_syss_rams_bas_fran_ar_1993_region_inrikesutrikes_kon_tid_RAMSForvInt03_RAMSForvInt04_RamsForvInt04N_ArRegArbStatus_scb.R")
sysselsatt <- funktion_upprepa_forsok_om_fel( function() {
  hamta_rams_bas_region_inrikesutrikes_kon_tid_scb(region_vekt = "20",
                                                   inrikesutrikes_klartext = "inrikes och utrikes födda")
}, hoppa_over = hoppa_over_felhantering)

# Sysselsättningsgrad preliminär för senaste år
source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_data_arbetsmarknadsstatus_bas_ar_prel.R")
sysselsatt_prel <- funktion_upprepa_forsok_om_fel( function() {
  hamta_arbetsmarknadsstatus_bas_ar_prel(region_vekt = "20",
                                         alder_klartext = "20-64 år",
                                         cont_klartext = "sysselsättningsgrad",
                                         fodelseregion_klartext = "totalt",
                                         tid_koder = "9999",
                                         kon_klartext = "*") %>% 
    rename("regionkod"=regionkoder) %>% 
     mutate(kön = ifelse(kön == "totalt", "kvinnor och män", kön))
}, hoppa_over = hoppa_over_felhantering)

# Binder ihop statistik
sysselsatt <- rbind(sysselsatt, sysselsatt_prel %>% filter(!(år%in%unique(sysselsatt$år))))

source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_arbetsmarknadsstatus_senastear.R")
gg_arbetsmaknadsstatus <- funktion_upprepa_forsok_om_fel( function() {
  diagram_arbetsmarknadsstatus(output_mapp_figur = output_mapp_figur,
                               spara_figur = FALSE,
                               diag_arbetslosthet = FALSE,
                               diag_arbetskraftsdeltagande = FALSE,
                               returnera_figur = TRUE,
                               returnera_data = TRUE)
}, hoppa_over = hoppa_over_felhantering)

source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_sociala_relationer_region_kon_ar_hlv1socxreg_fohm.R")

deltagande <- funktion_upprepa_forsok_om_fel( function() {
  hamta_sociala_relationer_region_kon_ar_fohm(region_vekt = "20",
                                              sociala_relationer_klartext = c("Lågt socialt deltagande","Svårt att lita på andra"),
                                              andel_och_konfidensintervall_klartext = "Andel")
}, hoppa_over = hoppa_over_felhantering) %>% 
  rename(Andel_konfidens = `Andel och konfidensintervall`,
         Andel = `Sociala relationer efter region, kön och år`,
         Sociala_relationer = `Sociala relationer`,
         Region = `region`) %>% 
  filter(!is.na(Andel))

# Hämtar data för långtidsarbetslöshet
långtidsarbetslöshet <- funktion_upprepa_forsok_om_fel( function() {
    hamta_kolada_df(kpi_id = c("N03926"),
                    valda_kommuner = "20",
                    valda_ar = 2011:2100,
                    konsuppdelat = TRUE)
  }, hoppa_over = hoppa_over_felhantering) %>% 
    mutate(kon = tolower(kon))

# långtidsarbetslöshet <- hamta_kolada_df(kpi_id = c("N03926"),
#                                         valda_kommuner = "20",
#                                         valda_ar = 2011:2100,
#                                         konsuppdelat = TRUE) %>% 
#   mutate(kon = tolower(kon))

langtidsarbetsloshet_ar_min = långtidsarbetslöshet$ar %>% min()
langtidsarbetsloshet_ar_max = långtidsarbetslöshet$ar %>% max()
langtidsarbetsloshet_kvinnor_min = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="kvinnor",ar==min(ar)) %>%  .$varde,1))
langtidsarbetsloshet_kvinnor_max = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="kvinnor",ar==max(ar)) %>%  .$varde,1))
langtidsarbetsloshet_man_min = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="män",ar==min(ar)) %>%  .$varde,1))
langtidsarbetsloshet_man_max = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="män",ar==max(ar)) %>%  .$varde,1))

# Invandringsetablering
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_etableringstid_kon_lan_tidsserie_SCB.R")
gg_etableringstid <- funktion_upprepa_forsok_om_fel( function() {
  diag_etablering_diverse_scb(output_mapp = output_mapp_figur,
                              returnera_data_rmarkdown = TRUE,
                              startar = 2012,
                              skriv_diagrambildfil = TRUE)
}, hoppa_over = hoppa_over_felhantering)

# Gini-koefficienten - 1 diagram
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
# Självskattad hälsa - diagram - Först län över tid sedan kommun
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_sjalvskattad_halsa_kon_lan_kommun_fohm.R")

gg_sjalvskattad_halsa <- funktion_upprepa_forsok_om_fel( function() {
  diag_sjalvskattad_halsa_kon_lan_kommun(region_vekt = "20",
                                         tid_koder = "*",
                                         output_mapp = output_mapp_figur,
                                         returnera_dataframe_global_environment = TRUE)
}, hoppa_over = hoppa_over_felhantering)
# gg_sjalvskattad_halsa <- diag_sjalvskattad_halsa_kon_lan_kommun(region_vekt = "20",
#                                                                 #region_vekt = "20",
#                                                                 tid_koder = "*",
#                                                                 output_mapp = output_mapp_figur,
#                                                                 returnera_dataframe_global_environment = TRUE)

gg_sjalvskattad_halsa_kommun <- funktion_upprepa_forsok_om_fel( function() {
  diag_sjalvskattad_halsa_kon_lan_kommun(region_vekt = hamtakommuner("20",tamedlan = F),
                                         #region_vekt = "20",
                                         tid_koder = "9999",
                                         kon_klartext = c("Totalt"),
                                         diagram_fargvekt = diagramfarger("rus_sex"),
                                         region_sort = TRUE,
                                         output_mapp = output_mapp_figur,
                                         returnera_dataframe_global_environment = TRUE)
}, hoppa_over = hoppa_over_felhantering)
# 
# gg_sjalvskattad_halsa_kommun <- diag_sjalvskattad_halsa_kon_lan_kommun(region_vekt = hamtakommuner("20",tamedlan = F),
#                                                                        #region_vekt = "20",
#                                                                        tid_koder = "9999",
#                                                                        kon_klartext = c("Totalt"),
#                                                                        diagram_fargvekt = diagramfarger("rus_sex"),
#                                                                        region_sort = TRUE,
#                                                                        output_mapp = output_mapp_figur,
#                                                                        returnera_dataframe_global_environment = TRUE)


rmarkdown::render(
  input = 'uppfoljning_dalastrategin_ny.Rmd',
  output_file = 'uppfoljning_dalastrategin_ny.html',
  envir = parent.frame()
)
