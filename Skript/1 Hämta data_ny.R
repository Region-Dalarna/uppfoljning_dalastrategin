if (!require("pacman")) install.packages("pacman")
p_load(here,
       tidyverse)

output_mapp_figur = here("Diagram","/")

spara_figurer = FALSE

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

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

