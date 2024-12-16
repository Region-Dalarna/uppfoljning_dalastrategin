##### Hämta data till RUS-indikatorerna #######

########################
#### Ladda in paket ####
########################
if (!require("pacman")) install.packages("pacman")
# pacman::p_load(httr,
#                purrr,
#                tidyr,
#                tidyverse,
#                rKolada,
#                pxweb,
#                askpass,
#                readxl,
#                data.table,
#                keyring)

p_load(httr,
       keyring)

##############
## Gateway ###
##############
# set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.info()[["user"]], password = key_get("rd")))
# set_config(config(ssl_verifypeer = 0L))

outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/"

###########################
### Klimatsmart Dalarna ###
###########################

###############
### Utsläpp ### 
###############
source("Skript/utslapp.R", encoding="UTF-8")
hamta_data_utslapp(outputmapp = outputmapp)

##############
### Avfall ### 
##############
source("Skript/avfall_ny.R", encoding="UTF-8")
hamta_data_avfall(outputmapp = outputmapp)

########################
### Energiproduktion ### 
########################
source("Skript/energiproduktion.R", encoding="UTF-8")
hamta_data_energiproduktion(outputmapp = outputmapp)

##########################
### Energieffektivitet ### 
##########################
source("Skript/energieffektivitet.R", encoding="UTF-8")
hamta_data_energieffektivitet(outputmapp = outputmapp)

#################
### Skogsmark ### 
#################
source("Skript/skogsmark.R", encoding="UTF-8")
hamta_data_skogsmark(outputmapp = outputmapp)

#################
### Betesmark ### 
#################
source("Skript/betesmark.R", encoding="UTF-8")
hamta_data_betesmark(region = c("0000","0020"),
                     outputmapp = outputmapp)

##########################
### Kollektivt resande ### 
##########################
source("Skript/kollektivtresande.R", encoding="UTF-8")
hamta_data_kollektivtresande(outputmapp = outputmapp)

##########################
### Livsmedel          ###  OBS! Fungerar inte!
##########################
# OBS - uppdateras inte automatiskt. År behöver för tillfället "hårdkodas"
source("Skript/livsmedel.R", encoding="UTF-8")
hamta_data_livsmedel(outputmapp = outputmapp,
                     filnamn = "livsmedel_ny.csv")

###############################
### Självförsörjning energi ###
###############################
source("Skript/energikonsumtion.R", encoding="UTF-8")
hamta_data_energikonsumtion(outputmapp = outputmapp)

##################################
### Konkurrenskraftigt Dalarna ###
##################################

#########################
## Chefsrepresentation ##
#########################
source("Skript/chefspositioner.R", encoding="UTF-8")
hamta_data_chefrepresentation(outputmapp = outputmapp)

#####################
## Ginikoefficient ##
#####################
source("Skript/gini.R", encoding="UTF-8")
hamta_data_gini(outputmapp = outputmapp)

##########################
## Långtidsarbetslöshet ##
##########################
source("Skript/långtidsarbetslöshet.R", encoding="UTF-8")
hamta_data_langtidsarb(output_mapp = outputmapp)

###############################
## Eftergymnasial utbildning ##
###############################
source("Skript/eftergymnasial.R", encoding="UTF-8")
hamta_data_eftergymnasial(outputmapp = outputmapp)

###############################
## Gymnasial genomströmmning ##
###############################
source("Skript/gymnasie.R", encoding="UTF-8")

###############
## Matchning ## OBS! Fungerar inte! Se andra skript, tror de har bytt adress
###############
source("Skript/matchning.R", encoding="UTF-8")
hamta_data_matchning(outputmapp = outputmapp)

#################
## Sysselsatta ## - Se körfil, hämtas numer via hamtadata-skript
#################
# source("Skript/sysselsatta.R", encoding="UTF-8")
# hamta_data_sysselsatta(outputmapp = outputmapp)

# source("Skript/sysselsatta_ny.R", encoding="UTF-8")
# hamta_data_sysselsatta_ny(outputmapp = outputmapp)

####################
## Etableringstid ##
####################
source("Skript/invandringetablering.R", encoding="UTF-8")
hamta_data_invandringsetablering(outputmapp = outputmapp,
                                 start_ar = "2010")

####################
## Forskning ##
####################
source("Skript/forskning.R", encoding="UTF-8")
hamta_data_forskning(outputmapp = outputmapp)

#####################################
######### Sammanhållet Dalarna ######
#####################################

##################
## Boendeformer ##
##################
source("Skript/boende.R", encoding="UTF-8")
hamta_data_boende(outputmapp = outputmapp,
                  Boendeform = c("SMAG","SMBO","FBBO","FBHY0"))

##############
## Bredband ##
##############
source("Skript/bredband.R", encoding="UTF-8")
hamta_data_bredband(outputmapp = outputmapp)

##############
## Service ##
##############
source("Skript/service.R", encoding="UTF-8")
hamta_data_service(outputmapp = outputmapp)

########################
## Tillgång till vård ##
########################
source("Skript/vard.R", encoding="UTF-8")
hamta_data_vard(outputmapp = outputmapp)

########################
## Socialt deltagande ## OBS! Fungerar inte! Måste eventuellt uppdateras baserat på Peters nya skript för att hämta data från FOHM
########################
source("Skript/socialtdeltagande_ny.R", encoding="UTF-8")
hamta_data_socialtdeltagande(outputmapp = outputmapp)

############
## Tillit ##
############
# Data ingår i uttag för socialt deltagande (ovan)
#source("Skript/tillit_ny.R", encoding="UTF-8")

##################
## Trångboddhet ##
##################
source("Skript/trangdboddhet.R", encoding="UTF-8")
hamta_data_trangboddhet(outputmapp = outputmapp,
                        fodelseregion = "200",
                        Alder = "totalt",
                        Kon = "TOT2")

###############
## Utsatthet ##
###############
source("Skript/utsatta.R", encoding="UTF-8")
hamta_data_utsatta(outputmapp = outputmapp)

###################
## Valdeltagande ##
###################
# source("Skript/valdeltagande.R", encoding="UTF-8")
# hamta_data_valdeltagande(outputmapp = outputmapp)

source("Skript/valdeltagande_nationellt.R", encoding="UTF-8")
berakna_valdeltagande_nationellt(outputmapp = outputmapp)

source("Skript/valdeltagande_EU.R", encoding="UTF-8")
berakna_valdeltagande_EU(outputmapp = outputmapp)
