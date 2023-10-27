##### Hämta data till RUS-indikatorerna #######

########################
#### Ladda in paket ####
########################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr,
               purrr,
               tidyr,
               tidyverse,
               rKolada,
               pxweb,
               askpass,
               readxl,
               data.table,
               keyring)

##############
## Gateway ###
##############
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.info()[["user"]], password = askpass(prompt = "Please enter your password: ")))
set_config(config(ssl_verifypeer = 0L))
# Skriv in det lösenord som används vid inloggning
#key_set("rd")

###########################
### Klimatsmart Dalarna ###
###########################

###############
### Utsläpp ### 
###############
source("Skript/utslapp.R", encoding="UTF-8")

##############
### Avfall ### 
##############
source("Skript/avfall.R", encoding="UTF-8")

source("Skript/avfall2.R", encoding="UTF-8")

########################
### Energiproduktion ### 
########################
source("Skript/energiproduktion.R", encoding="UTF-8")

##########################
### Energieffektivitet ### 
##########################
source("Skript/energieffektivitet.R", encoding="UTF-8")

#################
### Skogsmark ### 
#################
source("Skript/skogsmark.R", encoding="UTF-8")

#################
### Betesmark ### 
#################
source("Skript/betesmark.R", encoding="UTF-8")

##########################
### Kollektivt resande ### 
##########################
source("Skript/kollektivtresande.R", encoding="UTF-8")

##########################
### Livsmedel          ### 
##########################
# OBS - uppdateras inte automatiskt. År behöver för tillfället "hårdkodas"
source("Skript/livsmedel.R", encoding="UTF-8")

###############################
### Självförsörjning energi ###
###############################
source("Skript/energikonsumtion.R", encoding="UTF-8")

##################################
### Konkurrenskraftigt Dalarna ###
##################################

#########################
## Chefsrepresentation ##
#########################
source("Skript/chefspositioner.R", encoding="UTF-8")

#####################
## Ginikoefficient ##
#####################
source("Skript/gini.R", encoding="UTF-8")

##########################
## Långtidsarbetslöshet ##
##########################
source("Skript/långtidsarbetslöshet.R", encoding="UTF-8")

###############################
## Eftergymnasial utbildning ##
###############################
source("Skript/eftergymnasial.R", encoding="UTF-8")

###############################
## Gymnasial genomströmmning ##
###############################
source("Skript/gymnasie.R", encoding="UTF-8")

###############
## Matchning ##
###############
source("Skript/matchning.R", encoding="UTF-8")

###############
## Sysselsatta ##
###############
source("Skript/sysselsatta.R", encoding="UTF-8")

####################
## Etableringstid ##
####################
source("Skript/invandringetablering.R", encoding="UTF-8")

####################
## Forskning ##
####################
source("Skript/forskning.R", encoding="UTF-8")

#####################################
######### Sammanhållet Dalarna ######
#####################################

##################
## Boendeformer ##
##################
source("Skript/boende.R", encoding="UTF-8")

##############
## Bredband ##
##############
source("Skript/bredband.R", encoding="UTF-8")

##############
## Service ##
##############
source("Skript/service.R", encoding="UTF-8")

########################
## Socialt deltagande ##
########################
source("Skript/socialtdeltagande_ny.R", encoding="UTF-8")

############
## Tillit ##
############
source("Skript/tillit_ny.R", encoding="UTF-8")

##################
## Trångboddhet ##
##################
source("Skript/trangdboddhet.R", encoding="UTF-8")

###############
## Utsatthet ##
###############
source("Skript/utsatta.R", encoding="UTF-8")

###################
## Valdeltagande ##
###################
source("Skript/valdeltagande.R", encoding="UTF-8")

