###################################################################
### Indikator 4 och 5 - Insamlat hushållsavfall samt avfall/brp ###
###################################################################

#### Lista på variabler och koder 
#### U07801 - Insamlat hush?llsavfall totalt, kg/person
#### U07485 - Insamlat farligt avfall (inkl. elavfall och batterier), kg/person
#### U07483 - Insamlat f?rpackningar och returpapper, kg/person
#### U07484 - Insamlat grovavfall, kg/person
#### U07482 - Insamlat mat- och restavfall, kg/person

# # För att komma förbi brandvägg
# set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.info()[["user"]], password = key_get("rd")))
# set_config(config(ssl_verifypeer = 0L))

#### Dra hem variablerna från Kolada
avfall <- get_values(
  kpi = c("U07801","U07485", "U07483", "U07484", "U07482", "N01951"),
  municipality = c("0020"),
  period = 2013:2100
)

# Väljer bort variabler och ger mer rimliga namn.
avfall <- avfall %>% 
  filter(gender == "T") %>% 
    select(-c(gender,count,municipality_type,municipality_id,municipality)) %>% 
      mutate(kpi = case_when(
        kpi == "U07801" ~ "Insamlat kommunalt avfall totalt, kg/invånare (justerat)",
        kpi == "U07485" ~ "Insamlat farligt avfall (inkl. elavfall och batterier), kg/person",
        kpi == "U07483" ~ "Insamlat förpackningar och returpapper, kg/person",
        kpi == "U07484" ~ "Insamlat grovavfall, kg/person",
        kpi == "U07482" ~ "Insamlat mat- och restavfall, kg/person",
        kpi == "N01951" ~ "Invånare totalt, antal"))

# ### Byter namn från kpi/kolada-koder till mer beskrivande namn
# avfall[avfall=="U07801"] <- "Insamlat kommunalt avfall totalt, kg/invånare (justerat)"
# avfall[avfall=="U07485"] <- "Insamlat farligt avfall (inkl. elavfall och batterier), kg/person"
# avfall[avfall=="U07483"] <- "Insamlat förpackningar och returpapper, kg/person"
# avfall[avfall=="U07484"] <- "Insamlat grovavfall, kg/person"
# avfall[avfall=="U07482"] <- "Insamlat mat- och restavfall, kg/person"
# avfall[avfall=="N01951"] <- "Invånare totalt, antal"

### Gör datan wide istället för long
avfall <- pivot_wider(avfall, names_from=kpi, values_from=value)

write.csv(avfall,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/avfall.csv", fileEncoding="UTF-8", row.names = FALSE)
