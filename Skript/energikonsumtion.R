####################################
### Indikator - Energikonsumtion ###
####################################

#### Lista på variabler och koder 
#### N45926 - Elproduktion totalt inom det geografiska området, MWh

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               rKolada,
               readxl)

#### Dra hem variablerna från Kolada
elkonsumtion <- get_values(
  kpi = c("N45906", "N01951"),
  municipality = c("0020"),
  period = 2016:2100
)

# Väljer bort variabler och ger mer rimliga namn.
elkonsumtion <- elkonsumtion %>% 
  filter(gender == "T") %>% 
    select(-c(gender,count,municipality_type,municipality_id)) %>% 
      mutate(kpi = case_when(
        kpi == "N45906" ~ "Slutanvändning av el inom det geografiska området, MWh/inv",
        kpi == "N01951" ~ "Invånare totalt, antal"))

# Beräknar total konsumtion av el. Detta är enklare om data först görs om till wide
elkonsumtion <- pivot_wider(elkonsumtion, names_from=kpi, values_from=value) %>% 
  mutate(konsumtion = `Invånare totalt, antal`*`Slutanvändning av el inom det geografiska området, MWh/inv`) %>%
    select(-c(`Invånare totalt, antal`,`Slutanvändning av el inom det geografiska området, MWh/inv`)) 

# Läser in data för produktionen av el i Dalarna och relaterar denna till total produktion. Detta ger en självförsörjningsgrad för el 
el <- read.csv("G:/skript/projekt/data/uppfoljning_dalastrategin/Data/elproduktion.csv",encoding="UTF-8") %>% 
  rename(produktion = value) %>% 
    filter(kpi == "Totalt")  %>% 
      right_join(elkonsumtion,c("year","municipality")) %>% 
        mutate(elgrad = (produktion/konsumtion)*100) %>% 
          filter(!(is.na(elgrad))) %>% 
            select(year,municipality,elgrad)

write.csv(el,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/el.csv", fileEncoding="UTF-8", row.names = FALSE)
