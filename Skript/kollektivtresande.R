#######################################
### Indikator 7 - Kollektiv Resande ###
#######################################

#### Lista på variabler och koder 
#### U85427 - Marknadsandel för kollektivtrafik, andel (%)
#### N60404 - Resor med kollektivtrafik, resor/inv

#### Dra hem variablerna från Kolada
resande <- get_values(
  kpi = c("U85427", "N60404"),
  municipality = c("0020"),
  period = 2010:2100
)

# Väljer bort variabler och ger mer rimliga namn.
resande <- resande %>% 
  filter(gender == "T") %>% 
    select(-c(gender,count,municipality_type,municipality_id)) %>% 
      mutate(kpi = case_when(
        kpi == "U85427" ~ "Marknadsandel_procent",
        kpi == "N60404" ~ "Resor_per_invanare"))

### Skriv ut filen
write.csv(resande,"G:/skript/projekt/data/uppfoljning_dalastrategin/Data/resande.csv", fileEncoding="UTF-8", row.names = FALSE)