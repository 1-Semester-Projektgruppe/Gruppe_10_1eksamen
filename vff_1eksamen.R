pacman::p_load(tidyverse, janitor, rvest, tidyr, lubridate, dplyr, readxl, tibble, dslabs, jsonlite, rjson)

# Importerer udleveret datasæt
guld_raw <- read_excel("data/Guld.xlsx")

guld_clean <- guld_raw |>
  rename( # ændrer navnene på kolonnerne til mere relevante navne
    antal_afhentede = Guld_menu_stk,
    antal_bestilte = Antal_bestilte,
    modstander = Kamp,
    dato = Dato,
    max_antal = Antal_max) |> 
  filter(
    !is.na(antal_afhentede) & # Fjerner alle rækker med NA i antal afhentede
      !is.na(antal_bestilte) # Fjerner alle rækker med NA i antal_bestilte
  ) |> 
  select(!Gule_poletter_stk) |>         # Eksluderer Gule_poletter_stk kolonnen, da den ikke er relevant.
  mutate( # Skaber nye kolonner baseret på eksisterende kolonner
    dato = if_else(
      str_detect(dato, "^\\d+$"), # Tjek for tal (Excel-datoer)
      format(convert_to_date(as.numeric(dato)),   # Konverter Excel-datonummer
             "%d.%m.%Y"),
      format(dmy(dato), "%d.%m.%Y")),                      # Konverter tekstbaserede datoer
    afhentningsgrad = antal_afhentede / antal_bestilte,
    afhentede_max = antal_afhentede / max_antal,
    bestilte_max = antal_bestilte / max_antal
  ) |> 
  separate(dato, into = c("dag", "måned", "år")) |> 
  mutate(
    dato = make_date(år, måned, dag),
    ugedag = weekdays.Date(dato)
  ) |> 
  relocate(
    dato, år, måned, dag, ugedag
  )

# antal_bestilte < max_antal # Fjernet den hvor der var for mange bestilte?

write_rds(guld_clean, "data/vff.rds")
