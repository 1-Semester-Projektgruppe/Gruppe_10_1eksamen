pacman::p_load(tidyverse, janitor, rvest, tidyr, lubridate, dplyr, readxl, tibble, dslabs, jsonlite, rjson)

# Importerer udleveret datasæt
guld_raw <- read_excel("data/Guld.xlsx")

guld_clean <- guld_raw |>
  rename( # ændrer navnene på kolonnerne til mere relevante navne
    antal_afhentede = Guld_menu_stk,
    antal_bestilte = Antal_bestilte,
    ude = Kamp,
    dato = Dato,
    max_antal = Antal_max) |> 
  filter(
    !is.na(antal_afhentede) & # Fjerner alle rækker med NA i antal afhentede
      !is.na(antal_bestilte) # Fjerner alle rækker med NA i antal_bestilte
  ) |> 
  select(!Gule_poletter_stk) |>         # Eksluderer Gule_poletter_stk kolonnen, da den ikke er relevant.
  mutate( # Skaber nye kolonner baseret på eksisterende kolonner
    antal_spild = antal_bestilte - antal_afhentede,
    afhentningsgrad = round((antal_afhentede / antal_bestilte * 100), 1),
    afhentede_max = round((antal_afhentede / max_antal * 100), 1),
    bestilte_max = round((antal_bestilte / max_antal * 100), 1),
    spildegrad = round((antal_spild / antal_bestilte * 100), 1),
    dato = if_else(
      str_detect(dato, "^\\d+$"), # Tjek for tal (Excel-datoer)
      format(convert_to_date(as.numeric(dato)),   # Konverter Excel-datonummer
             "%d.%m.%Y"),
      format(dmy(dato), "%d.%m.%Y"))                      # Konverter tekstbaserede datoer
  ) |> 
  mutate(
    dato = make_date(år, måned, dag),
    ugedag = weekdays.Date(dato)
  ) |> 
  separate(dato, into = c("dag", "måned", "år")) |> 
  relocate(
    dato, år, måned, dag, ugedag
  )

vff <- guld_clean |> 
  group_by(ude) |> 
  mutate(
    mean_afhentede = round(mean(antal_afhentede), 1),
    mean_bestilte = round(mean(antal_bestilte), 1),
    mean_afhentningsgrad = round(mean(afhentningsgrad), 1),
    mean_spild = round(mean(antal_spild)),
    sidste_afhentede = antal_afhentede[which.max(dato)],
    sidste_bestilte = antal_bestilte[which.max(dato)],
    sidste_afhentningsgrad = afhentningsgrad[which.max(dato)],
    sidste_spild = antal_spild[which.max(dato)],
    årstid = case_when(
      måned %in% c("12", "01", "02") ~ "Vinter",
      måned %in% c("03", "04", "05") ~ "Forår",
      måned %in% c("06", "07", "08") ~ "Sommer",
      måned %in% c("09", "10", "11") ~ "Efterår"
    )
  ) |> 
  ungroup() |> 
  mutate(
    årstid = case_when(
      måned %in% c("12", "01", "02") ~ "Vinter",
      måned %in% c("03", "04", "05") ~ "Forår",
      måned %in% c("06", "07", "08") ~ "Sommer",
      måned %in% c("09", "10", "11") ~ "Efterår"
  )) |> 
  relocate(årstid, .after = år)

# Skal vi fjerne den hvor der var for mange bestilte? antal_bestilte < max_antal

write_rds(vff, "data/vff.rds")

vff <- read_rds("data/vff.rds")
