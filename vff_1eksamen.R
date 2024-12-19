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
  select(!Gule_poletter_stk) |>  # Eksluderer Gule_poletter_stk kolonnen, da den ikke er relevant.
  mutate(
    antal_spild = antal_bestilte - antal_afhentede, # laver ny kolonne med antallet af guld_menuer spildt til hver kamp
    afhentningsgrad = round((antal_afhentede / antal_bestilte * 100), 1), # kolonne med procentdel af bestilte der blev afhentet
    afhentede_max = round((antal_afhentede / max_antal * 100), 1), # kolonne med procentdel af max_antal der blev afhentet
    bestilte_max = round((antal_bestilte / max_antal * 100), 1), # kolonne med procentdel af max der blev bestilt
    spildegrad = round((antal_spild / antal_bestilte * 100), 1), # kolonne med procentdel af hvor mange bestilte der blev spildt
    dato = if_else(
      str_detect(dato, "^\\d+$"), # Tjekker om dato-kolonne, har tal fra start til slut, da dette karakteriserer excel-tal datoer.
      format(convert_to_date(as.numeric(dato)),   # Hvis tal hele vejen, så ændres excel-tallet til dato
             "%d.%m.%Y"),
      format(dmy(dato), "%d.%m.%Y")) # Hvis der er andet end tal f.eks. "-", som der er i de korrekt skrevede datoer, så ændres det bare til dato-format
  ) |> 
  separate(dato, into = c("dag", "måned", "år")) |> # laver nye kolonner af dato-kolonnen (dag, måned og år)
  mutate(
    dato = make_date(år, måned, dag), # formaterer til samme dato-format som skal dmi og superstats til senere brug
    ugedag = weekdays.Date(dato) # laver en kolonne med ugedage baseret på dato-kolonnen
  ) |> 
  relocate( # sætter følgende kolonner forrest i den samlede tabel.
    dato, år, måned, dag, ugedag
  )

vff <- guld_clean |> 
  group_by(ude) |> # grupperer efter hvilket udehold det er
  mutate(
    mean_afhentede = round(mean(antal_afhentede), 1), # gennemsnitligt antal afhentede pr. hold
    mean_bestilte = round(mean(antal_bestilte), 1), # gennemsnitligt antal bestilte pr. hold
    mean_afhentningsgrad = round(mean(afhentningsgrad), 1), # gennemsnitlig afhentningsgrad pr. hold
    mean_spild = round(mean(antal_spild)), 
    sidste_afhentede = antal_afhentede[which.max(dato)],
    sidste_bestilte = antal_bestilte[which.max(dato)],
    sidste_afhentningsgrad = afhentningsgrad[which.max(dato)],
    sidste_spild = antal_spild[which.max(dato)],
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

write_rds(vff, "data/vff.rds")

vff <- read_rds("data/vff.rds")
