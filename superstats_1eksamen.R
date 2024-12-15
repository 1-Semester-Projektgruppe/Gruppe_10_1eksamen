pacman::p_load(tidyverse, janitor, rvest, tidyr, lubridate, dplyr, readxl, tibble, dslabs, jsonlite, rjson)

View(sæson)

# Webscraping: Superstats.dk
base_url <- "https://superstats.dk/program?season="

superstats_combined <- tibble()

sæson <- c(2014, 2016, 2017, 2022, 2023, 2024)
antal_runder <- c(33, 33, 36, 32, 32, 32)

for (i in 1:6) {
  url <- paste0(base_url, sæson[i])
  html <- read_html(url)
  table <- html |> 
    html_elements("table") |> 
    html_table(convert = FALSE) # convert = FALSE, for at undgå tilskuere automatisk håndteres som double og derfor 
                                # sletter 0 til sidst i tal senere
  bind_table <- bind_rows(table[1:antal_runder[i]])
  bind_clean <- bind_table |> 
    separate(...3, into = c("hjemme", "ude")) |> 
    filter(hjemme == "VFF") |> 
    pivot_longer(
      cols = ends_with("...2"),
      names_to = "runde",
      values_to = "dato",
      values_drop_na = TRUE
    ) |> 
    separate(dato, into = c("dag", "måned","kampstart"), sep = "/| ") |> 
    mutate(
      tilskuere = as.numeric(gsub("\\.", "", ...5)), #gsub fjerner "." og skifter det ud med "", altså ingenting.
      runde = parse_number(runde),
      år = if_else(måned %in% c("02", "03", "04", "05"), sæson[i],
                   sæson[i] - 1),
      dato = make_date(år, måned, dag),
      tid = parse_time(tid)
    ) |> 
    select(dato, år, måned, dag, tid, runde, hjemme, ude, tilskuere)
  superstats_combined <- bind_rows(superstats_combined, bind_clean)
}

# Note om valg af sæsoner: 
# Vi kunne have valgt at tage data fra flere sæsoner kun til udregning af gennemsnitlige tilskuere, men jeg vil mene dette er en dårlig
# idé, da VFF har markant flere tilskuere i gennemsnit de sidste par år, sammenlignet med f.eks. 2007/08. Her var gennemsnittet 4416 og i 
# 2023/24 er gennemsnittet 6371. Så hvis vi tager de tidligere år med, så vil det bare unødvendigt trække gennemsnittet af tilskuerne ned
# og derfor gøre det mere upræcist i fremtiden.

# Men nu kom jeg lige i tanke om at vi kunne tage de kampe med fra sæson 2024/25

# Her er webscraping kode til hvis vi vil have tilskuer-data med fra den nyeste sæson også:

base_url <- "https://superstats.dk/program?season="

superstats_combined <- tibble()

sæson <- c(2014, 2016, 2017, 2022, 2023, 2024, 2025)
antal_runder <- c(33, 33, 36, 32, 32, 32, 17)

for (i in 1:7) {
  url <- paste0(base_url, sæson[i])
  html <- read_html(url)
  table <- html |> 
    html_elements("table") |> 
    html_table(convert = FALSE) # convert = FALSE, for at undgå tilskuere automatisk håndteres som double og derfor 
  # sletter 0 til sidst i tal senere
  bind_table <- bind_rows(table[1:antal_runder[i]])
  bind_clean <- bind_table |> 
    separate(...3, into = c("hjemme", "ude")) |> 
    filter(hjemme == "VFF") |> 
    pivot_longer(
      cols = ends_with("...2"),
      names_to = "runde",
      values_to = "dato",
      values_drop_na = TRUE
    ) |> 
    separate(dato, into = c("dag", "måned","kampstart"), sep = "/| ") |> 
    mutate(
      tilskuere = as.numeric(gsub("\\.", "", ...5)), #gsub fjerner "." og skifter det ud med "", altså ingenting.
      runde = parse_number(runde),
      år = if_else(måned %in% c("02", "03", "04", "05"), sæson[i],
                   sæson[i] - 1),
      dato = make_date(år, måned, dag)
    ) |> 
    select(dato, år, måned, dag, kampstart, runde, hjemme, ude, tilskuere)
  superstats_combined <- bind_rows(superstats_combined, bind_clean)
}

superstats_flot <- superstats_combined |> 
  group_by(ude) |> 
  mutate(
    mean_tilskuere = round(mean(tilskuere), 0),
    sidste_tilskuer_antal = tilskuere[which.max(dato)]
  ) |> 
  ungroup()

write_rds(superstats_flot, "data/superstats.rds")

superstats <- read_rds("data/superstats.rds")
