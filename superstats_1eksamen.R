pacman::p_load(tidyverse, janitor, rvest, tidyr, lubridate, dplyr, readxl, tibble, dslabs, jsonlite, rjson)

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
    html_table()
  bind_table <- bind_rows(table[1:antal_runder[i]])
  bind_clean <- bind_table |> 
    pivot_longer(
      cols = ends_with("...2"),
      names_to = "runde",
      values_to = "dato",
      values_drop_na = TRUE
    ) |> 
    rename(
      kamp = ...3,
      tilskuere = ...5
    ) |> 
    separate(kamp, into = c("hjemme", "ude")) |> 
    separate(dato, into = c("dag", "måned", "tid"), sep = "/| ") |> 
    mutate(
      runde = parse_number(runde),
      år = if_else(måned %in% c("02", "03", "04", "05"), sæson[i],
                   sæson[i] - 1),
      dato = make_date(år, måned, dag)
    ) |> 
    filter(hjemme == "VFF") |> 
    select(dato, år, måned, dag, tid, runde, hjemme, ude, tilskuere)
  superstats_combined <- bind_rows(superstats_combined, bind_clean)
}

superstats_flot <- superstats_combined |> 
  mutate(
    tilskuere = as.numeric(gsub("\\.", "", tilskuere))
  )

write_csv(superstats_combined, "tilskuere.csv")

write_rds(superstats_combined, "superstats.rds")