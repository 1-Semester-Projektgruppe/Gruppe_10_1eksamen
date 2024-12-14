pacman::p_load(tidyverse, janitor, rvest, tidyr, lubridate, dplyr, readxl, tibble, dslabs, jsonlite, rjson)

# Importerer udleveret datasæt
guld_raw <- read_excel("Guld.xlsx")

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

mean_afhentningsgrad <- mean(guld_clean$afhentningsgrad) # Gennemsnitlig afhentningsgrad = 0,86
mean_bestilte <- mean(guld_clean$antal_bestilte) # Gennemsnitlig antal bestilte = 798,4 stk.
mean_afhentede <- mean(guld_clean$antal_afhentede) # Gennemsnitlig antal afhentede = 689,6 stk.
mean_spild <- mean_bestilte - mean_afhentede # Gennemsnitligt antal spildte guldmenuer = 108,8 stk.
mean_spild * 50 * 16

total_bestilte <- sum(guld_clean$antal_bestilte) # = 78.242 stk.
total_afhentede <- sum(guld_clean$antal_afhentede) # = 67.579 stk.
total_spildt <- total_bestilte - total_afhentede # = 10.663 stk.

# Webscraping: Superstats.dk
base_url <- "https://superstats.dk/program?season="

combined_df <- tibble()

sæson <- c(2014, 2016, 2017, 2022, 2023, 2024)
antal_runder <- c(33, 33, 36, 32, 32, 32)

View(table)

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
  combined_df <- bind_rows(combined_df, bind_clean)
 }

write_csv(combined_df, "tilskuere.csv")

samlet_dmi <- tibble()

samlet_dmi_flot <- tibble()

tilskuere <- read_csv("tilskuere.csv")

# Evt. joine superstats + guld + vejrdata med SQL, for at vise vi kan det?

# Tilføj ugedag, ferie?, 

# API: DMI
# Lave variabler for URL: base, info, station, alle datoer
alle_datoer <- as.character(guld_clean$dato) # as.character() fordi ellers så bruger den mærkelige tal som dato
tidspunkt <- tilskuere$tid

# Definere dele af request-url
base_url <- "https://dmigw.govcloud.dk/v2/"
info_url <- "metObs/collections/observation/items?"
station <- "stationId=06060"
limit <- "&limit=300000"
api_key <- "&api-key=8119aa28-31a1-4cec-9ab7-549fb14e0090"

for (dato_x in alle_datoer) {
  
  # Definere hvilken dato der skal hentes data fra.
  start_dato <- paste0("&datetime=", dato_x, "T00:00:00Z")
  slut_dato <- paste0("/", dato_x, "T23:59:59Z")
  
  # Samle fulde URL
  full_url <- paste0(base_url, info_url, station, start_dato, slut_dato, limit, api_key)

  # Lave en get-efterspørgsel til API
  api_call <- httr::GET(full_url)
  
  # Tjekke statuskode og http_type
  if (api_call$status_code == 200) {
    api_char <- base::rawToChar(api_call$content)
    api_JSON <- jsonlite::fromJSON(api_char, flatten = TRUE)
    dmi_dato_tibble <- as_tibble(api_JSON$features)
    
    # Rydde op i data
    dmi_flot <- dmi_dato_tibble |> 
      mutate(
        dato = parse_date(substr(properties.observed, 1, 10)),
        tid = parse_time(substr(properties.observed, 12, 19))
      ) |> 
      select(
        dato, tid, properties.parameterId, properties.value
      ) |> 
      pivot_wider(
        names_from = properties.parameterId,
        values_from = properties.value
      )
    samlet_dmi_flot <- bind_rows(samlet_dmi_flot, dmi_flot)
  } else {  # Printer besked om hvis bestemte datoer ikke havde status-kode 200, og derfor ikke kunne hentes.
    warning(paste("Fejl ved", dato_x))
  }
}
  
saveRDS(samlet_dmi_flot, file = "samlet_dmi_flot.RData")

load("samlet_dmi_flot.RData")

write_rds(samlet_dmi_flot, "samlet_dmi_flot.rds")

flot_dmi <- read_rds("samlet_dmi_flot.rds")

kampstart <- tilskuere$tid

# Noter fra Bjarne
# fredag, lørdag, søndag + andre. Factor og dummy variabler
# forward, ridge, lasso, naiv model y streg 1 gennemsnitligt antal bestilte/afhentede
# test alle på test-data

flot_dmi |> 
  group_by(dato, )
  mutate(
    # Udregner gennemsnitlig temperatur baseret på 1 time før kampstart og 2 timer efter.
    kamp_mean_temp = mean([kampstart])
  )

time_før_kamp <- tilskuere$tid + dhours(2)  
tidspunkt <- tilskuere$tid
tidspunkt[1] + hours(2)


# Filtrer relevante kolonner
flot_dmi_filtered <- flot_dmi %>%
  select(dato, tid, temp_mean_past1h, precip_dur_past1h, precip_past1h, wind_speed_past1h) %>%
  drop_na()

tilskuere <-  read_csv("tilskuere.csv")

level("tilskuere$tid")


# Kombiner dato og tid til en datetime-kolonne i flot_dmi_filtered
flot_dmi_filtered <- flot_dmi_filtered %>%
  mutate(
    timestamp = ymd_hms(paste(dato, tid))  # Kombiner dato og tid
  )

# Kombiner dato og tid til en datetime-kolonne i tilskuere
tilskuere <- tilskuere %>%
  mutate(
    kampstart = ymd_hms(paste(dato, tid))  # Kombiner dato og tid
  )


# Beregn gennemsnitsværdier for 3 timer før kampstart
result <- tilskuere %>%
  rowwise() %>%  # Behandler hver række (kamp) individuelt
  mutate(
    # Temperatur: Gennemsnitlig 3 timer før kampstart
    # Filtrerer 'flot_dmi_filtered' for rækker i tidsintervallet [kampstart - 3 timer, kampstart]
    # og beregner gennemsnittet af 'temp_mean_past1h'
    temp_mean_3hours = mean(
      flot_dmi_filtered$temp_mean_past1h[
        flot_dmi_filtered$timestamp >= kampstart - hours(3) &
          flot_dmi_filtered$timestamp < kampstart
      ],
      na.rm = TRUE  # Ignorerer NA-værdier
    ),
    # Nedbør: Gennemsnitlig 3 timer før kampstart
    # Beregner gennemsnittet af 'precip_past1h' for det samme tidsinterval
    precip_mean_3hours = mean(
      flot_dmi_filtered$precip_past1h[
        flot_dmi_filtered$timestamp >= kampstart - hours(3) &
          flot_dmi_filtered$timestamp < kampstart
      ],
      na.rm = TRUE
    ),
    # Nedbørens varighed: Gennemsnitlig 3 timer før kampstart
    # Beregner gennemsnittet af 'precip_dur_past1h' (antal minutter med nedbør) i tidsintervallet
    precip_dur_mean_3hours = mean(
      flot_dmi_filtered$precip_dur_past1h[
        flot_dmi_filtered$timestamp >= kampstart - hours(3) &
          flot_dmi_filtered$timestamp < kampstart
      ],
      na.rm = TRUE
    ),
    # Vindhastighed: Gennemsnitlig 3 timer før kampstart
    # Beregner gennemsnittet af 'wind_speed_past1h' i tidsintervallet
    wind_mean_3hours = mean(
      flot_dmi_filtered$wind_speed_past1h[
        flot_dmi_filtered$timestamp >= kampstart - hours(3) &
          flot_dmi_filtered$timestamp < kampstart
      ],
      na.rm = TRUE
    )
  ) %>%
  ungroup()  # Fjerner 'rowwise'-grupperingen for at gøre datasættet til et normalt tibble igen

# Udtræk dato fra datetime i begge datasæt (hvis nødvendigt)
result <- result %>%
  mutate(dato = as.Date(kampstart))  # Antag at kampstart er datetime

guld_clean <- guld_clean %>%
  mutate(dato = as.Date(dato))  # Hvis dato allerede er korrekt, er dette ikke nødvendigt

# Merge datasættene baseret på dato
merged_data <- result %>%
  left_join(guld_clean, by = "dato")

# Klargør datasættet (tidy process)
tidy_data <- merged_data %>%
  # Opdel 'kampstart' i 'Kampdato' og 'Kamptidspunkt'
  separate(kampstart, into = c("Kampdato", "Kamptidspunkt"), sep = " ") %>%
  # Fjern overflødige kolonner
  select(-dato, -tid, -år.x, -måned.x, -dag.x, -år.y, -måned.y, -dag.y, -hjemme, -modstander) %>%
  # Oversæt ugedag fra engelsk til dansk og tilføj Weekend/Hverdag
  mutate(
    ugedag = case_when(
      ugedag == "Sunday" ~ "Søndag",
      ugedag == "Monday" ~ "Mandag",
      ugedag == "Tuesday" ~ "Tirsdag",
      ugedag == "Wednesday" ~ "Onsdag",
      ugedag == "Thursday" ~ "Torsdag",
      ugedag == "Friday" ~ "Fredag",
      ugedag == "Saturday" ~ "Lørdag",
      TRUE ~ ugedag  # Beholder originalværdi, hvis ingen match
    ),
    Weekend = ifelse(ugedag %in% c("lørdag", "søndag"), "Weekend", "Hverdag")
  ) %>%
  # Flyt relevante kolonner (Weekend efter ugedag)
  select(Kampdato, Kamptidspunkt, ugedag, Weekend, everything()) %>%
  # Beregn afhentningsrelaterede procenter
  mutate(
    afhentningsgrad = (antal_afhentede / antal_bestilte) * 100,
    afhentede_max = (antal_afhentede / max_antal) * 100,
    bestilte_max = (antal_bestilte / max_antal) * 100
  )

# Beregn gennemsnitligt tilskuertal pr. modstander og klassificér dem i type A, B, C
modstander_tilskuere <- tidy_data %>%
  group_by(ude) %>%
  summarise(
    gennemsnit_tilskuere = mean(tilskuere, na.rm = TRUE)
  ) %>%
  arrange(desc(gennemsnit_tilskuere)) %>%
  mutate(
    type = case_when(
      row_number() <= 3 ~ "A",  # Top 3 hold er A
      row_number() <= 7 ~ "B",  # Næste 4 hold er B
      TRUE ~ "C"               # Resterende hold er C
    )
  )

# Flet modstanderklassifikation (type) tilbage i tidy_data
tidy_data <- tidy_data %>%
  left_join(modstander_tilskuere, by = "ude") %>%
  relocate(type, .after = ude)  # Flyt 'type' til efter 'ude'

# Ryd decimaler i relevante kolonner for overskuelighed
tidy_data <- tidy_data %>%
  mutate(
    afhentningsgrad = round(afhentningsgrad, 0),
    afhentede_max = round(afhentede_max, 0),
    bestilte_max = round(bestilte_max, 0),
    temp_mean_3hours = round(temp_mean_3hours, 1),
    precip_mean_3hours = round(precip_mean_3hours, 1),
    precip_dur_mean_3hours = round(precip_dur_mean_3hours, 1),
    wind_mean_3hours = round(wind_mean_3hours, 1)
  )

# Tilføj lokalopgør baseret på afstand
afstande <- data.frame(
  klub = c("FCM", "SIF", "RFC", "AGF", "AaB", "BIF", "FCK", "FCN", "ACH", "SJF", "VB", "OB", "HOB", "EFB", "LBK", "HIF", "FCV"),
  afstand_km = c(40, 50, 60, 80, 90, 200, 220, 240, 230, 150, 120, 130, 65, 175, 240, 150, 95)
)

tidy_data <- tidy_data %>%
  left_join(afstande, by = c("ude" = "klub")) %>%
  mutate(
    lokalt_opgoer = ifelse(afstand_km <= 85, 1, 0)
  ) %>%
  relocate(afstand_km, lokalt_opgoer, .after = ude)  # Flyt 'lokalt_opgoer' efter 'ude'

# #Omdøb kolonner til mere beskrivende navne
# tidy_data <- tidy_data %>%
#   rename(
#     Runde = runde,
#     Hjemmehold = hjemme,
#     Modstander = ude,  # Omnavn fra 'ude'
#     Tilskuertal = tilskuere,
#     Temperatur_3timer = temp_mean_3hours,
#     Nedbør_3timer = precip_mean_3hours,
#     Nedbørsvarighed_3timer = precip_dur_mean_3hours,
#     Vindhastighed_3timer = wind_mean_3hours,
#     Afhentede_Billetter = antal_afhentede,
#     Bestilte_Billetter = antal_bestilte,
#     Maks_Billetter = max_antal,
#     Afhentningsgrad = afhentningsgrad,
#     Afhentede_Maksandel = afhentede_max,
#     Bestilte_Maksandel = bestilte_max
#   )

# Gem det opdaterede datasæt i Excel
write_csv(tidy_data, "tidy_data.csv")

testing <- tidy_data |> 
  group_by(ude) |> 
  mutate(
    mean_afhentningsgrad = mean(afhentningsgrad),
    mean_bestilte = mean(antal_bestilte),
    mean_afhentede = mean(antal_afhentede),
    tilskuere = as.numeric(gsub("\\.", "", tilskuere)),
    mean_tilskuere = mean(tilskuere)
    ) |> 
  ungroup()

# Laver oversigt over gennensnitligt antal afhentede, bestilte, afhentningsgrad og tilskuere grupperet efter udehold. 
# n = antal observationer brugt til udregning
antal_ude_summary <- testing |> 
  group_by(ude) |> 
  summarize(
    n = n()
  )


