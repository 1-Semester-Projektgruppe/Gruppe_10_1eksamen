pacman::p_load(tidyverse, janitor, rvest, tidyr, lubridate, dplyr, readxl, tibble, dslabs, jsonlite, rjson)

<<<<<<< Updated upstream
samlet_dmi <- tibble()

=======
# API: DMI
# Laver tom tibble til at indsætte dmi-data
>>>>>>> Stashed changes
samlet_dmi_flot <- tibble()

tilskuere <- read_csv("tilskuere.csv")

# API: DMI
# Lave liste over datoer der skal hentes data fra.
alle_datoer <- as.character(guld_clean$dato) # as.character() fordi ellers så bruger den mærkelige tal som dato

# Definere dele af request-url, der ikke ændres.
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