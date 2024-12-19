pacman::p_load(tidyverse, janitor, rvest, tidyr, lubridate, dplyr, readxl, tibble, dslabs, jsonlite, rjson)

# API: DMI
# Laver tom tibble til at indsætte dmi-data
samlet_dmi_flot <- tibble()

# Lave liste over datoer der skal hentes data fra.
vff <- read_rds("data/vff.rds")
alle_datoer <- as.character(vff$dato) # as.character() fordi ellers så bruger den mærkelige tal som dato

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

dmi <- samlet_dmi_flot |> 
  mutate(
    tid = as.character(tid) 
  )

write_rds(dmi, "data/dmi.rds")

dmi <- read_rds("data/dmi.rds")
