pacman::p_load(tidyverse, janitor, rvest, tidyr, lubridate, dplyr, readxl, tibble, dslabs, jsonlite, rjson)

# API: DMI
# Laver tom tibble til at indsætte dmi-data
samlet_dmi_flot <- tibble()

# Lave liste over datoer der skal hentes data fra.
vff <- read_rds("data/vff.rds")
alle_datoer <- as.character(vff$dato) # as.character() fordi ellers så bruger den mærkelige tal som dato

# Definere dele af request-url, der ikke ændres.
base_url <- "https://dmigw.govcloud.dk/v2/" # Definerer basen af url
info_url <- "metObs/collections/observation/items?" # Definerer yderligere hvor dataen skal hentes fra
station <- "stationId=06060" # Definerer hvilken station der skal hentes data fra
limit <- "&limit=300000" # Definerer hvad grænsen for antallet af rækker data der kan hentes pr. request
api_key <- "&api-key=8119aa28-31a1-4cec-9ab7-549fb14e0090" # Definerer API-nøgle

for (dato_x in alle_datoer) { # Laver et loop der starter med den første dato i "alle_datoer" og kører igennem hele listen
  
  # Definere hvilken dato der skal hentes data fra.
  start_dato <- paste0("&datetime=", dato_x, "T00:00:00Z")
  slut_dato <- paste0("/", dato_x, "T23:59:59Z")
  
  # Samler fulde URL
  full_url <- paste0(base_url, info_url, station, start_dato, slut_dato, limit, api_key)
  
  # Laver en get-efterspørgsel til API
  api_call <- httr::GET(full_url)
  
  # Tjekker statuskode og omdanner fra JSON til tibble
  if (api_call$status_code == 200) {
    api_char <- base::rawToChar(api_call$content)
    api_JSON <- jsonlite::fromJSON(api_char, flatten = TRUE)
    dmi_dato_tibble <- as_tibble(api_JSON$features)
    
    # Rydder op i data
    dmi_flot <- dmi_dato_tibble |> 
      mutate(
        dato = parse_date(substr(properties.observed, 1, 10)), # Udleder dato fra karakterene fra 1-10 i properties.observed
        tid = parse_time(substr(properties.observed, 12, 19)) # Udleder tidspunkt fra karakterene 12-19 i properties.observed
      ) |> 
      select(
        dato, tid, properties.parameterId, properties.value # Udvælger relevante kolonner
      ) |> 
      pivot_wider( # Da parametrene står som rækkeværdier, så omdannes de her til kolonneværdier
        names_from = properties.parameterId,
        values_from = properties.value
      )
    samlet_dmi_flot <- bind_rows(samlet_dmi_flot, dmi_flot) # Forbinder data fra enkelte datoer til samlet datasæt
  } else {  # Printer besked om hvis bestemte datoer ikke havde status-kode 200, og derfor ikke kunne hentes.
    warning(paste("Fejl ved", dato_x))
  }
}

dmi <- samlet_dmi_flot |> 
  mutate(
    tid = as.character(tid) # Omdanner tidskolonne til character, da det ellers ændres fra rds til sql
  )

# Laver rds-fil for at gemme det endelige dmi-datasæt
write_rds(dmi, "data/dmi.rds")
