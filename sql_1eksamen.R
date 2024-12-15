pacman::p_load(DBI, RSQLite, readr, dplyr, lubridate)

vff <- readRDS("data/vff.rds")
dmi <- readRDS("data/dmi.rds")
superstats <- readRDS("data/superstats.rds")



connection <- dbConnect(SQLite(), "vff.db") 

dbWriteTable(connection, "vff", vff)
dbWriteTable(connection, "superstats", superstats)
dbWriteTable(connection, "dmi", dmi)

dbListTables(connection)

# Få rettet query når alle tabeller er klar
samlet_sql_query <- dbGetQuery(connection,
           "select vff.dato, vff.ugedag, vff.ude, vff.antal_spild, vff.mean_spild, vff.sidste_spild, vff.mean_afhentningsgrad,
           superstats.kampstart, superstats.runde, superstats.mean_tilskuere, superstats.sidste_tilskuer_antal,
           dmi.tid, dmi.temp_mean_past1h, dmi.precip_dur_past1h, dmi.precip_past1h, dmi.wind_speed_past1h
           from vff
           left join superstats ON vff.dato = superstats.dato
           left join dmi ON vff.dato = dmi.dato
           where dmi.temp_mean_past1h is not NULL")

# Eksporterer samlet tabel i rds-fil til videre forarbejdning
write_rds(samlet_sql_query, "samlet_sql_query.rds")

dbDisconnect(connection)

samlet_data <- as_tibble(samlet_sql_query) |> 
  mutate(
    dato = as.Date(dato, origin = "1970-01-01"),
    kampstart = hms(parse_time(kampstart)),
    tid = hms(parse_time(tid))
  )

overall_mean_tilskuere <- mean(superstats$tilskuere) # = 5401

samlet_data_flot <- samlet_data |> 
  filter(
    kampstart >= tid & kampstart - hours(6) <= tid
  ) |> 
  group_by(dato) |> 
  mutate(
    mean_temp_6h = round(mean(temp_mean_past1h), 1),
    mean_precip_6h = round(mean(precip_past1h), 1),
    mean_precip_dur_6h = round(mean(precip_dur_past1h), 1),
    mean_wind_speed_6h = round(mean(wind_speed_past1h), 1),
    dag_type = ifelse(ugedag %in% c("lørdag", "søndag"), "Weekend", "Hverdag"),
    mean_tilskuer_scoring = round((mean_tilskuere / overall_mean_tilskuere), 1)
  ) |> 
  slice_head() |> 
  ungroup() |> 
  select(dato, 
         kampstart,
         ugedag,
         dag_type,
         runde,
         ude,
         antal_spild,
         mean_spild,
         sidste_spild,
         mean_afhentningsgrad,
         mean_tilskuere,
         sidste_tilskuer_antal,
         mean_tilskuer_scoring,
         mean_temp_6h,
         mean_precip_6h,
         mean_precip_dur_6h,
         mean_wind_speed_6h
         ) # y-variable + 15 + tidspunktskategori, lokalopgør, afstand


# Tilføj lokalopgør baseret på afstand, og smider det ind i en dataframe. 
# Afstanden er ca. afrundet afstand taget fra google maps 
afstande <- data.frame(
  klub = c("FCM", "SIF", "RFC", "AGF", "AaB", "BIF", "FCK", "FCN", "ACH", "SJF", "VB", "OB", "HOB", "EFB", "LBK", "HIF", "FCV"),
  afstand_km = c(40, 50, 60, 80, 90, 200, 220, 240, 230, 150, 120, 130, 65, 175, 240, 150, 95)
)

samlet_data_afstand <- samlet_data_flot |> 
  left_join(afstande, by = c("ude" = "klub")) |> 
  mutate(
    lokalt_opgoer = ifelse(afstand_km <= 85, 1, 0)
  ) |> 
  relocate(afstand_km, lokalt_opgoer, .after = ude)  # Flyt 'lokalt_opgoer' efter 'ude'

# Tilføj en ny kolonne 'tidspunktskategori'
samlet_data_flot <- samlet_data_afstand |> 
  mutate(
    tidspunktskategori = case_when(
      as.numeric(substr(kampstart, 1, 2)) < 15 ~ "Middag",
      as.numeric(substr(kampstart, 1, 2)) < 18 ~ "Eftermiddag",
      TRUE ~ "Aften"
    )
  ) |> 
  relocate(
    tidspunktskategori, .after = dag_type
  )

samlet_data_flot <- samlet_data_flot |> 
  mutate(
  kampstart = parse_time(sprintf("%02d:%02d", hour(kampstart), minute(kampstart)))
  )

