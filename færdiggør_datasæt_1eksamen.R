pacman::p_load(readr, dplyr, lubridate)

# Henter samlet datasæt fra SQL-query
samlet_sql_query <- read_rds("data/samlet_sql_query.rds")

# 
samlet_data <- as_tibble(samlet_sql_query) |> # Ændre til tibble, for letter at se kolonneformater
  mutate(
    dato = as.Date(dato, origin = "1970-01-01"), # Da dato er skrevet ind som dage siden 1970-01-01, så formateres de rigtigt her.
    kampstart = hms(parse_time(kampstart)), # Ændre format på kampstart kolonnen, så der senere kan filtreres med -6 timer fra kampstart
    tid = hms(parse_time(tid)) # Ændre også format her så den kan sammenlignes med kampstart
  )

samlet_data_flot <- samlet_data |> 
  filter(
    kampstart >= tid & kampstart - hours(6) <= tid # Filter hvor der kun indgår tider fra 6 timer før kampstart og frem til kampstart.
  ) |> 
  group_by(dato) |> # Grupperer efter dato, så alle rækker med samme dato behandles sammen.
  mutate(
    # Udregner gennemsnitlig vejrforhold 6 timer før kampstart til kampstart, da der er filtreret og grupperet efter dette.
    mean_temp_6h = round(mean(temp_mean_past1h), 1), # Gennemsnitlig temperatur
    mean_precip_6h = round(mean(precip_past1h, na.rm = TRUE), 1), # Gennemsnitlig mængde nedbør
    mean_precip_dur_6h = round(mean(precip_dur_past1h, na.rm = TRUE), 1),# Gennemsnitlig antal minutter med nedbør
    mean_wind_speed_6h = round(mean(wind_speed_past1h), 1), # Gennemsnitligt vindhastighed
    dag_type = ifelse(ugedag %in% c("fredag","lørdag", "søndag"), "Weekend", "Hverdag") # Kolonne om det er weekend/hverdag
  ) |> 
  slice_head() |> # Da der lige nu er flere rækker fra hver time der er målt hver data, 
  # så fjerner vi her alle rækker bortset for 1 række for hver dato, da vi har grupperet efter hver dato.
  ungroup() |> # Her fjernes grupperingen af dato
  select( # Her udvælges de faktorer der skal bruges til videre forarbejdning og der fjernes dem som ikke længere skal bruges
    år,
    årstid,
    kampstart,
    dag,
    ugedag,
    dag_type,
    runde,
    ude,
    antal_afhentede,
    mean_afhentede,
    sidste_afhentede,
    mean_tilskuere,
    sidste_tilskuer_antal,
    mean_temp_6h,
    mean_precip_6h,
    mean_precip_dur_6h,
    mean_wind_speed_6h
  ) 


# Laver en data.frame til senere at kunne vurdere om det er lokalopgør baseret på afstand.
# Afstanden er ca. afrundet afstand taget fra google maps 
afstande <- data.frame(
  ude = c("FCM", "SIF", "RFC", "AGF", "AaB", "BIF", "FCK", "FCN", "ACH", "SJF", "VB", "OB", "HOB", "EFB", "LBK", "HIF", "FCV", "SDR"),
  afstand_km = c(40, 50, 60, 80, 90, 200, 220, 240, 230, 150, 120, 130, 65, 175, 240, 150, 95, 150)
)

samlet_data_afstand <- samlet_data_flot |> 
  left_join(afstande, by = c("ude" = "ude")) |> # Her joines "afstande" til samlet datasæt på kolonnen "ude" og "ude"
  mutate(
    lokalt_opgoer = ifelse(afstand_km <= 85, 1, 0) # Hvis afstanden er mindre end 85 km, så er det lokalt opgør (1),
                                                   # hvis der er mere end 85 km så er det ikke lokalt opgør (0)
  ) |> 
  relocate(afstand_km, lokalt_opgoer, .after = ude)  # Flytter afstand_km og lokalt_opgoer efter ude

# Tilføjer en ny kolonne 'tidspunktskategori baseret på kampstart.
samlet_data_flot <- samlet_data_afstand |> 
  mutate(
    tidspunktskategori = case_when(
      as.numeric(substr(kampstart, 1, 2)) < 15 ~ "Middag", # Før kl. 15 = Middag
      as.numeric(substr(kampstart, 1, 2)) < 18 ~ "Eftermiddag", # Mellem 15 og 18 = Eftermiddag
      TRUE ~ "Aften" # Og ellers er det aften
    )
  ) |> 
  relocate(
    tidspunktskategori, .after = dag_type # Flytter tidspunktkategori efter dag_type
  )

samlet_data_flot <- samlet_data_flot |> 
  mutate(
    mean_temp_6h_poly2 = poly(mean_temp_6h, degree = 2, raw = TRUE)[, 2]) # Laver kolonne med gennemsnitlig temperatur af
                                                                         # andengradspolynomium 

samlet_data_flot <- samlet_data_flot |> 
  mutate(
    regn_6h = mean_precip_6h * mean_precip_dur_6h, # Kombinerer regnmængde og tidslængde da der var høj korrelation
    måned_tid = case_when( # Laver ny kolonne hvor der er opdelt efter om det er start, midt eller slutningen af måneden
      dag <= 10 ~ "start", # start er de første 10 dage i måneden
      dag > 10 & dag <= 20 ~ "midt", # midt er dag 11-20
      dag > 20 ~ "slut" # slut er dag 21 og frem
    )) |> 
  relocate(
    måned_tid, .after = dag # Flytter måned_tid til efter dag.
  )

# Beregn gennemsnitligt tilskuertal pr. modstander og klassificér dem i type A, B, C
modstander_tilskuere <- samlet_data_flot |> 
  group_by(ude) |> 
  summarise(
    gennemsnit_tilskuere = mean(mean_tilskuere, na.rm = TRUE)
  ) |> 
  arrange(desc(gennemsnit_tilskuere)) %>%
  mutate(
    type = case_when(
      row_number() <= 3 ~ "A",  # Top 3 hold er A (BIF, FCM, FCK)
      row_number() <= 7 ~ "B",  # Næste 4 hold er B (AGF, HIF, AaB, RFC)
      TRUE ~ "C"               # Resterende hold er C (VB, SIF, LBK, FCN, OB, FCV, HOB, EFB, SDR, SJF, ACH)
    )
  )

# Joiner modstanderklassifikation (type) sammen med samlet datasæt på "ude"-kolonnen
samlet_data_flot <- samlet_data_flot |> 
  left_join(modstander_tilskuere, by = "ude") |> 
  relocate(type, .after = ude)  # Flyt 'type' til efter 'ude'

samlet_data_flot <- samlet_data_flot |> 
  mutate(
    kombi_afhentede = (mean_afhentede + sidste_afhentede) / 2, # mean_afhentede og sidste_afhentede er kombineret pga. høj korrelation
    kombi_tilskuere = (mean_tilskuere + sidste_tilskuer_antal) / 2 # disse er også kombineret pga. høj korrelation ifølge GVIF
  ) |> 
  select( # Fjerner faktorer brugt til udregning af kombi-faktorer + fjernet ugedag og mean_temp_6h pga. høj korrelation.
    antal_afhentede,
    år,
    årstid,
    måned_tid,
    dag_type,
    tidspunktskategori,
    runde,
    type,
    afstand_km,
    lokalt_opgoer,
    mean_temp_6h_poly2,
    regn_6h,
    mean_wind_speed_6h,
    kombi_afhentede,
    kombi_tilskuere
    )

# Laver rds-fil til at gemme datasættet til fremtidig brug
write_rds(samlet_data_flot, "data/dataset.rds")
