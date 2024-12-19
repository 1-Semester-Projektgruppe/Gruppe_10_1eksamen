pacman::p_load(tidyverse, janitor, rvest, tidyr, lubridate, dplyr, readxl, tibble, dslabs, jsonlite, rjson)

# Webscraping: Superstats.dk

# Definerer basen af url'et til scraping
base_url <- "https://superstats.dk/program?season="

# Laver en tom tibble til at kunne indsætte indhentet data
superstats_combined <- tibble()

# Note om valg af sæsoner: 
# Vi kunne have valgt at tage data fra flere sæsoner kun til udregning af gennemsnitlige tilskuere, men vil mene dette er en dårlig
# idé, da VFF har markant flere tilskuere i gennemsnit de sidste par år, sammenlignet med f.eks. 2007/08. Her var gennemsnittet 4416 og i 
# 2023/24 er gennemsnittet 6371. Så hvis vi tager de tidligere år med, så vil det bare unødvendigt trække gennemsnittet af tilskuerne ned
# og derfor gøre det mere upræcist i fremtiden.

# Men der kan medtages data fra 2024/25 sæsonen, som kan bruges til at gøre faktorer baseret på antal tilskuere mere præcise.

# Her er webscraping kode til at få tilskuer-data med fra alle sæsoner med guldmenu-data og den nyeste sæson også:

# Definerer hvilke sæsoner der skal indsamles data fra
sæson <- c(2014, 2016, 2017, 2022, 2023, 2024, 2025)

# Definerer hvor mange runder der er hvert år, for kun at medtage det antal tabeller med fra Superstats
antal_runder <- c(33, 33, 36, 32, 32, 32, 17)

# For-loop der kører 7 gange, da der skal indhentes data fra 7 sæsoner
for (i in 1:7) {
  url <- paste0(base_url, sæson[i]) # sammensætter base-url og sæson-årstal for "i"
  html <- read_html(url) 
  table <- html |> 
    html_elements("table") |> 
    html_table(convert = FALSE) # convert = FALSE, for at undgå tilskuere automatisk håndteres som double og derfor 
                                # sletter 0 til sidst i tal senere
  bind_table <- bind_rows(table[1:antal_runder[i]]) # Sammensætter tabeller og da der bliver medtaget 
                                                    # flere tabeller end nødvendigt, så er der tilføjet "antal_runder[i]"
                                                    # så der kun bliver medtaget de tabeller med runde-data.
  bind_clean <- bind_table |> 
    separate(...3, into = c("hjemme", "ude")) |> # Deler kolonnen "...3" op i hjemme- og udehold
    filter(hjemme == "VFF") |> # Filtrerer så det kun er rækker hvor VFF er hjemmehold der medtages
    pivot_longer( # Da der er rundenumre der står som kolonnenavne, flyttes disse til 
      cols = ends_with("...2"),
      names_to = "runde",
      values_to = "dato",
      values_drop_na = TRUE
    ) |> 
    separate(dato, into = c("dag", "måned","kampstart"), sep = "/| ") |> # Opdeler dato i dag, måned og kampstart,
                                                                         # der opdeles efter enten "/" eller " " (mellemrum)
    mutate(
      tilskuere = as.numeric(gsub("\\.", "", ...5)), #gsub fjerner "." og skifter det ud med "", altså ingenting.
      runde = parse_number(runde), # Fjerner bogstaverne og medtager kun tallet i runde-kolonnen
      år = if_else(måned %in% c("02", "03", "04", "05"), sæson[i], # Hvis måneden er fra februar til maj, så er det slutningen
                                                                   # af sæsonen og derfor det højeste årstål i sæsonen
                   sæson[i] - 1), # Hvis det ikke er fra feb.-maj, så er det før nytår og dermed det tidligere årstal.
      dato = make_date(år, måned, dag) # laver en kolonne med dato i dato-format
    ) |> 
    select(dato, år, måned, dag, kampstart, runde, hjemme, ude, tilskuere) # Udvælger alle relevante kolonner
  superstats_combined <- bind_rows(superstats_combined, bind_clean) # Sætter data ind i den tibble hvor alt scraped data samles
} # Og der startes et nyt loop

superstats_flot <- superstats_combined |> 
  group_by(ude) |> # Grupperer efter hvilket udehold det er
  mutate(
    mean_tilskuere = round(mean(tilskuere), 0), # Udregner gennemsnit af tilskuere pr. hold
    sidste_tilskuer_antal = tilskuere[which.max(dato)] # Tager det sidste antal tilskuere der var til en kamp for hvert hold
  ) |> 
  ungroup()

# Laver rds-fil med alt data fra superstats
write_rds(superstats_flot, "data/superstats.rds")

