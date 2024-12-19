pacman::p_load(DBI, RSQLite, readr)

# Henter rds-filer fra vff, dmi og superstats
vff <- readRDS("data/vff.rds")
dmi <- readRDS("data/dmi.rds")
superstats <- readRDS("data/superstats.rds")

# Skaber forbindelse til en databasen "vff.db", eller opretter database med samme navn hvis den ikke findes.
connection <- dbConnect(SQLite(), "vff.db") 

# Importerer tabeller fra vff, superstats og dmi
dbWriteTable(connection, "vff", vff)
dbWriteTable(connection, "superstats", superstats)
dbWriteTable(connection, "dmi", dmi)

# Viser oversigt over hvilke tabeller der er i databasen (dmi, superstats, vff)
dbListTables(connection)

# Henter alle kolonner der skal bruges og joiner på dato-kolonnen af de forskellige datasæt
samlet_sql_query <- dbGetQuery(connection,
           "select vff.dato, vff.år, vff.måned, vff.dag, vff.årstid, vff.ugedag, vff.ude, vff.antal_afhentede, vff.mean_afhentede, vff.sidste_afhentede,
           superstats.kampstart, superstats.runde, superstats.mean_tilskuere, superstats.sidste_tilskuer_antal,
           dmi.tid, dmi.temp_mean_past1h, dmi.precip_dur_past1h, dmi.precip_past1h, dmi.wind_speed_past1h
           from vff
           left join superstats ON vff.dato = superstats.dato
           left join dmi ON vff.dato = dmi.dato
           where dmi.temp_mean_past1h is not NULL") # Da der kun medtages timebaserede vejr-variabler, så med denne linje
                                                    # medtages der kun rækker hvor der er data fra hver time, da 
                                                    # temp_mean_past1h er timebaseret.

# Fjerner forbindelse til databasen
dbDisconnect(connection)

# Laver rds-fil af sql-query, så vi ikke behøver at connecte til sql og lave query i fremtiden.
write_rds(samlet_sql_query, "data/samlet_sql_query.rds")
