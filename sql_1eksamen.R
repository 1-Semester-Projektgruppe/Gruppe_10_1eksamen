pacman::p_load(DBI, RSQLite)

vff <- readRDS("vff.rds")
dmi <- readRDS("dmi_færdig.rds")
superstats <- read.csv("tilskuere.csv") # ret filnavn og format til "superstats.rds" og få det til at hænge sammen med superstats_1eksamen.R

connection <- dbConnect(SQLite(), "vff.db") 

dbWriteTable(connection, "vff", vff)
dbWriteTable(connection, "superstats", superstats)
dbWriteTable(connection, "dmi", dmi)

dbListTables(connection)

# Få rettet query når alle tabeller er klar
samlet_vff <- dbGetQuery(connection,
           "select vff.*, superstats.*, dmi.*
           from guld
           left join superstats ON vff.dato = superstats.dato
           left join dmi ON vff.dato = dmi.dato")

# Eksporterer samlet tabel i rds-fil til videre forarbejdning
write_rds(samlet_vff, "samlet_vff.rds")