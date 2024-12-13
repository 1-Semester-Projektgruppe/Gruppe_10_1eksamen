pacman::p_load(DBI, dbplyr, tidyverse, RSQLite)

vff <- readRDS("vff.rds")
dmi <- readRDS("dmi_færdig.rds")
superstats <- read.csv("tilskuere.csv")

connection <- dbConnect(SQLite(), "vff.db") 

dbWriteTable(connection, "vff", vff)
dbWriteTable(connection, "superstats", superstats)
dbWriteTable(connection, "dmi", dmi)

?dbWriteTable

dbListTables(connection)

samlet_vff <- dbGetQuery(connection,
           "select vff.*, superstats.*, dmi.*
           from guld
           left join superstats ON vff.dato = superstats.dato
           left join dmi ON vff.dato = dmi.dato")
