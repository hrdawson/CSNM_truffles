library(tidyverse)
library(tidylog)
# We have several formats going on so importing separately, harmonising, then joining
jan24 = read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points January '24.csv", na.strings=c("","NA")) |>
  mutate(Season = "Winter",
         Species = coalesce(Species, field.guess))

jun23 = read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points June '23.csv", na.strings=c("","NA")) |>
  mutate(Species = coalesce(Species, Molecular.species),
         Date = case_when(
           Site == "Surveyor" ~ "2023-06-16",
           Site == "Horseshoe Ranch" ~ "2023-06-17",
           Site == "Mariposa Meadows" ~ "2023-06-18"
         )) |>
  drop_na(Species) |>
  mutate(Season = "Summer")

nov23 = read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points November '23.csv", na.strings=c("","NA")) |>
  mutate(Species = case_when(
    !is.na(Molecular.species) ~ Molecular.species,
    TRUE ~ Species
  )) |>
  mutate(Season = "Winter")

oct23.2 = read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points October '23 Wk2.csv", na.strings=c("","NA")) |>
  rename(Molecular.species = Species) |>
  mutate(Species = case_when(
    !is.na(Molecular.species) ~ Molecular.species,
    TRUE ~ field.guess
  )) |>
  mutate(Season = "Autumn")

oct23.1 = read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points October '23.csv", na.strings=c("","NA")) |>
  rename(Molecular.species = Species) |>
  mutate(Species = case_when(
    !is.na(Molecular.species) ~ Molecular.species,
    TRUE ~ field.guess
  )) |>
  mutate(Season = "Autumn")

sep23 = read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points September '23.csv", na.strings=c("","NA")) |>
  rename(Molecular.species = Species) |>
  mutate(Species = case_when(
    !is.na(Molecular.species) ~ Molecular.species,
    TRUE ~ field.guess
  ))|>
  mutate(Point = as.character(Point)) |>
  mutate(Season = "Autumn")

mar24 = read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points March 2024.csv", na.strings=c("","NA")) |>
  rename(Molecular.species = Species) |>
  mutate(Species = case_when(
    !is.na(Molecular.species) ~ Molecular.species,
    TRUE ~ field.guess
  )) |>
  mutate(Season = "Spring")

may24 = read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points May 2024.csv", na.strings=c("","NA")) |>
  bind_rows(read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points May 2024 Wk2.csv",
                     na.strings=c("","NA")) |>
              mutate(Specimen = readr::parse_number(Point))) |>
  rename(Molecular.species = Species) |>
  mutate(Species = case_when(
    Point == "Pogie" & Date == "2024-05-26" ~ "Unknown",
    !is.na(Molecular.species) ~ Molecular.species,
    TRUE ~ field.guess
  )) |>
  mutate(Season = "Spring")

jul24 = read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points July 2024.csv", na.strings=c("","NA")) |>
  mutate(Point = as.character(Point),
         Season = "Summer",
         Date = "2024-07-26",
         Species = coalesce(Species, field.guess),
         Specimen = readr::parse_number(Specimen)
         )

# Bind them together
truffles = jan24 |>
  bind_rows(jun23, nov23, oct23.1, oct23.2, mar24) |>
  mutate(Point = as.character(Point)) |>
  bind_rows(sep23, may24, jul24) |>
  select(Point, Species, Date, Site, Season, Habitat, Specimen) |>
  # The GPS has changed how points are labeled
  mutate(Point = paste0("0", Point)) |>
  separate(Species, into = "Genus", remove = FALSE) |>
  mutate(Genus = case_when(
    Genus == "Phylum" ~ "Unknown",
    TRUE ~ Genus
  ))

write.csv(truffles, "clean_data/CSNM_GPSpoints_2023-24.csv", row.names = FALSE)

# Make list of species
truffles.list = truffles |>
  select(Species) |>
  distinct()

truffles.new = truffles.list |>
  filter(str_detect(Species, " sp. ")) |>
  arrange(Species)

write.csv(truffles.new, "output/CSNMunknownSpp2023.csv")

# ARCHIVE ####
## Import May 2024 data ----
trufflepoints_2324_may2 = read.csv("spatial/csnmmaygpxfiles/2024.05.05 waypoint key.csv") |>
  bind_rows(read.csv("spatial/csnmmaygpxfiles/2024.05.25 waypoint key.csv"),
            read.csv("spatial/csnmmaygpxfiles/2024.05.26 waypoint key.csv"),
            read.csv("spatial/csnmmaygpxfiles/2024.05.27 waypoint key.csv"),
            read.csv("spatial/csnmmaygpxfiles/2024.05.28 waypoint key.csv")) |>
  mutate(Season = "Spring") |>
  rename(Point = name) |>
  mutate(Genus = case_when(
    str_detect(Point, "Balma") ~ "Balsamia",
    str_detect(Point, "Cazi") ~ "Cazia",
    str_detect(Point, "(?i)Chiro") ~ "Choiromyces",
    str_detect(Point, "Chorio") ~ "Choiromyces",
    str_detect(Point, "Chrio") ~ "Choiromyces",
    str_detect(Point, "Cort") ~ "Phlegmacium",
    str_detect(Point, "Czia") ~ "Cazia",
    str_detect(Point, "Elapho") ~ "Elaphomyces",
    str_detect(Point, "Gaut") ~ "Gautieria",
    str_detect(Point, "Gena") ~ "Genea",
    str_detect(Point, "Genea") ~ "Genea",
    str_detect(Point, "Geopo") ~ "Geopora",
    str_detect(Point, "Glom") ~ "Glomerales",
    str_detect(Point, "Gnea") ~ "Genea",
    str_detect(Point, "Hmno") ~ "Hymenogaster",
    str_detect(Point, "Hydnotryopsis") ~ "Hydnotryopsis",
    str_detect(Point, "Hymeno") ~ "Hymenogaster",
    str_detect(Point, "Hymno") ~ "Hymenogaster",
    str_detect(Point, "Hyst") ~ "Hysterangium",
    str_detect(Point, "Hysrang") ~ "Hysterangium",
    str_detect(Point, "Lactarius") ~ "Lactarius",
    str_detect(Point, "Leucan") ~ "Leucangium",
    str_detect(Point, "(?i)Melan") ~ "Melanogaster",
    str_detect(Point, "(?i)Para") ~ "Paragalactinia",
    str_detect(Point, "(?i)Parg") ~ "Paragalactinia",
    str_detect(Point, "Pogie") ~ "Rhizopogon",
    str_detect(Point, "Russ") ~ "Russula",
    str_detect(Point, "Schlero") ~ NA,
    str_detect(Point, "Tcand") ~ "Tuber",
    str_detect(Point, "Tq") ~ "Tuber",
    str_detect(Point, "Trappea") ~ "Phallogaster",
    str_detect(Point, "Tuber") ~ "Tuber",
    str_detect(Point, "Xero") ~ "Xerocomellus",
    TRUE ~ NA
  )) |>
  drop_na(Genus) |>
  mutate(Date = lubridate::ymd(Date)) |>
  select(Genus, Point, Date, Season)

write.csv(trufflepoints_2324_may2, "output/2024.09.29_MayForay.csv")
