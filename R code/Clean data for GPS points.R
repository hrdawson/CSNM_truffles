library(tidyverse)
library(tidylog)
# We have several formats going on so importing separately, harmonising, then joining
jan24 = read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points January '24.csv") |>
  mutate(Season = "Winter")

jun23 = read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points June '23.csv", na.strings=c("","NA")) |>
  mutate(Species = coalesce(Species, Molecular.species)) |>
  drop_na(Species) |>
  mutate(Season = "Spring")

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

# Bind them together
truffles = jan24 |>
  bind_rows(jun23, nov23, oct23.1, oct23.2, mar24) |>
  mutate(Point = as.character(Point)) |>
  bind_rows(sep23) |>
  select(Point, Species, Date, Season) |>
  # The GPS has changed how points are labeled
  mutate(Point = paste0("0", Point)) |>
  separate(Species, into = "Genus", remove = FALSE) |>
  mutate(Genus = case_when(
    Genus == "Phylum" ~ "Unknown",
    TRUE ~ Genus
  ))

write.csv(truffles, "clean_data/CSNM_GPSpoints_2023-24.csv")

# Make list of species
truffles.list = truffles |>
  select(Species) |>
  distinct()

truffles.new = truffles.list |>
  filter(str_detect(Species, " sp. ")) |>
  arrange(Species)

write.csv(truffles.new, "output/CSNMunknownSpp2023.csv")
