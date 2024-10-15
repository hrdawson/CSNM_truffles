library(stringr)

CSNMspp = read.csv("raw_data/CSNM Species List All.csv") |>
  select(Genus, X, epithet) |>
  distinct()

CSNMspp.2022 = read.csv("raw_data/2024.09.27_CSNM Species List All.csv") |>
  mutate(collNr = str_replace(sample_id, "CS", "")) |>
  filter(collNr < 52) |>
  select(Genus, X, epithet) |>
  distinct()

CSNMspp.2023 = read.csv("raw_data/2024.09.27_CSNM Species List All.csv") |>
  mutate(collNr = as.numeric(str_replace(sample_id, "CS", ""))) |>
  filter(collNr >= 52) |>
  select(Genus, X, epithet) |>
  arrange(Genus, X, epithet) |>
  distinct()

write.csv(CSNMspp.2023, "output/CSNM_speciesList_2023.csv")

CSNMspp.2023 |> filter(str_detect(epithet, "CS"))

only2023 = anti_join(CSNMspp.2023, CSNMspp.2022) |>
  add_column(new = 1)

anti_join(CSNMspp.2022, CSNMspp.2023)

CSNMspp.export = CSNMspp |>
  left_join(only2023) |>
  mutate(new = replace_na(new, 0)) |>
  arrange(Genus, X, epithet)
  write.csv("output/CSNM_speciesList.csv")

