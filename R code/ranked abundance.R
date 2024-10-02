library(tidyverse)
library(lubridate)
library(stringr)

trufflepoints_2324_june = read.csv("spatial/csnmmaygpxfiles/2024.05.05 waypoint key.csv") |>
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
  select(Genus, Point, Date, Season)

write.csv(trufflepoints_2324_june, "output/2024.09.29_MayForay.csv")

trufflepoints_2324 = read.csv("clean_data/CSNM_GPSpoints_2023-24.csv") |>
  bind_rows(
    read.csv("spatial/waypoint keys/2023.06 waypoint key.csv") |> mutate(Point = as.character(Point),
                                                                         Season = "Spring"),
    read.csv("spatial/waypoint keys/2024.07.26 waypoint key.csv") |> mutate(Point = as.character(Point),
                                                                            Season = "Summer") |>
      separate(species, into = "Genus"),
            trufflepoints_2324_june) |>

  select(Species, Genus, Point, Date, Season)

trufflepoints_2223 = read.csv("spatial/waypoint keys/2022.10.30 waypoint key.csv") |>
  mutate(Season = "Autumn") |>
  bind_rows(read.csv("spatial/waypoint keys/2023.02.04 waypoint key.csv") |> mutate(Season = "Winter"),
            read.csv("spatial/waypoint keys/2023.05.13 waypoint key.csv") |> mutate(Season = "Spring"),
            read.csv("spatial/waypoint keys/2023.05.14 waypoint key.csv") |> mutate(Season = "Spring")) |>
  select(waypoint, species, Season) |>
  separate(species, into = "Genus", remove = FALSE) |>
  mutate(across(c(species, Genus), ~na_if(., ""))) |>
  drop_na(Genus) |>
  rename(Species = species, Point = waypoint) |>
  mutate(Point = as.character(Point))

trufflepoints = trufflepoints_2223 |>
  bind_rows(trufflepoints_2324) |>
  filter(Genus != "Tomst") |>
  filter(Genus != "")

trufflecounts = trufflepoints |>
  mutate(Genus = case_when(
    Genus == "Endogonaceae" ~ "Endogone",
    Genus == "Gymnomyces" ~ "Russula",
    Genus == "Zelleromyces" ~ "Lactarius",
    Genus == "Hymenogaster" & Date == "3/10/2024" ~ "Xerocomellus",
    TRUE ~ Genus
  )) |>
  filter(Genus != "Zygomyces") |>
  group_by(Genus) |>
  summarize(n = length(Point))

abundance = trufflecounts |>
  rename(ct = n) |>
  arrange(ct) |>
  mutate(category = case_when(
    ct >= 100 ~ "Abundant",
    ct < 100 & ct > 30 ~ "Common",
    ct <= 30 & ct >= 10 ~ "Uncommon",
    ct < 10 ~ "Rare"
  )) |>
  mutate(category = factor(category, levels = c("Abundant", "Common", "Uncommon", "Rare"))) |>
  filter(!Genus %in% c("Unknown", "Blue"))


ggplot(abundance, aes(x = reorder(Genus, -ct), y = ct, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ct), size = 5, vjust = -0.5) +
  scale_fill_manual(values = c("#014636", "#02818a", "#67a9cf", "#d0d1e6"))+
  labs(x = "", y = "Number of truffles found") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45,hjust = 1, face="bold"),
        text=element_text(size=20),
        panel.grid.minor = element_blank())

# ggsave("presentation/2024.06.05_TruffleFreq.png", width = 19, height = 10, units = "in")
ggsave("output/2024.09.28_TruffleFreq.png", width = 19, height = 10, units = "in")

# Seasonal abundance ----
trufflecounts.seasonal = trufflepoints |>
  mutate(Genus = case_when(
    Genus == "Endogonaceae" ~ "Endogone",
    Genus == "Gymnomyces" ~ "Russula",
    Genus == "Zelleromyces" ~ "Lactarius",
    Genus == "Hymenogaster" & Date == "3/10/2024" ~ "Xerocomellus",
    TRUE ~ Genus
  )) |>
  filter(Genus != "Zygomyces") |>
  group_by(Season, Genus) |>
  summarize(n = length(Point))

abundance.seasonal = trufflecounts.seasonal |>
  rename(ct = n) |>
  arrange(ct) |>
  mutate(category = case_when(
    ct >= 100 ~ "Abundant",
    ct < 100 & ct > 30 ~ "Common",
    ct <= 30 & ct >= 10 ~ "Uncommon",
    ct < 10 ~ "Rare"
  )) |>
  mutate(category = factor(category, levels = c("Abundant", "Common", "Uncommon", "Rare")),
         Season = factor(Season, levels = c("Spring", "Summer", "Autumn", "Winter"))) |>
  filter(!Genus %in% c("Unknown", "Blue"))

# Visualise with all spp present
ggplot(abundance.seasonal, aes(x = Genus, y = ct, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ct), size = 5, vjust = -0.5) +
  scale_fill_manual(values = c("#014636", "#02818a", "#67a9cf", "#d0d1e6"))+
  labs(x = "", y = "Number of truffles found") +
  ggh4x::facet_grid2(Season~., scale = "free_y", independent = "y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45,hjust = 1, face="bold"),
        text=element_text(size=20),
        panel.grid.minor = element_blank())

ggsave("output/2024.09.28_TruffleFreq_seasonal_alphabetical.png", width = 19, height = 10, units = "in")

# Visualise with independent X
ggplot(abundance.seasonal, aes(x = Genus, y = ct, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ct), size = 5, vjust = -0.5) +
  scale_fill_manual(values = c("#014636", "#02818a", "#67a9cf", "#d0d1e6"))+
  ggh4x::facet_grid2(Season~., scale = "free", independent = "all") +
  labs(x = "", y = "Number of truffles found") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45,hjust = 1, face="bold"),
        text=element_text(size=20),
        panel.grid.minor = element_blank())

ggsave("output/2024.09.28_TruffleFreq_seasonal_freeX.png", width = 10, height = 19, units = "in")

# Ranked abundance for NATS ----
nats = read.csv("raw_data/observations-445963_NATS.csv") |>
  separate(scientific_name, into = c("Genus", "Species"), remove = FALSE) |>
  drop_na(Species) |>
  group_by(scientific_name) |>
  summarize(ct = length(id)) |>
  filter(ct >= 30)

ggplot(nats, aes(x = reorder(scientific_name, -ct), y = ct)) +
  geom_bar(stat = "identity", fill = "#73ab00") +
  geom_text(aes(label = ct), size = 5, vjust = -0.5) +
  # scale_fill_manual(values = c("#014636", "#02818a", "#67a9cf", "#d0d1e6"))+
  labs(x = "", y = "Number of iNat observations") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=65,hjust = 1, face="bold"),
        text=element_text(size=20),
        panel.grid.minor = element_blank())

ggsave("visualizations/2024.06.04_NATSTruffleFreq.png", width = 10, height = 10, units = "in")
