library(tidyverse)
library(lubridate)
library(stringr)

trufflepoints_2324 = read.csv("clean_data/CSNM_GPSpoints_2023-24.csv") |>
  mutate(Date = lubridate::ymd(Date)) |>
  select(Species, Genus, Point, Date, Season, Habitat)

trufflepoints_2223 = read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points Oct '22.csv") |>
  mutate(Season = "Autumn") |>
  bind_rows(read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points Feb '23.csv") |>
              mutate(Season = "Winter"),
            read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points May '23.csv") |>
              mutate(Season = "Spring")) |>
  # # Add dates
  # mutate(Date = case_when(
  #   waypoint >= 296 & waypoint <= 309 ~ "2022-10-30",
  #   waypoint >= 310 & waypoint <= 367 ~ "2022-10-31",
  #   waypoint >= 368 & waypoint <= 373 ~ "2022-11-01",
  #   waypoint >= 396 & waypoint < 492 ~ "2023-02-04",
  #   waypoint == 492 & Season == "Winter" ~ "2023-02-04", #Note that 492 somehow was duplicated between Feb and May
  #   waypoint == 492 & Season == "Spring" ~ "2023-05-14",
  #   waypoint > 492 & waypoint <= 511 ~ "2023-05-14",
  #   TRUE ~ "2023-05-13"
  # )) |>
  # Add species
  rename(field.guess = species) |>
  mutate(species = case_when(
    original.ID %in% c("#2", "#33") ~ "Elaphomyces sp. CS2",
    original.ID %in% c("#3 Genea", "#4 Genea") ~ "Genea sp. CS3",
    original.ID == "Genea 45" ~ "Genea sp. CS45",
    original.ID == "Genea 46" ~ "Genea sp. CS46",
    original.ID %in% c("#5", "#11", "#15") ~ "Choiromyces sp. CS5",
    original.ID %in% c("#6", "#7", "#8") ~ "Russula cf. similaris",
    original.ID == "#9" ~ "Leucogaster odoratus",
    original.ID == "#10" ~ "Rhizopogon sp. CS10",
    original.ID %in% c("#12", "#13", "#14", "#20", "#24") ~ "Tuber aff. anniae",
    original.ID == "#16" ~ "Gautieria sp. CS16",
    original.ID == "#17" ~ "Gautieria sp. CS17",
    original.ID == "#18" ~ "Leucogaster sp. CS18",
    original.ID %in% c("#19", "#23") ~ "Tuber whetstonense",
    original.ID == "#21" ~ "Hysterangium cf. setchellii",
    original.ID == "#22" ~ "Tuber sp. CS22",
    original.ID %in% c("#25", "#27") ~ "Tuber beyerlei",
    original.ID == "#26" ~ "Choiromyces",
    original.ID %in% c("#30 Balsamia", "#37 Balsamia") ~ "Balsamia aff. filamentosa",
    original.ID == "#32 Balsamia" ~ "Balsamia cf. setchellii",
    original.ID == "#33 Elapho" ~ "Elaphomyces sp. CS2",
    original.ID == "#35 Tuber" ~ "Tuber candidum",
    original.ID == "#38 Balsamia" ~ "Hysterangium sp. CS38",
    original.ID == "#39 Genea" ~ "Genea arenaria",
    original.ID == "#40 Zellero" ~ "Lactarius sp. CS40",
    original.ID %in% c("Balsamia 42", "Balsamia 1", "Balsamia 43") ~ "Balsamia aff. latispora",
    original.ID == "Leucangium 44" ~ "Leucangium carthusianum",
    original.ID == "Elaphomyces 50" ~ "Elaphomyces sp. CS50",
    original.ID == "Hysterangium 49" ~ "Hysterangium sp. CS38",
    original.ID == "Hymenogaster 57" ~ "Hymenogaster raphanodorus",
    TRUE ~ field.guess
  )) |>
  relocate(species, .after = field.guess) |>
  select(waypoint, species, Season, Date, Habitat) |>
  separate(species, into = "Genus", remove = FALSE) |>
  mutate(across(c(species, Genus), ~na_if(., ""))) |>
  drop_na(Genus) |>
  rename(Species = species, Point = waypoint) |>
  mutate(Point = as.character(Point))

trufflepoints = trufflepoints_2223 |>
  mutate(Date = lubridate::ymd(Date)) |>
  bind_rows(trufflepoints_2324) |>
  filter(Genus != "Tomst") |>
  filter(Genus != "") |>
  mutate(Genus = case_when(
    Genus == "Endogonaceae" ~ "Endogone",
    Genus == "Zygomyces" ~ "Glomeraceae",
    Genus == "Glomerales" ~ "Glomeraceae",
    Genus == "Gymnomyces" ~ "Russula",
    Genus == "Genear" ~ "Genea",
    Genus == "Glomus" ~ "Glomeraceae",
    Genus == "Hydnotryopsis" ~ "Sarcosphaera",
    Genus == "Zelleromyces" ~ "Lactarius",
    Genus == "Phallales" ~ "Trappea",
    Genus == "Albatrellaceae" ~ "Leucophleps",
    Genus == "Geastrales" ~ "Geastrum",
    Genus == "Blue" ~ "Lepiota",
    Genus == "Hymenogaster" & Date == "3/10/2024" ~ "Xerocomellus",
    TRUE ~ Genus
  ),
  Species_original = Species,
  Species = coalesce(Species_original, Genus),
  Species_updated = case_when(
    Genus == "Agaricus" ~ "Agaricus cf. inapertus",
    Genus == "Alpova" ~ "Alpova trappei",
    Species == "Balsamia cf. latispora w/Microascus sp." ~ "Balsamia cf. latispora",
    # Species %in% c("Balsamia latispora", "Balsamia cf. latispora", "Balsamia aff. latispora") ~ "Balsamia latispora",
    Genus == "Cazia" ~ "Cazia flexiascus",
    Species == "Choiromyces alveolatus" ~ "Choiromyces", # Only CS61 can be confidently called cf alveolatus
    Species == "Choiromyces sp. CS52" ~ "Choiromyces sp. CS5",
    Genus == "Coniophora" ~ "Coniphora sp. CS143",
    Genus == "Cortinarius" ~ "Cortinarius pinguis",
    Species == "Elaphomyces sp" ~ "Elaphomyces",
    Species == "Gautieria sp" ~ "Gautieria",
    Species == "Gautieria cf. pterosperma" ~ "Gautieria sp. CS53",
    Species == "Geastrales sp. CS131" ~ "Geastrum sp. CS131",
    Species == "Geastrales sp. CS140" ~ "Geastrum sp. CS140",
    Species %in% c("Genea sp", "Genea harknessii", "Genea gardneri") ~ "Genea",
    Species == "Genear arenaria" ~ "Genea arenaria",
    Species %in% c("Glomerales", "Zygomyces") ~ "Glomeraceae",
    Species %in% c("Hydnotryopsis", "Hydnotryopsis cf. setchellii", "Hydnotryopsis setchellii") ~ "Sarcosphaera cf. setchellii",
    Species == "Hydnotryopsis sp. CS252" ~ "Sarcosphaera sp. CS252",
    Species == "Hysterangium sp" ~ "Hysterangium",
    Species %in% c("Hysterangium sp. CS75", "Hysterangium sp. CS134") ~ "Hysterangium sp. CS38",
    Species == "Zelleromyces" ~ "Lactarius",
    Genus == "Zelleromyces" ~ "Lactarius",
    Species == "Lactarius sp. CS78" ~ "Lactarius sp. CS40",
    Genus == "Leucangium" ~ "Leucangium carthusianum",
    Species == "Leucogaster odoratus" ~ "Leucogaster cf. odoratus",
    Species %in% c("Leucogaster sp. CS107", "Leucogaster citrinus") ~ "Leucogaster",
    Species %in% c("Leucogaster cf. rubescens", "Leucogaster sp. CS89") ~ "Leucogaster rubescens",
    Species == "Albatrellaceae sp. CS113" ~ "Leucophleps sp. CS113",
    Genus == "Lepiota" ~ "Lepiota viridigleba",
    Species %in% c("Melanogaster cf. ambiguus", "Melanogaster euryspermus") ~ "Melanogaster cf. euryspermus",
    Genus == "Paraglactinia" | Species == "Paragalactinia" ~ "Paragalactinia infossa",
    Genus == "Phallogaster" ~ "Phallogaster phillipsii",
    Genus == "Phlegmacium" ~ "Phlegmacium sublilacinum",
    Species == "Rhizopogon sp" ~ "Rhizopogon",
    Species %in% c("Gymnomyces sp", "Gymnomyces") ~ "Russula",
    Genus == "Schenella" | Species == "Schenella" ~ "Schenella pityophila",
    Genus == "Sedecula" ~ "Seducula pulvinata",
    Genus == "Trappea" ~ "Trappea sp. CS127",
    Species == "Tuber sp" ~ "Tuber",
    Species == "Tuber anniae" ~ "Tuber aff. anniae",
    Species == "Tuber cf. candidum" ~ "Tuber candidum",
    Species == "Tuber cf. whetstonense" ~ "Tuber whetstonense",
    Species == "Phylum unknown" ~ "Unknown",
    Genus == "Xerocomellus" ~ "Xerocomellus sp. CS129",
    TRUE ~ Species
  ),
  Species_updated = trimws(Species_updated)
  ) |>
  # Add in taxonomy
  left_join(read.csv("raw_data/2024.11.08_Genera.csv") |> rename(Genus = Taxon))

# write.csv(trufflepoints, "output/2024.11.08_AllTrufflePoints.csv")

# Make lists of genera and species -----
## Habitat ----
genera.list.habitat = trufflepoints |>
  select(Phylum, Order, Family, Genus, Habitat) |>
  distinct() |>
  mutate(n = "X") |>
  pivot_wider(names_from = Habitat, values_from = n, values_fill = "--") |>
  arrange(Phylum, Order, Family, Genus) |>
  relocate(Mixed, .after = Oak) |>
  rename(Taxon = Genus)
  # write.csv("output/2024.11.08_Genera.csv", row.names = FALSE)

species.list.habitat = trufflepoints |>
  filter(Species_updated != Genus) |>
  select(Phylum, Order, Family, Species_updated, Habitat) |>
  mutate(n = "X") |>
  distinct() |>
  pivot_wider(names_from = Habitat, values_from = n, values_fill = "--") |>
  arrange(Phylum, Order, Family, Species_updated) |>
  relocate(Mixed, .after = Oak) |>
  rename(Taxon = Species_updated)

table.lists.habitat = genera.list.habitat |>
  bind_rows(species.list.habitat)

## Season ----
genera.list.season = trufflepoints |>
  select(Phylum, Order, Family, Genus, Season) |>
  distinct() |>
  mutate(n = "X") |>
  pivot_wider(names_from = Season, values_from = n, values_fill = "--") |>
  arrange(Phylum, Order, Family, Genus) |>
  # relocate(Mixed, .after = Oak) |>
  rename(Taxon = Genus)

species.list.season = trufflepoints |>
  filter(Species_updated != Genus) |>
  select(Phylum, Order, Family, Species_updated, Season) |>
  mutate(n = "X") |>
  distinct() |>
  pivot_wider(names_from = Season, values_from = n, values_fill = "--") |>
  arrange(Phylum, Order, Family, Species_updated) |>
  # relocate(Mixed, .after = Oak) |>
  rename(Taxon = Species_updated)

table.lists.season = genera.list.season |>
  bind_rows(species.list.season)

table.lists = table.lists.habitat |>
  left_join(table.lists.season) |>
  relocate(Phylum, Order, Family, .after = last_col()) |>
  write.csv("output/2024.11.08_Table1.csv", row.names = FALSE)

spp.list = trufflepoints |>
  group_by(Genus, Species_updated, Season) |>
  summarize(n = length(Species_updated)) |>
  pivot_wider(names_from = Season, values_from = n, values_fill = 0) |>
  arrange(Species_updated) |>
  write.csv("output/2024.10.25_SpeciesCounts.csv", row.names = FALSE)

# Calculate abundance ----

trufflecounts = trufflepoints |>
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

# write.csv(abundance |> select(Genus) |> distinct(), "output/2024.10.05_Genera.csv")
viz.abundance =
ggplot(abundance, aes(x = reorder(Genus, -ct), y = ct, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ct), size = 5, vjust = -0.5) +
  scale_fill_manual(values = c("#014636", "#02818a", "#67a9cf", "#d0d1e6"))+
  # labs(x = "", y = "Number of truffles found") +
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45,hjust = 1, face="bold"),
        text=element_text(size=20),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# ggsave("presentation/2024.06.05_TruffleFreq.png", width = 19, height = 10, units = "in")
# ggsave("output/2024.09.28_TruffleFreq.png", width = 19, height = 10, units = "in")

# Seasonal abundance ----
trufflecounts.seasonal = trufflepoints |>
  filter(Genus != "Zygomyces") |>
  group_by(Season, Genus) |>
  summarize(n = length(Point))

abundance.seasonal = trufflecounts.seasonal |>
  rename(ct = n) |>
  arrange(ct) |>
  left_join(abundance |> select(Genus, category, ct) |> rename(total = ct) |> distinct()) |>
  # mutate(category = case_when(
  #   ct >= 100 ~ "Abundant",
  #   ct < 100 & ct > 30 ~ "Common",
  #   ct <= 30 & ct >= 10 ~ "Uncommon",
  #   ct < 10 ~ "Rare"
  # )) |>
  mutate(category = factor(category, levels = c("Abundant", "Common", "Uncommon", "Rare")),
         Season = factor(Season, levels = c("Spring", "Summer", "Autumn", "Winter"))) |>
  filter(!Genus %in% c("Unknown", "Blue"))

## Visualise with all spp present ----
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

# ggsave("output/2024.09.28_TruffleFreq_seasonal_alphabetical.png", width = 19, height = 10, units = "in")

## Visualise with independent X ----
plot.abundance = function(season){
  ggplot(abundance.seasonal |> filter(Season == season),
         aes(x = reorder(Genus, -ct), y = ct, fill = category)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = ct), size = 5, vjust = -0.5) +
    scale_fill_manual(values = c("#014636", "#02818a", "#67a9cf", "#d0d1e6"))+
    ggh4x::facet_grid2(~Season, scale = "free_x", independent = "x") +
    ylim(0,175) +
    labs(x = "", y = "Number of truffles found") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45,hjust = 1, face="bold"),
          text=element_text(size=20),
          panel.grid.minor = element_blank(),
          legend.position = "none")
}

library(patchwork)

spring = plot.abundance("Spring")
summer = plot.abundance("Summer")
autumn = plot.abundance("Autumn")
winter = plot.abundance("Winter")

(viz.abundance |
(spring + summer + autumn + winter + plot_layout(ncol = 2, axes = "collect"))) +
plot_layout(nrow = 2, heights = c(1, 3), axes = "collect")

# ggsave("output/2024.10.04_TruffleFreq_seasonal_freeX.png", width = 19, height = 10, units = "in")
# ggsave("output/2024.10.05_TruffleFreq_seasonal_freeX.png", width = 19, height = 10, units = "in")
ggsave("output/2024.11.08_TruffleFreq_all.png", width = 15, height = 15, units = "in")

## Stacked ----
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

ggplot(abundance.seasonal, aes(x = reorder(Genus, -total), y = ct, fill = Season)) +
  geom_bar(stat = "identity", position = "stack") +
  # geom_text(aes(label = ct), size = 5, vjust = -0.5) +
  scale_fill_manual(values = c("#0099D1FF", "#FEC289FF", "#F09000FF", "#9DC7E4FF"))+
  labs(x = "", y = "Number of truffles found") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45,hjust = 1, face="bold"),
        text=element_text(size=20),
        panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.8))

ggsave("output/2024.09.28_TruffleFreq_seasonal_stacked.png", width = 19, height = 10, units = "in")

## Calculate stats seasonally ----
### How many forays? ----
length(table(trufflepoints$Date))

forays = trufflepoints |>
  select(Season, Date) |>
  distinct()

table(forays$Season)

truffle.seasons = trufflepoints |>
  group_by(Season, Genus) |>
  summarize(n = length(Genus)) |>
  pivot_wider(names_from = Genus, values_from = n)

season.counts = trufflepoints |>
  select(Season, Species) |>
  distinct()

table(season.counts$Season)

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
