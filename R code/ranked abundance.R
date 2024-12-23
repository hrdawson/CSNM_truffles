library(tidyverse)
library(lubridate)
library(stringr)

trufflepoints_2324 = read.csv("clean_data/CSNM_GPSpoints_2023-24.csv") |>
  mutate(Date = lubridate::ymd(Date)) |>
  select(Species, Genus, Point, Date, Site, Season, Habitat)

trufflepoints_2223 = read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points Oct '22.csv") |>
  mutate(Season = "Autumn") |>
  bind_rows(read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points Feb '23.csv") |>
              mutate(Season = "Winter"),
            read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points May '23.csv") |>
              mutate(Season = "Spring")) |>
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
  select(waypoint, species, Season, Site, Date, Habitat) |>
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
  mutate(
    Site = case_when(
      Site == "Mixed oak-conifer" ~ "Site 1",
      Site == "PCT Pilot" ~ "PCT Pilot Rock",
      Site == "Tyler Rd" ~ "Taylor Rd",
      TRUE ~ Site
    ),
    Genus = case_when(
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
    Species == "Balsamia cf. latispora w/Microascus sp." ~ "Balsamia aff. latispora",
    Species %in% c("Balsamia sp. CS105", "Balsamia sp. CS106", "Balsamia sp. CS117") ~ "Balsamia sp. CS101",
    Species == "Balsamia cf. quercicola" ~ "Balsamia quercicola",
    Species %in% c("Balsamia latispora", "Balsamia cf. latispora", "Balsamia aff. latispora") ~ "Balsamia aff. latispora",
    Species == "Balsamia cf. setchellii" ~ "Balsamia setchellii",
    Genus == "Cazia" ~ "Cazia flexiascus",
    Species == "Choiromyces alveolatus" ~ "Choiromyces", # Only CS61 can be confidently called cf alveolatus
    Species == "Choiromyces sp. CS52" ~ "Choiromyces sp. CS5",
    Species == "Choiromyces cf. alveolatus" ~ "Choiromyces sp. CS61",
    Genus == "Coniophora" ~ "Coniphora sp. CS143",
    Genus == "Cortinarius" ~ "Cortinarius pinguis",
    Species == "Elaphomyces sp" ~ "Elaphomyces",
    Species == "Endogonaceae sp. CS155" ~ "Endogone sp. CS155",
    Species == "Gautieria sp" ~ "Gautieria",
    Species == "Gautieria cf. pterosperma" ~ "Gautieria sp. CS53",
    Species == "Geastrales sp. CS131" ~ "Geastrum sp. CS131",
    Species == "Geastrales sp. CS140" ~ "Geastrum sp. CS140",
    Species %in% c("Genea sp", "Genea harknessii", "Genea gardneri") ~ "Genea",
    Species %in% c("Genea sp. CS201") ~ "Genea sp. CS79",
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

### Species specific habitats ----
trufflepoints |>
  filter(Genus == "Genea") |>
  group_by(Habitat) |>
  summarize(n = length(Species_updated))

trufflepoints |>
  filter(Genus == "Balsamia") |>
  group_by(Habitat) |>
  summarize(n = length(Species_updated))

trufflepoints |>
  filter(Genus == "Tuber") |>
  group_by(Species_updated) |>
  summarize(n = length(Species_updated))

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
  arrange(Species_updated)
  write.csv("output/2024.10.25_SpeciesCounts.csv", row.names = FALSE)

trufflepoints |>
  filter(Species_updated != Genus) |>
  select(Species_updated) |>
  distinct() |>
  arrange(Species_updated)

genus.habitat.list = trufflepoints |>
  select(Genus, Habitat) |>
  distinct() |>
  arrange(Genus) |>
  mutate(n = 1) |>
  pivot_wider(names_from = Habitat, values_from = n) |>
  mutate(Habitat = case_when(
    is.na(Oak) & Conifer == 1 ~ "Conifer",
    is.na(Conifer) & Oak == 1 ~ "Oak",
    Oak == 1 & Conifer == 1 ~ "Mixed",
    TRUE ~ NA
  )) |>
  select(-c(Oak, Conifer, Mixed))

genus.habitat.list.seasonal = trufflepoints |>
  select(Genus, Habitat, Season) |>
  distinct() |>
  arrange(Genus) |>
  mutate(n = 1) |>
  pivot_wider(names_from = Habitat, values_from = n) |>
  mutate(Habitat = case_when(
    is.na(Oak) & Conifer == 1 ~ "Conifer",
    is.na(Conifer) & Oak == 1 ~ "Oak",
    Oak == 1 & Conifer == 1 ~ "Mixed",
    is.na(Oak) & is.na(Conifer) & Mixed == 1 ~ "Mixed",
    TRUE ~ NA
  )) |>
  select(-c(Oak, Conifer, Mixed))

genus.season.list = trufflepoints |>
  select(Genus, Season) |>
  distinct() |>
  arrange(Genus) |>
  mutate(n = Season) |>
  pivot_wider(names_from = Season, values_from = n, values_fill = " ") |>
  mutate(season.colour = str_c(Autumn, Winter, Spring, Summer, sep = " "),
         season.colour = str_trim(season.colour))

genus.season.count = trufflepoints |>
  select(Genus, Season) |>
  distinct() |>
  arrange(Genus) |>
  mutate(n = 1) |>
  pivot_wider(names_from = Season, values_from = n, values_fill = 0) |>
  mutate(season.count = Autumn + Winter + Spring + Summer)

table(genus.season.count$season.count)

# Count season richness ----
season.richness = trufflepoints |>
  filter(Species_updated != Genus) |>
  select(Species_updated, Season) |>
  distinct() |>
  mutate(n = 1) |>
  pivot_wider(names_from = Season, values_from = n, values_fill = 0)

sum(season.richness$Autumn)
sum(season.richness$Winter)
sum(season.richness$Spring)
sum(season.richness$Summer)

# Count foray productivity ----
foray.productivity = trufflepoints |>
  group_by(Site, Date) |>
  summarize(n = length(Point))

foray.richness = trufflepoints |>
  select(Species_updated, Site, Date) |>
  distinct() |>
  group_by(Site, Date) |>
  summarize(n = length(Species_updated))

# Count site visits ----
sites = trufflepoints |>
  select(Site, Date) |>
  distinct() |>
  group_by(Site) |>
  summarize(n = length(Date)) |>
  arrange(desc(n))

# write.csv(sites, "output/2024.11.22_SiteList.csv")

trufflepoints |>
  select(Date) |>
  distinct()

trufflepoints |>
  select(Date, Season) |>
  distinct() |>
  group_by(Season) |>
  summarize(n = length(Date))

# Check how many CS species there are ----
trufflepoints |>
  filter(Species_updated != Genus) |>
  select(Species_updated) |>
  distinct() |>
  filter(str_detect(Species_updated, "CS"))

# Count overlap between oak and conifer ----
habitat.overlap = trufflepoints |>
  # Remove truffles only ID'd to genus
  filter(Species_updated != Genus) |>
  # Extract only relevant info
  select(Species_updated, Habitat) |>
  # Make a list of distinct entries
  distinct() |>
  # Assign marker
  mutate(n = 1) |>
  # Pivot out
  pivot_wider(names_from = Habitat, values_from = n, values_fill = 0) |>
  # Remove mixed
  # select(-Mixed) |>
  # Find any that are in both
  mutate(Both = case_when(
    Oak == 1 & Conifer == 1 ~ "Both",
    Oak == 0 & Conifer == 0 & Mixed == 1 ~ "Mixed only",
    Oak == 1 & Conifer == 0 & Mixed == 1 ~ "Oak + Mixed",
    Oak == 0 & Conifer == 1 & Mixed == 1 ~ "Conifer + Mixed",
    Oak == 0 & Conifer == 1 & Mixed == 0 ~ "Conifer only",
    Oak == 1 & Conifer == 0 & Mixed == 0 ~ "Oak only")
  )

table(habitat.overlap$Both)

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
  filter(!Genus %in% c("Unknown")) |>
  left_join(genus.habitat.list) |>
  left_join(genus.season.count)
  # # Add colour
  # mutate(label.colour = case_when(
  #   Habitat == "Oak" ~ "#a63603",
  #   Habitat == "Conifer" ~ "#006d2c",
  #   Habitat == "Mixed" ~ "#4292c6"
  # ))

# write.csv(abundance |> select(Genus) |> distinct(), "output/2024.10.05_Genera.csv")

# viz.abundance =
ggplot(abundance, aes(x = reorder(Genus, -ct), y = ct, fill = Habitat)) +
  geom_bar(stat = "identity") +
  # geom_text(aes(label = ct), size = 5, vjust = -0.5) +
  geom_label(aes(label = ct, fill = Habitat, alpha = 0.8),
             size = 5, vjust = -0.5, label.padding = unit(0.05, "lines")) +
  scale_fill_manual(values = c("#006d2c", "#4292c6", "#a63603")) +
  # scale_fill_manual(values = c("#014636", "#02818a", "#67a9cf", "#d0d1e6"))+
  # labs(x = "", y = "Number of truffles found") +
  ylim(0,260) +
  labs(x = "", y = "") +
  theme_bw() +
  # https://stackoverflow.com/questions/61956199/ggplot-color-axis-labels-based-on-variable
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
  filter(!Genus %in% c("Unknown")) |>
  left_join(genus.habitat.list.seasonal)

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
plot.abundance = function(season, background.color){
  ggplot(abundance.seasonal |> filter(Season == season),
         aes(x = reorder(Genus, -ct), y = ct, fill = Habitat)) +
    geom_bar(stat = "identity") +
    # geom_text(aes(label = ct), size = 5, vjust = -0.5) +
    geom_label(aes(label = ct, fill = Habitat, alpha = 0.8), size = 5, vjust = -0.5, label.padding = unit(0.05, "lines")) +
    scale_fill_manual(values = c("#006d2c", "#4292c6", "#a63603")) +
    # scale_fill_manual(values = c("#014636", "#02818a", "#67a9cf", "#d0d1e6"))+
    ggh4x::facet_grid2(~Season, scale = "free_x", independent = "x") +
    ylim(0,175) +
    labs(x = "", y = "Number of truffles found") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45,hjust = 1, face="bold"),
          text=element_text(size=20),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          strip.background = element_rect(fill=background.color))
}

library(patchwork)

spring = plot.abundance("Spring", "#fefadc")
summer = plot.abundance("Summer", "#fee1dc")
autumn = plot.abundance("Autumn", "#f3e8fc")
winter = plot.abundance("Winter", "#e8f9fc")

(viz.abundance |
(autumn + winter + spring + summer + plot_layout(ncol = 2, axes = "collect"))) +
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
